// db_svd.cpp - OP-Aware Gram Operator for Fast Eigen CSC SVD
//
// Streams Arrow triplets directly into Eigen CSC format, then uses Spectra
// for eigendecomposition of the Gram matrix A @ A^T.
//
// Memory: O(nnz) for CSC matrix + O(n_rows * k) for eigenvectors
// Complexity: O(nnz * k * iterations) for Lanczos iteration

#include <Eigen/Core>
#include <Eigen/Sparse>
#include <Spectra/SymEigsSolver.h>
#include <atomic>
#include <chrono>
#include <cmath>
#include <numeric>
#include <thread>
#include <vector>

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif
#ifdef warning
#undef warning
#endif

#include <nanoarrow/r.h>

using namespace Spectra;

// OP-Aware Gram Operator for implicit centering/scaling inspired by BPCells
// Computes y = A_norm @ A_norm^T @ x where A_norm = diag(row_scale) * A * diag(col_scale) +
// row_offset
class OpAwareGramOp {
private:
  const Eigen::SparseMatrix<double, Eigen::ColMajor> &A_;
  const Eigen::VectorXd &row_offset_;
  const Eigen::VectorXd &row_scale_;
  const Eigen::VectorXd &col_scale_;
  int n_rows_, n_cols_, operator_threads_;
  mutable Eigen::VectorXd z_, scaled_z_;

public:
  OpAwareGramOp(const Eigen::SparseMatrix<double, Eigen::ColMajor> &A,
                const Eigen::VectorXd &row_offset,
                const Eigen::VectorXd &row_scale,
                const Eigen::VectorXd &col_scale,
                int operator_threads)
      : A_(A), row_offset_(row_offset),
        row_scale_(row_scale), col_scale_(col_scale),
        n_rows_(A.rows()), n_cols_(A.cols()), operator_threads_(operator_threads),
        z_(A.cols()), scaled_z_(A.cols()) {}

  int rows() const { return n_rows_; }
  int cols() const { return n_rows_; }

  void perform_op(const double *x_in, double *y_out) const {
    Eigen::Map<const Eigen::VectorXd> x(x_in, n_rows_);
    Eigen::Map<Eigen::VectorXd> y(y_out, n_rows_);

    const int n_workers = std::max(1, std::min(operator_threads_, n_cols_));
    if (n_workers > 1) {
      const Eigen::VectorXd scaled_x = row_scale_.cwiseProduct(x);
      const double offset_dot_x = row_offset_.dot(x);
      std::vector<Eigen::VectorXd> local_y(
          n_workers, Eigen::VectorXd::Zero(n_rows_));
      std::vector<double> local_z_sum(n_workers, 0.0);
      std::vector<std::thread> workers;
      workers.reserve(n_workers);

      for (int worker = 0; worker < n_workers; ++worker) {
        workers.emplace_back([&, worker]() {
          const int first_col = n_cols_ * worker / n_workers;
          const int last_col = n_cols_ * (worker + 1) / n_workers;
          Eigen::VectorXd &accumulator = local_y[worker];
          double z_sum = 0.0;

          for (int col = first_col; col < last_col; ++col) {
            double dot = offset_dot_x;
            for (Eigen::SparseMatrix<double, Eigen::ColMajor>::InnerIterator it(A_, col); it; ++it)
              dot += it.value() * scaled_x[it.row()];

            const double z = col_scale_[col] * dot;
            const double scaled_z = col_scale_[col] * z;
            z_sum += z;
            for (Eigen::SparseMatrix<double, Eigen::ColMajor>::InnerIterator it(A_, col); it; ++it)
              accumulator[it.row()] += it.value() * scaled_z;
          }
          local_z_sum[worker] = z_sum;
        });
      }
      for (auto &worker : workers)
        worker.join();

      y.setZero();
      double z_sum = 0.0;
      for (int worker = 0; worker < n_workers; ++worker) {
        y += local_y[worker];
        z_sum += local_z_sum[worker];
      }
      y = row_scale_.cwiseProduct(y);
      y += row_offset_ * z_sum;
      return;
    }

    // A_norm = diag(row_scale) * A * diag(col_scale) + row_offset
    // z = A_norm^T @ x = col_scale * (A^T @ (row_scale * x) + row_offset^T @ x)
    Eigen::VectorXd scaled_x = row_scale_.cwiseProduct(x);
    double offset_dot_x = row_offset_.dot(x);
    // Multiplying the transpose expression directly avoids materializing a
    // second full sparse matrix. For a column-major A, A^T * x can still be
    // evaluated efficiently one stored column at a time.
    z_.noalias() = A_.transpose() * scaled_x;
    z_ = col_scale_.cwiseProduct(z_);
    z_.array() += col_scale_.array() * offset_dot_x;

    // y = A_norm @ z = row_scale * (A @ (col_scale * z)) + row_offset * sum(z)
    scaled_z_ = col_scale_.cwiseProduct(z_);
    y.noalias() = A_ * scaled_z_;
    y = row_scale_.cwiseProduct(y);
    y += row_offset_ * z_.sum();
  }
};

// [[Rcpp::export(.compute_op_svd_arrow_cpp)]]
Rcpp::List compute_op_svd_arrow_cpp(SEXP stream_factory, int n_rows, int n_cols,
                                    Rcpp::NumericVector row_offset,
                                    Rcpp::NumericVector row_scale,
                                    Rcpp::NumericVector col_scale, int k,
                                    double tol, int maxit, int ncv,
                                    bool pca_scores, int score_threads,
                                    int operator_threads, double expected_nnz) {
  const auto started_at = std::chrono::steady_clock::now();
  // Stream Arrow triplets into Eigen
  std::vector<Eigen::Triplet<double>> triplets;
  if (std::isfinite(expected_nnz) && expected_nnz > 0 &&
      expected_nnz <= static_cast<double>(triplets.max_size()))
    triplets.reserve(static_cast<std::size_t>(expected_nnz));

  SEXP stream_sexp = Rcpp::Function(stream_factory)();
  if (TYPEOF(stream_sexp) != EXTPTRSXP)
    Rcpp::stop("Invalid stream factory.");

  auto *stream_ptr = (struct ArrowArrayStream *)R_ExternalPtrAddr(stream_sexp);
  if (!stream_ptr || !stream_ptr->get_next)
    Rcpp::stop("Invalid stream.");

  struct ArrowSchema schema;
  if (stream_ptr->get_schema(stream_ptr, &schema) == 0 && schema.release)
    schema.release(&schema);

  struct ArrowArray chunk;
  while (stream_ptr->get_next(stream_ptr, &chunk) == 0 && chunk.release) {
    if (chunk.n_children == 3) {
      const int32_t *i = (const int32_t *)chunk.children[0]->buffers[1];
      const int32_t *j = (const int32_t *)chunk.children[1]->buffers[1];
      const double *x = (const double *)chunk.children[2]->buffers[1];
      for (int64_t idx = 0; idx < chunk.length; ++idx)
        triplets.emplace_back(i[idx] - 1, j[idx] - 1, x[idx]);
    }
    chunk.release(&chunk);
  }
  if (stream_ptr->release)
    stream_ptr->release(stream_ptr);
  const auto stream_ready_at = std::chrono::steady_clock::now();

  // Build CSC matrix
  Eigen::SparseMatrix<double, Eigen::ColMajor> A(n_rows, n_cols);
  A.setFromTriplets(triplets.begin(), triplets.end());
  A.makeCompressed();
  triplets.clear();
  triplets.shrink_to_fit();
  const auto matrix_ready_at = std::chrono::steady_clock::now();

  // Create operator and solve
  Eigen::VectorXd r_off = Rcpp::as<Eigen::VectorXd>(row_offset);
  Eigen::VectorXd r_scl = Rcpp::as<Eigen::VectorXd>(row_scale);
  Eigen::VectorXd c_scl = Rcpp::as<Eigen::VectorXd>(col_scale);
  OpAwareGramOp op(A, r_off, r_scl, c_scl, operator_threads);

  SymEigsSolver<double, LARGEST_ALGE, OpAwareGramOp> eigs(&op, k, ncv);
  eigs.init();
  eigs.compute(maxit, tol);
  const auto eigensolver_done_at = std::chrono::steady_clock::now();

  if (eigs.info() != SUCCESSFUL)
    Rcpp::warning("Spectra computation was not fully successful.");

  // Extract and sort eigenvalues
  Eigen::VectorXd evalues = eigs.eigenvalues();
  Eigen::MatrixXd evecs = eigs.eigenvectors();

  std::vector<std::pair<double, int>> eval_idx;
  for (int i = 0; i < evalues.size(); ++i)
    eval_idx.emplace_back(evalues(i), i);
  std::sort(eval_idx.begin(), eval_idx.end(),
            std::greater<std::pair<double, int>>());

  Rcpp::NumericVector d(k);
  Rcpp::NumericMatrix u_vecs(n_rows, k);

  for (int i = 0; i < k && i < (int)eval_idx.size(); ++i) {
    d[i] = eval_idx[i].first > 0 ? std::sqrt(eval_idx[i].first) : 0;
    for (int r = 0; r < n_rows; ++r)
      u_vecs(r, i) = evecs(r, eval_idx[i].second);
  }

  // Compute V = A_norm^T @ U @ D^-1
  // A_norm^T = col_scale * A^T * row_scale + row_offset^T
  Rcpp::NumericMatrix v_vecs(n_cols, k);

  const double *u_data = REAL(u_vecs);
  double *v_data = REAL(v_vecs);
  const double *d_data = REAL(d);
  const auto project_column = [&](int col) {
    Eigen::Map<const Eigen::VectorXd> u_col(u_data + col * n_rows, n_rows);
    Eigen::Map<Eigen::VectorXd> v_out(v_data + col * n_cols, n_cols);
    if (d_data[col] < 1e-10) {
      v_out.setZero();
      return;
    }
    // v = col_scale * (A^T @ (row_scale * u) + row_offset^T @ u)
    Eigen::VectorXd scaled_u = r_scl.cwiseProduct(u_col);
    double offset_dot_u = r_off.dot(u_col);
    Eigen::VectorXd v_col = A.transpose() * scaled_u;
    v_col = c_scl.cwiseProduct(v_col);
    v_col.array() += c_scl.array() * offset_dot_u;
    // A^T U is already V D. Divide only for the SVD representation; the PCA
    // representation can return cell coordinates directly without creating a
    // second n_cols-by-k matrix in R.
    if (!pca_scores)
      v_col /= d_data[col];

    v_out = v_col;
  };

  const int n_score_workers = std::max(1, std::min(score_threads, k));
  if (n_score_workers == 1) {
    for (int col = 0; col < k; ++col)
      project_column(col);
  } else {
    std::atomic<int> next_col(0);
    std::vector<std::thread> workers;
    workers.reserve(n_score_workers);
    for (int worker = 0; worker < n_score_workers; ++worker) {
      workers.emplace_back([&]() {
        while (true) {
          const int col = next_col.fetch_add(1);
          if (col >= k)
            break;
          project_column(col);
        }
      });
    }
    for (auto &worker : workers)
      worker.join();
  }
  const auto scores_done_at = std::chrono::steady_clock::now();

  const auto seconds_between = [](const auto &start, const auto &end) {
    return std::chrono::duration<double>(end - start).count();
  };

  return Rcpp::List::create(Rcpp::Named("d") = d, Rcpp::Named("u") = u_vecs,
                            Rcpp::Named("v") = v_vecs,
                            Rcpp::Named("niter") = eigs.num_iterations(),
                            Rcpp::Named("nops") = eigs.num_operations(),
                            Rcpp::Named("score_threads") = n_score_workers,
                            Rcpp::Named("operator_threads") = std::max(1, std::min(operator_threads, n_cols)),
                            Rcpp::Named("matrix_build_sec") = seconds_between(started_at, matrix_ready_at),
                            Rcpp::Named("arrow_stream_sec") = seconds_between(started_at, stream_ready_at),
                            Rcpp::Named("csc_build_sec") = seconds_between(stream_ready_at, matrix_ready_at),
                            Rcpp::Named("eigensolver_sec") = seconds_between(matrix_ready_at, eigensolver_done_at),
                            Rcpp::Named("score_projection_sec") = seconds_between(eigensolver_done_at, scores_done_at));
}
