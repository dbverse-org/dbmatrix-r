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
#include <numeric>
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
  Eigen::SparseMatrix<double, Eigen::RowMajor> At_;
  const Eigen::VectorXd &row_offset_;
  const Eigen::VectorXd &row_scale_;
  const Eigen::VectorXd &col_scale_;
  int n_rows_, n_cols_;
  mutable Eigen::VectorXd z_, scaled_z_;

public:
  OpAwareGramOp(const Eigen::SparseMatrix<double, Eigen::ColMajor> &A,
                const Eigen::VectorXd &row_offset,
                const Eigen::VectorXd &row_scale,
                const Eigen::VectorXd &col_scale)
      : A_(A), At_(A.transpose()), row_offset_(row_offset),
        row_scale_(row_scale), col_scale_(col_scale),
        n_rows_(A.rows()), n_cols_(A.cols()),
        z_(A.cols()), scaled_z_(A.cols()) {}

  int rows() const { return n_rows_; }
  int cols() const { return n_rows_; }

  void perform_op(const double *x_in, double *y_out) const {
    Eigen::Map<const Eigen::VectorXd> x(x_in, n_rows_);
    Eigen::Map<Eigen::VectorXd> y(y_out, n_rows_);

    // A_norm = diag(row_scale) * A * diag(col_scale) + row_offset
    // z = A_norm^T @ x = col_scale * (A^T @ (row_scale * x) + row_offset^T @ x)
    Eigen::VectorXd scaled_x = row_scale_.cwiseProduct(x);
    double offset_dot_x = row_offset_.dot(x);
    z_.noalias() = At_ * scaled_x;
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
                                    Rcpp::NumericVector col_scale, int k) {
  // Stream Arrow triplets into Eigen
  std::vector<Eigen::Triplet<double>> triplets;

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

  // Build CSC matrix
  Eigen::SparseMatrix<double, Eigen::ColMajor> A(n_rows, n_cols);
  A.setFromTriplets(triplets.begin(), triplets.end());
  A.makeCompressed();
  triplets.clear();
  triplets.shrink_to_fit();

  // Create operator and solve
  Eigen::VectorXd r_off = Rcpp::as<Eigen::VectorXd>(row_offset);
  Eigen::VectorXd r_scl = Rcpp::as<Eigen::VectorXd>(row_scale);
  Eigen::VectorXd c_scl = Rcpp::as<Eigen::VectorXd>(col_scale);
  OpAwareGramOp op(A, r_off, r_scl, c_scl);

  int ncv = std::min(std::max(4 * k, 40), n_rows);
  SymEigsSolver<double, LARGEST_ALGE, OpAwareGramOp> eigs(&op, k, ncv);
  eigs.init();
  eigs.compute(10000, 1e-12);

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
  Eigen::SparseMatrix<double, Eigen::RowMajor> At = A.transpose();
  Rcpp::NumericMatrix v_vecs(n_cols, k);

  for (int col = 0; col < k; ++col) {
    if (d[col] < 1e-10)
      continue;

    Eigen::VectorXd u_col(n_rows);
    for (int r = 0; r < n_rows; ++r)
      u_col[r] = u_vecs(r, col);

    // v = col_scale * (A^T @ (row_scale * u) + row_offset^T @ u)
    Eigen::VectorXd scaled_u = r_scl.cwiseProduct(u_col);
    double offset_dot_u = r_off.dot(u_col);
    Eigen::VectorXd v_col = At * scaled_u;
    v_col = c_scl.cwiseProduct(v_col);
    v_col.array() += c_scl.array() * offset_dot_u;
    v_col /= d[col];

    for (int j = 0; j < n_cols; ++j)
      v_vecs(j, col) = v_col[j];
  }

  return Rcpp::List::create(Rcpp::Named("d") = d, Rcpp::Named("u") = u_vecs,
                            Rcpp::Named("v") = v_vecs,
                            Rcpp::Named("niter") = eigs.num_iterations(),
                            Rcpp::Named("nops") = eigs.num_operations());
}
