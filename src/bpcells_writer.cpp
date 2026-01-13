// BPCells File Writer - Streams Arrow COO to BPCells CSC format
// Memory: O(max_col_nnz), Complexity: O(nnz)

#include <Rcpp.h>
#include <cstdint>
#include <fstream>
#include <nanoarrow/r.h>
#include <string>
#include <vector>

template <typename T> void write_header(std::ofstream &file);
template <> void write_header<uint32_t>(std::ofstream &file) {
  uint32_t h[2] = {0x544e4955, 0x31763233};
  file.write((char *)h, 8);
}
template <> void write_header<uint64_t>(std::ofstream &file) {
  uint32_t h[2] = {0x544e4955, 0x31763436};
  file.write((char *)h, 8);
}
template <> void write_header<double>(std::ofstream &file) {
  uint32_t h[2] = {0x42554F44, 0x3176454C};
  file.write((char *)h, 8);
}

// [[Rcpp::export(.write_arrow_to_bpcells_cpp)]]
Rcpp::List write_arrow_to_bpcells_cpp(std::string output_dir, int n_rows,
                                      int n_cols, Rcpp::NumericVector col_scale,
                                      Rcpp::Function stream_factory) {
  std::ofstream idx_f(output_dir + "/index", std::ios::binary);
  std::ofstream val_f(output_dir + "/val", std::ios::binary);
  std::ofstream ptr_f(output_dir + "/idxptr", std::ios::binary);
  std::ofstream shp_f(output_dir + "/shape", std::ios::binary);
  std::ofstream ord_f(output_dir + "/storage_order");
  std::ofstream rn_f(output_dir + "/row_names");
  std::ofstream cn_f(output_dir + "/col_names");
  std::ofstream ver_f(output_dir + "/version");

  if (!idx_f || !val_f || !ptr_f)
    Rcpp::stop("Failed to open output files in: " + output_dir);

  write_header<uint32_t>(idx_f);
  write_header<double>(val_f);
  write_header<uint64_t>(ptr_f);
  write_header<uint32_t>(shp_f);

  ver_f << "unpacked-double-matrix-v2";
  ver_f.close();
  ord_f << "col";
  ord_f.close();
  rn_f.close();
  cn_f.close();

  SEXP stream_sexp = stream_factory();
  if (TYPEOF(stream_sexp) != EXTPTRSXP)
    Rcpp::stop("Invalid stream factory.");
  auto *stream_ptr = (struct ArrowArrayStream *)R_ExternalPtrAddr(stream_sexp);
  if (!stream_ptr || !stream_ptr->get_next)
    Rcpp::stop("Invalid stream.");

  struct ArrowSchema schema;
  if (stream_ptr->get_schema(stream_ptr, &schema) == 0 && schema.release)
    schema.release(&schema);

  int last_col = -1;
  uint64_t total_nnz = 0;
  std::vector<uint64_t> col_ptrs(n_cols + 1, 0);
  std::vector<uint32_t> rows;
  std::vector<double> vals;

  struct ArrowArray chunk;
  while (stream_ptr->get_next(stream_ptr, &chunk) == 0 && chunk.release) {
    if (chunk.n_children == 3) {
      const int32_t *i = (const int32_t *)chunk.children[0]->buffers[1];
      const int32_t *j = (const int32_t *)chunk.children[1]->buffers[1];
      const double *x = (const double *)chunk.children[2]->buffers[1];

      for (int64_t k = 0; k < chunk.length; ++k) {
        int row = i[k] - 1, col = j[k] - 1;
        if (row < 0 || row >= n_rows || col < 0 || col >= n_cols)
          continue;

        if (col != last_col) {
          if (!rows.empty()) {
            idx_f.write((char *)rows.data(), rows.size() * sizeof(uint32_t));
            val_f.write((char *)vals.data(), vals.size() * sizeof(double));
            total_nnz += rows.size();
            rows.clear();
            vals.clear();
          }
          for (int c = last_col + 1; c <= col; c++)
            col_ptrs[c] = total_nnz;
          last_col = col;
        }
        rows.push_back((uint32_t)row);
        vals.push_back(x[k] * col_scale[col]);
      }
    }
    chunk.release(&chunk);
  }

  if (!rows.empty()) {
    idx_f.write((char *)rows.data(), rows.size() * sizeof(uint32_t));
    val_f.write((char *)vals.data(), vals.size() * sizeof(double));
    total_nnz += rows.size();
  }

  for (int c = last_col + 1; c <= n_cols; c++)
    col_ptrs[c] = total_nnz;
  ptr_f.write((char *)col_ptrs.data(), col_ptrs.size() * sizeof(uint64_t));

  uint32_t shape[2] = {(uint32_t)n_rows, (uint32_t)n_cols};
  shp_f.write((char *)shape, 8);

  if (stream_ptr->release)
    stream_ptr->release(stream_ptr);
  idx_f.close();
  val_f.close();
  ptr_f.close();
  shp_f.close();

  return Rcpp::List::create(Rcpp::Named("path") = output_dir,
                            Rcpp::Named("nnz") = total_nnz,
                            Rcpp::Named("n_cols") = n_cols);
}
