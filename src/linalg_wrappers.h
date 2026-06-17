#ifndef CPP_LINALG_EXPTD_R_H
#define CPP_LINALG_EXPTD_R_H

#include <RcppEigen.h>
#include <cmath>


// validators 

inline void validate_data_mat(const Eigen::Ref<const Eigen::MatrixXd>& X,
                              const std::string& x_name = "X",
                              bool check_finite = false,
                              bool allow_zero_rows = false,
                              bool allow_zero_cols = false)
{
  if (!allow_zero_rows && X.rows() == 0)
    Rcpp::stop("%s must have at least one row", x_name.c_str())
    ;
  if (!allow_zero_cols && X.cols() == 0)
    Rcpp::stop("%s must have at least one column", x_name.c_str());
  
  if (check_finite){
    for (int j = 0; j < X.cols(); ++j) {
      for (int i = 0; i < X.rows(); ++i) {
        if (!std::isfinite(X(i, j)))
          Rcpp::stop("%s contains non-finite values", x_name.c_str());
      }
    }
  }
}


inline void validate_cov_mat(const Eigen::Ref<const Eigen::MatrixXd>& S,
                             const std::string& s_name = "S",
                             double sym_tol = 1e-8,
                             bool check_finite = false,
                             bool require_positive_diag = true)
{
  if (S.rows() == 0 || S.cols() == 0)
    Rcpp::stop("%s must be non-empty", s_name.c_str());
  
  if (S.rows() != S.cols())
    Rcpp::stop("%s must be square", s_name.c_str());
  
  if (check_finite){
    for (int j = 0; j < S.cols(); ++j) {
      for (int i = 0; i < S.rows(); ++i) {
        if (!std::isfinite(S(i, j)))
          Rcpp::stop("%s contains non-finite values", s_name.c_str());
      }
    }
  }
  
  const double scale = std::max(1.0, S.cwiseAbs().maxCoeff());
  if ((S - S.transpose()).cwiseAbs().maxCoeff() > sym_tol * scale)
    Rcpp::stop("%s must be symmetric within tolerance", s_name.c_str());
  
  if (require_positive_diag) {
    for (int j = 0; j < S.cols(); ++j) {
      if (S(j, j) <= 0.0)
        Rcpp::stop("%s must have strictly positive diagonal entries", s_name.c_str());
    }
  }
}

inline void validate_same_nrow(const Eigen::Ref<const Eigen::MatrixXd>& A,
                               const Eigen::Ref<const Eigen::MatrixXd>& B,
                               const std::string& a_name = "A",
                               const std::string& b_name = "B")
{
  if (A.rows() != B.rows())
    Rcpp::stop("%s and %s must have the same number of rows",
         a_name.c_str(), b_name.c_str());
}

inline void validate_same_ncol(const Eigen::Ref<const Eigen::MatrixXd>& A,
                               const Eigen::Ref<const Eigen::MatrixXd>& B,
                               const std::string& a_name = "A",
                               const std::string& b_name = "B")
{
  if (A.cols() != B.cols())
    Rcpp::stop("%s and %s must have the same number of columns",
         a_name.c_str(), b_name.c_str());
}

inline void validate_mult_dims(const Eigen::Ref<const Eigen::MatrixXd>& A,
                               const Eigen::Ref<const Eigen::MatrixXd>& B,
                               const std::string& a_name = "A",
                               const std::string& b_name = "B")
{
  if (A.cols() != B.rows())
    Rcpp::stop("%s and %s are not conformable for matrix multiplication",
         a_name.c_str(), b_name.c_str());
}

template<typename VectorType>
inline void validate_vec_len(const VectorType& vec,
                             int expected,
                             const std::string& v_name = "v")
{
  if (vec.size() != expected)
    Rcpp::stop("%s must have length %d", v_name.c_str(), expected);
}

#endif
