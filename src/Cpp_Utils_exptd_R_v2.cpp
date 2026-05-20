#include <cmath>
#include <RcppEigen.h>

#include "Cpp_LinAlg_exptd_R.h"  // get validators
#include "support_tall_v3.h"
// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;
using namespace std;


//====================================================
// Scaling and centering data helpers
//====================================================

// replication of R base::scale no deep copy
// [[Rcpp::export]]
Eigen::MatrixXd scaleC(const Eigen::Map<Eigen::MatrixXd>& A,
                       bool center = true,
                       bool scale = true) {
  try {
    if (A.rows() == 0 || A.cols() == 0)
      Rcpp::stop("A must have at least one row and one column");
    
    if (!A.allFinite())
      Rcpp::stop("A must contain only finite values");
    
    if (scale && A.rows() < 2)
      Rcpp::stop("at least two rows are needed to scale columns");
    
    Eigen::MatrixXd C = A; // copy is required
    Eigen::VectorXd m(A.cols());
    
    if (center) {
      m = C.colwise().mean();
      C.array().rowwise() -= m.array().transpose();
    }
    
    if (scale) {
      Eigen::VectorXd cm = A.colwise().mean();
      Eigen::MatrixXd Ac = A;
      Ac.array().rowwise() -= cm.array().transpose();
      m = (Ac.array().square().colwise().sum() / double(A.rows() - 1)).sqrt();
      
      if ((m.array() <= 0.0).any())
        Rcpp::stop("at least one column has zero scale");
      
      C.array().rowwise() /= m.array().transpose();
    }
    
    return C;
  }
  catch (std::exception& ex) {
    Rcpp::stop("scaleC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("scaleC failed: unknown C++ error");
  }
}




// centers and scales columns to L1 or L2 norm
// sig is a vector of +/- 1 for changing sign to columns
// [[Rcpp::export]]
Eigen::MatrixXd scaleColsC(const Eigen::Map<Eigen::MatrixXd>& A,
                           int normtype,
                           const Eigen::Map<Eigen::VectorXd>& sig) {
  try {
    if (A.rows() == 0 || A.cols() == 0)
      Rcpp::stop("A must have at least one row and one column");
    
    if (!A.allFinite() || !sig.allFinite())
      Rcpp::stop("A and sig must contain only finite values");
    
    if (A.cols() != sig.size())
      Rcpp::stop("matrix A must have the same number of columns as length of sig");
    
    if ((normtype != 1) && (normtype != 2))
      Rcpp::stop("normtype must be 1 or 2");
    
    for (int i = 0; i < sig.size(); i++) {
      if ((std::abs(sig(i) - 1.0) > 1e-8) &&
          (std::abs(sig(i) + 1.0) > 1e-8))
        Rcpp::stop("The elements of vector sig must be 1 or -1");
    }
    
    Eigen::MatrixXd M = A; // copy is required
    
    if (normtype == 2) {
      const Eigen::VectorXd l2 = A.colwise().norm();
      
      if ((l2.array() <= 0.0).any())
        Rcpp::stop("at least one column has zero L2 norm");
      
      M.array().rowwise() /= (l2.array() * sig.array()).transpose();
    } else {
      const Eigen::VectorXd l1 = A.array().abs().colwise().sum().matrix();
      
      if ((l1.array() <= 0.0).any())
        Rcpp::stop("at least one column has zero L1 norm");
      
      M.array().rowwise() /= (l1.array() * sig.array()).transpose();
    }
    
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("scaleColsC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("scaleColsC failed: unknown C++ error");
  }
}


//====================================================
// Deflation and explained-variance helpers
//====================================================

// computes exact vexp using loadings A and cov S
//exported wrapper for internal makeVexpSC
// [[Rcpp::export]]
List make_vexpSC(const Eigen::Map<Eigen::MatrixXd>& A,
                 const Eigen::Map<Eigen::MatrixXd>& S) {
  try {
    return makeVexpSC(A, S);
  }
  catch (std::exception& ex) {
    Rcpp::stop("make_vexpSC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("make_vexpSC failed: unknown C++ error");
  }
}
// transforms a variance matrix to correlation matrix
// [[Rcpp::export]]
Eigen::MatrixXd var2corC(const Eigen::Map<Eigen::MatrixXd>& S) {
  try {
    if (S.rows() != S.cols())
      Rcpp::stop("S must be square");
    
    if (!S.allFinite())
      Rcpp::stop("S must contain only finite values");
    
    Eigen::ArrayXd sd = S.diagonal().array();
    
    if ((sd <= 0.0).any())
      Rcpp::stop("S must have positive diagonal entries");
    
    sd = sd.sqrt();
    
    Eigen::ArrayXXd M = S.array();
    M.rowwise() /= sd.transpose();
    M.colwise() /= sd;
    
    if (!M.allFinite())
      Rcpp::stop("non-finite values produced");
    
    return M.matrix();
  }
  catch (std::exception& ex) {
    Rcpp::stop("var2corC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("var2corC failed: unknown C++ error");
  }
}


// Computes the correlation between sPCs (using loadings A and cov  S).
// Returns a ncomps x ncomps matrix.
// [[Rcpp::export]]
Eigen::MatrixXd makeCorCompC(const Eigen::Map<Eigen::MatrixXd>& A,
                             const Eigen::Map<Eigen::MatrixXd>& S,
                             int ncomps = 0) {
  try {
    if (A.rows() == 0 || A.cols() == 0)
      Rcpp::stop("A must have at least one row and one column");
    
    if (S.rows() != S.cols())
      Rcpp::stop("S must be square");
    
    if (A.rows() != S.rows())
      Rcpp::stop("A and S have incompatible dimensions");
    
    if (!A.allFinite() || !S.allFinite())
      Rcpp::stop("A and S must contain only finite values");
    
    if (ncomps == 0 || ncomps > A.cols())
      ncomps = A.cols();
    
    if (ncomps < 2)
      Rcpp::stop("ncomps must be > 1");
    
    if (ncomps < 0)
      Rcpp::stop("ncomps must be non-negative");
    
    const Eigen::MatrixXd Ad = A.leftCols(ncomps);
    
    Eigen::MatrixXd SA(S.rows(), ncomps);
    SA.noalias() = S * Ad;
    
    Eigen::MatrixXd C(ncomps, ncomps);
    C.noalias() = Ad.transpose() * SA;
    
    Eigen::ArrayXd v = C.diagonal().array();
    
    if ((!v.isFinite()).any())
      Rcpp::stop("component variances are non-finite");
    
    if ((v <= 0.0).any())
      Rcpp::stop("component variances must be positive");
    
    Eigen::ArrayXd s = v.sqrt();
    
    C.array().rowwise() /= s.transpose();
    C.array().colwise() /= s;
    
    if (!C.allFinite())
      Rcpp::stop("non-finite correlations produced");
    
    return C;
  }
  catch (std::exception& ex) {
    Rcpp::stop("makeCorCompC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeCorCompC failed: unknown C++ error");
  }
}

//returns matrix of correlations between components T
// [[Rcpp::export]]
Eigen::MatrixXd makeCorScoresC(const Eigen::Map<Eigen::MatrixXd>& T,
                               int ncomps = 0) {
  try {
    if (ncomps < 0)
      Rcpp::stop("ncomps must be non-negative");
    
    if ((ncomps > T.cols()) || (ncomps == 0))
      ncomps = T.cols();
    
    if (ncomps < 2)
      Rcpp::stop("makeCorScoresC: more than one score is needed");
    
    Eigen::MatrixXd C(ncomps, ncomps);
    C = T.leftCols(ncomps).transpose() * T.leftCols(ncomps);
    
    if ((!C.diagonal().array().isFinite()).any() ||
        (C.diagonal().array() <= 1e-14).any())
      Rcpp::stop("makeCorScoresC: at least one score column has zero or non-finite norm");
    
    Eigen::ArrayXd s = C.diagonal().array().sqrt();
    C.array().rowwise() /= s.transpose();
    C.array().colwise() /= s;
    
    return C;
  }
  catch (std::exception& ex) {
    Rcpp::stop("makeCorScoresC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeCorScoresC failed: unknown C++ error");
  }
}




