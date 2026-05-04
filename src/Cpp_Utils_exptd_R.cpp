#include <cmath>
#include <RcppEigen.h>

#include "Cpp_LinAlg_exptd_R.h"  // get validators

// [[Rcpp::depends(RcppEigen)]]
using namespace Rcpp;
using namespace std;


//====================================================
// Scaling and centering data helpers
//====================================================

// replication of R base::scale but columns devided by L2 norm, not sd
// [[Rcpp::export]]
Eigen::MatrixXd scaleC(const Eigen::Map<Eigen::MatrixXd>& A,
                       bool center = true, bool scale = true) {
  
  Eigen::MatrixXd C = A; // copy is required (we modify in place)
  Eigen::VectorXd m(A.cols());
  
  if (center) {
    m = C.colwise().mean();
    C.array().rowwise() -= m.array().transpose();
  }
  if (scale) {
    m = C.colwise().norm();
    C.array().rowwise() /= m.array().transpose();
  }
  return C;
}

// centers and scales columns to L1 or L2 norm
// sig is a vecor of +/- 1 for changing sign to columns
// [[Rcpp::export]]
Eigen::MatrixXd scaleColsC(const Eigen::Map<Eigen::MatrixXd>& A, int normtype, const Eigen::Map<Eigen::VectorXd>& sig) {
  
  if (A.cols() != sig.size())
    stop("matrix A must have the same number of columns as length of sig");
  
  if((normtype != 1) && (normtype != 2))
      stop("normtype must be 1 or 2");

  // sig must be +/- 1      
  for (int i = 0; i < sig.size(); i ++){
    if((abs(sig(i) - 1) > 1e-8))
      stop("The element of vector sig must be 1 or -1");
  }  
  
  // copy is required (returns a scaled matrix)
  Eigen::MatrixXd M = A; 
  
    if (normtype == 2) {
      const Eigen::VectorXd l2 = A.colwise().norm();
      M.array().rowwise() /= (l2.array() * sig.array()).transpose();
    } else { 
        const Eigen::VectorXd l1 = A.array().abs().colwise().sum().matrix();
        M.array().rowwise() /= (l1.array() * sig.array()).transpose();
      }

  return M;
}

//====================================================
// Deflation and explained-variance helpers
//====================================================


// computes exaxt vaexp using oadings A and cov S
// [[Rcpp::export]]
List make_vexpSC(const Eigen::Map<Eigen::MatrixXd>& A,
               const Eigen::Map<Eigen::MatrixXd>& S) {
  // Same as makeVexpC, but kept as a separate entry point for API compatibility.

  const int d = A.cols();
  Eigen::VectorXd vexp(d);
  Eigen::VectorXd cvexp(d);

  Eigen::MatrixXd M(S.rows(), d);
  M.noalias() = S * A;

  const double denom0 = A.col(0).dot(M.col(0));
  if (!std::isfinite(denom0) || denom0 == 0.0) {
    Rcpp::stop("makeVexpSC: non-finite or zero denominator for first component");
  }

  vexp(0) = M.col(0).squaredNorm() / denom0;
  cvexp(0) = vexp(0);

  for (int i = 1; i < d; ++i) {
    const int k = i + 1;

    const Eigen::MatrixXd Mi = M.leftCols(k);
    const Eigen::MatrixXd Ai = A.leftCols(k);

    Eigen::MatrixXd G(k, k);
    G.noalias() = Ai.transpose() * Mi;

    const Eigen::MatrixXd Ginv = G.ldlt().solve(Eigen::MatrixXd::Identity(k, k));

    Eigen::MatrixXd H(k, k);
    H.noalias() = Mi.transpose() * Mi;

    cvexp(i) = (Ginv * H).trace();
    vexp(i) = cvexp(i) - cvexp(i - 1);
  }

  return List::create(Named("vexp") = vexp, Named("cvexp") = cvexp);
}



// transforms a variance matrix to correlation matrix
// [[Rcpp::export]]
Eigen::MatrixXd var2corC(Eigen::Map<Eigen::MatrixXd > S){
  // turns a symmetric matrix into a correlation matrix by divid by sqrt diag
  Eigen::ArrayXXd M(S);
  Eigen::ArrayXd sd = S.diagonal().array().sqrt();
  M.rowwise() /= sd.transpose() ;
  M.transposeInPlace();
  M.rowwise() /= sd.transpose() ;
  return(M);
}


// Computes the correlation between sPCs (using loadings A and cov  S).
// Returns a d x d matrix.
// [[Rcpp::export]] 
Eigen::MatrixXd makeCorCompC(const Eigen::Map<Eigen::MatrixXd>& A,
                             const Eigen::Map<Eigen::MatrixXd>& S,
                             int d = 0) {
  if ((d > A.cols()) || (d == 0)) {
    d = A.cols();
  }
  if (d < 2) {
    Rcpp::stop("makeCorComp: d must be > 1");
  }

  const Eigen::MatrixXd Ad = A.leftCols(d);

  Eigen::MatrixXd SA(S.rows(), d);
  SA.noalias() = S * Ad;

  Eigen::MatrixXd C(d, d);
  C.noalias() = Ad.transpose() * SA;

  const Eigen::VectorXd s = C.diagonal().array().sqrt().matrix();

  C.array().rowwise() /= s.array().transpose();
  C.array().colwise() /= s.array();

  return C;
}

//returns matrix of correlations between components T
// [[Rcpp::export]]
Eigen::MatrixXd makeCorScoresC(const Eigen::Map<Eigen::MatrixXd>&  T,  
                               int d = 0){
  if ((d > T.cols()) || (d == 0))
    d = T.cols(); 
  if (d < 2)
    Rcpp::stop("makeCorScoresC: more than one score is needed");
  Eigen::MatrixXd C(d, d);
  C = T.leftCols(d).transpose() * T.leftCols(d);
  
  if (C.diagonal().minCoeff() <= 1e-14)
    Rcpp::stop("makeCorScoresC: at least one score column has zero norm");
  
  Eigen::VectorXd s(C.diagonal().array().sqrt());
  C.array().rowwise() /= s.array().transpose();
  C.array().colwise() /= s.array();
  return C;
}



