#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

#include "Cpp_LinAlg_exptd_R.h"  // get validators

// Linear-algebra helpers (no-copy interfaces)

using namespace Rcpp;
using namespace std;


using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::Map;
using Eigen::SelfAdjointEigenSolver;
using Eigen::EigenvaluesOnly;
using Eigen::GeneralizedSelfAdjointEigenSolver;
//using Eigen::llt.solve;
using Eigen::LLT;

// matrix and vector multiplications
// naming convention
// u, v, are vectors
// a, b, d are matrices
// t means transpose

// [[Rcpp::export]]
Eigen::MatrixXd aatC(const Eigen::Map<Eigen::MatrixXd>& A) {
  const int p = A.rows();
  MatrixXd M(p, p);
  M.noalias() = A * A.transpose();
  return M;
}

// [[Rcpp::export]]
Eigen::MatrixXd ataC(const Eigen::Map<Eigen::MatrixXd>& A) {
  const int p = A.cols();
  MatrixXd M(p, p);
//  M.noalias() = A.transpose() * A;
  M.noalias() = A.transpose() * A;
  return M;
}

// [[Rcpp::export]]
Eigen::MatrixXd atbC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::MatrixXd>& B) {
  const int p = A.cols();
  const int q = B.cols();
  MatrixXd M(p, q);
  M.noalias() = A.transpose() * B;
  return M;
}

// [[Rcpp::export]]
Eigen::MatrixXd abC(const Eigen::Map<Eigen::MatrixXd>& A,
                    const Eigen::Map<Eigen::MatrixXd>& B) {
  const int p = A.rows();
  const int q = B.cols();
  if (A.cols() != B.rows())
    stop("abC: A and B must be compatible for multiplication A * B");
  MatrixXd M(p, q);
  M.noalias() = A * B;
  return M;
}

// [[Rcpp::export]]
Eigen::MatrixXd abtC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::MatrixXd>& B) {
  const int p = A.rows();
  const int q = B.rows();
  if (A.cols() != B.cols())
    stop("abtC: A and B must have the same number of columns");
  MatrixXd M(p, q);
  M.noalias() = A * B.transpose();
  return M;
}

// [[Rcpp::export]]
Eigen::MatrixXd atdaC(const Eigen::Map<Eigen::MatrixXd>& A,
                      const Eigen::Map<Eigen::MatrixXd>& D) {
  const int p = A.cols();
  MatrixXd M(p, p);
  M.noalias() = A.transpose() * D * A;
  return M;
}

// [[Rcpp::export]]
Eigen::MatrixXd atdbC(const Eigen::Map<Eigen::MatrixXd>& A,
                      const Eigen::Map<Eigen::MatrixXd>& D,
                      const Eigen::Map<Eigen::MatrixXd>& B) {
  const int p = A.cols();
  const int q = B.cols();
  MatrixXd M(p, q);
  M.noalias() = A.transpose() * D * B;
  return M;
}

// [[Rcpp::export]]
Eigen::VectorXd avC(const Eigen::Map<Eigen::MatrixXd>& A,
                    const Eigen::Map<Eigen::VectorXd>& v) {
  return A * v;
}

// [[Rcpp::export]]
Eigen::VectorXd atvC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::VectorXd>& v) {
  // returns (v' A)' as a column vector
  return A.transpose() * v;
}
// [[Rcpp::export]]
Eigen::VectorXd vtaC(
    const Eigen::Map<Eigen::VectorXd>& v,
    const Eigen::Map<Eigen::MatrixXd>& A) {
    return A.transpose() * v;
}

// [[Rcpp::export]]
double vtauC(const Eigen::Map<Eigen::VectorXd>& v,
             const Eigen::Map<Eigen::MatrixXd>& A,
             const Eigen::Map<Eigen::VectorXd>& u) {
  return (v.dot(A * u));
}

// [[Rcpp::export]]
double vtvC(const Eigen::Map<Eigen::VectorXd>& v) {
  return v.squaredNorm();
}

// [[Rcpp::export]]
double vtuC(const Eigen::Map<Eigen::VectorXd>& v,
            const Eigen::Map<Eigen::VectorXd>& u) {
  return v.dot(u);
}


// [[Rcpp::export]]
double traceC(const Eigen::Map<Eigen::MatrixXd>& S) {
  return S.trace();
}


//======================================
//        helpers for lsspca
//======================================

// [[Rcpp::export]]
Rcpp::List EigenC(const Eigen::Map<Eigen::MatrixXd>& S) {
  if (S.cols() != S.rows())
    stop("S must be square");
  
  if ((S - S.transpose()).norm() > 1e-6 * S.norm())
    stop("S must be symmetric");  
  SelfAdjointEigenSolver<MatrixXd> es(S);
  
  return List::create(
    Named("vec") = MatrixXd(es.eigenvectors()).rowwise().reverse(),
    Named("val") = VectorXd(es.eigenvalues()).reverse()
  );
}

// cange name to comp_eigvalC

// [[Rcpp::export]]
Eigen::VectorXd compute_eigenvaluesC(const Eigen::Map<Eigen::MatrixXd>& S) {
  if (S.cols() != S.rows())
    stop("S must be square");
  
  if ((S - S.transpose()).norm() > 1e-6 * S.norm())
    stop("S must be symmetric");  
    
  SelfAdjointEigenSolver<MatrixXd> es(S, EigenvaluesOnly);
  return VectorXd(es.eigenvalues().reverse());
}


// [[Rcpp::export]]
Rcpp::List GenEigenC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::MatrixXd>& B) {
  
  if (A.cols() != A.rows())
    stop("A must be square");
  if ((A - A.transpose()).norm() > 1e-6 * A.norm())
    stop("A must be symmetric");  
  
  if (B.cols() != B.rows())
      stop("B must be square");
    if ((B - B.transpose()).norm() > 1e-6 * B.norm())
      stop("B must be symmetric");  


  GeneralizedSelfAdjointEigenSolver<MatrixXd> es(A, B);
  return List::create(
    Named("vec") = MatrixXd(es.eigenvectors()).rowwise().reverse(),
    Named("val") = VectorXd(es.eigenvalues()).reverse()
  );
}

// [[Rcpp::export]]
Eigen::MatrixXd solveC(const Eigen::Map<Eigen::MatrixXd>& S) {
  const int d = S.cols();
  if (S.cols() != S.rows())
    stop("S must be square");

  LLT<MatrixXd> llt(S);
  return llt.solve(MatrixXd::Identity(d, d));
}
