#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath>
#include <stdexcept>

#include "support_shared_v2.h"

using namespace Rcpp;
using namespace std;

// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;
using Eigen::Matrix;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::MatrixXi;
using Eigen::VectorXi;
using Eigen::GeneralizedSelfAdjointEigenSolver;
using Eigen::SelfAdjointEigenSolver;
using Eigen::LLT;
using Eigen::Ref;

// Creates a sub-matrix of covariance matrix S with indices in e, 
//   both rows and columns faster x2 varsion, because in Eigen matrices 
//   are column-major so faster than symm i=j..d
// @param S matrix to reduce
// @param e indices of columns/rows to keep
// 
// @returns The submatrix sub
// 
// @noRd
Eigen::MatrixXd makeSubS(
    const Eigen::Ref<const Eigen::MatrixXd>& S, 
    Eigen::VectorXi& e)
  {
  int p = S.cols();
  int d = e.size();
  
  if (d > p) {
    Rcpp::stop("makeSubS:Too many indices to eliminate.\n");
  }
  if (e.maxCoeff() >= p){
    Rcpp::stop("makeSubS: largest index greater than the number of columns.\n");
  }
  // here should check if any index is repeated
  if (d == p)
    return S;
  
  Eigen::MatrixXd M(d, d);  
  for (int j = 0; j < d; ++j){
    for (int i = 0; i < d; ++i){
      M(i, j) = S(e(i), e(j));
    }
  }
  
  return M;
} 

// creates M as G'G, where G is S[, e] 
// @param S input matrix (p x p) 
// @param M output matrix (size(e), size(e) )
// @details used in pspca with S = G deflated S
//
// @returns matrix M by reference
// 
// @noRd
void maked_loopColF(const Eigen::Ref<const Eigen::MatrixXd>& S, Eigen::MatrixXd& M, Eigen::VectorXi& e){
  int d = e.size();
  for (int i = 0; i < d; i++){
    for (int ii = i; ii < d; ii++){  
      M(i, ii) = S.col(e(i)).transpose() * S.col(e(ii));  
      M(ii, i) = M(i, ii);
    } 
  }      
}

// Selects columns of a matrix
// @param X matrix (n x p)
// @param ind integer vector of columns to keep
// @param Xd matrix (n x ind.size()) output
// 
// @returns submatrix Xd by reference
// 
// @noRd
void makeXdC(const Eigen::Ref<const Eigen::MatrixXd>& X,
                    const Eigen::Ref<const Eigen::VectorXi>& ind,
                                 MatrixXd& Xd)
{
  int d = ind.size();
  for (int i = 0; i < d; i++)
    Xd.col(i) = X.col(ind(i));
}

Eigen::MatrixXd CorCompC(const Eigen::Map<Eigen::MatrixXd>& A,
                             const Eigen::Map<Eigen::MatrixXd>& S,
                             int d) // 0
  {
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

// Power method computes first eigvec, about 82 times faster than eigen!
// 
// @param X symmetric matrix
// @param val the largest eigenvalue (output)
// @param eps (default 1e-5), tolerance for convergence; 
//   int maxiter (default 100).
 // 
 // @details returns eigevc*sqrt(lambda) 
 //   because it is used for variable selection
 // 
 // @returns the eigenvector corresponding to the largest eigenvalue multiplied
 //  by the eigenvalue and the eigenvalue as `val` by reference
 // 
 // @noRd
Eigen::VectorXd eigvecPMC(
    const Eigen::MatrixXd& X, 
    double& val, 
    double eps, // 1e-5, 
    int maxiter) // 100)
  {
  const int p = X.cols();
  double sqp = sqrt(double(p));
  Eigen::VectorXd v0 = VectorXd::Constant(p, 1.0/sqp);
  Eigen::VectorXd v = VectorXd::Constant(p, 0.0);
  double stp = 1.0;
  int k = 0;
  while (stp > eps){
    v = X * v0;
    val = v.norm();
    v = v.array()/val;
    stp = (v0.array() - v.array()).matrix().norm();
    v0 = v;
    k++;
    if (k % 200 == 0) // in case maxiter increased
      Rcpp::checkUserInterrupt();
    Rcpp::warning("power method did not converge; returning last iterate");
    break;
    }  

  //  Rcout << "k = " << k << "; stp = " << stp << endl;
  return (v.array() * sqrt(val));  
}

// GeigvecPMC: computes the first eingenpair by power method
// Throws on numerical breakdown.
// cspca_varselC() catches failures from the generalized-eigen step and
// reports them through the singular flag without crashing the search.
// Candidate subsets can then be skipped by the calling search routine.
// @param A and B two square matrices for generalized eigendecomposition 
//   Av = val Bv. B must be full rank.
// @param val double output with largest eigenvalue
// @param eps (default 1e-5), tolerance for convergence;
//  int maxiter (default 100).
// 
// @details throws an error if B is singular
// 
// @returns the eigenvector corresponding to the largest generalized eigenvalue
// 
// @noRd
Eigen::VectorXd GeigvecPMC(const Eigen::MatrixXd& A,
                                  const Eigen::MatrixXd& B,
                                  double& val,
                                  double eps, //1e-5,
                                  int maxiter) // 100)
{
  const int p = B.cols();
  const double sqp = std::sqrt(double(p));
  
  Eigen::LLT<Eigen::MatrixXd> llt(B);
  if (llt.info() != Eigen::Success)
    throw std::runtime_error("LLT failed in GeigvecPMC");
  
  const Eigen::MatrixXd L = llt.matrixL().solve(MatrixXd::Identity(p, p));
  Eigen::MatrixXd M = L * A * L.transpose();
  
  Eigen::VectorXd v0 = VectorXd::Constant(p, 1.0 / sqp);
  Eigen::VectorXd v = VectorXd::Zero(p);
  double stp = 1.0;
  int k = 0;
  
  while (stp > eps) {
    v = M * v0;
    val = v.norm();
    if (!std::isfinite(val) || val <= 0.0)
      throw std::runtime_error("power method failed in GeigvecPMC");
    
    v = (v.array() / val).matrix();
    stp = (v0 - v).norm();
    v0 = v;
    ++k;
    
    if (k % 200 == 0)
      Rcpp::checkUserInterrupt();
    
    if (k > maxiter)
      Rcpp::warning("power method did not converge; returning last iterate");
    break;
  }
  
  v = L.transpose() * v;
  const double nv = v.norm();
  if (!std::isfinite(nv) || nv <= 0.0)
    throw std::runtime_error("generalized eigenvector normalization failed");
  
  v /= nv;
  return v;
}
