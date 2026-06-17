#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath>
#include <stdexcept>

#include "support_shared.h"

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

// same as Cpp_Utils_exptd_R_v2.cpp. CorCompC() but
// takes Ref<const 
Eigen::MatrixXd makeCorComp_int(const Eigen::Ref<const Eigen::MatrixXd>& A,
                             const Eigen::Ref<const Eigen::MatrixXd>& S,
                             int ncomps){
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
    Rcpp::stop("makeCorComp_int failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeCorComp_int failed: unknown C++ error");
  }
}

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
  try {
    int p = S.cols();
    int d = e.size();
    
    if (S.rows() != S.cols())
      Rcpp::stop("makeSubS: S must be square");
    if (d < 1)
      Rcpp::stop("makeSubS: index vector must be non-empty");
    if (d > p)
      Rcpp::stop("makeSubS: too many indices");
    if (e.minCoeff() < 0)
      Rcpp::stop("makeSubS: indices must be non-negative");
    if (e.maxCoeff() >= p)
      Rcpp::stop("makeSubS: largest index greater than the number of columns");
    
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
  catch (std::exception& ex) {
    Rcpp::stop("makeSubS failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeSubS failed: unknown C++ error");
  }
} 

// creates M as G'G, where G is S[, e] 
// @param S input matrix (p x p) 
// @param M output matrix (size(e), size(e) )
// @details used in pspca with S = G deflated S
//
// @returns matrix M by reference
// 
// @noRd
void maked_loopColF(const Eigen::Ref<const Eigen::MatrixXd>& S,
                    Eigen::MatrixXd& M,
                    Eigen::VectorXi& e)
{
  try {
    int d = e.size();
    
    if (d < 1)
      Rcpp::stop("maked_loopColF: index vector must be non-empty");
    if (e.minCoeff() < 0)
      Rcpp::stop("maked_loopColF: indices must be non-negative");
    if (e.maxCoeff() >= S.cols())
      Rcpp::stop("maked_loopColF: largest index greater than the number of columns");
    if (M.rows() < d || M.cols() < d)
      Rcpp::stop("maked_loopColF: M is too small");
    
    for (int i = 0; i < d; i++){
      for (int ii = i; ii < d; ii++){  
        M(i, ii) = S.col(e(i)).transpose() * S.col(e(ii));  
        M(ii, i) = M(i, ii);
      } 
    }
  }
  catch (std::exception& ex) {
    Rcpp::stop("maked_loopColF failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("maked_loopColF failed: unknown C++ error");
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
  try {
    int d = ind.size();
    
    if (d < 1)
      Rcpp::stop("makeXdC: index vector must be non-empty");
    if (ind.minCoeff() < 0)
      Rcpp::stop("makeXdC: indices must be non-negative");
    if (ind.maxCoeff() >= X.cols())
      Rcpp::stop("makeXdC: largest index greater than the number of columns");
    if (Xd.rows() != X.rows() || Xd.cols() < d)
      Rcpp::stop("makeXdC: Xd has incompatible dimensions");
    
    for (int i = 0; i < d; i++)
      Xd.col(i) = X.col(ind(i));
  }
  catch (std::exception& ex) {
    Rcpp::stop("makeXdC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeXdC failed: unknown C++ error");
  }
}

Eigen::MatrixXd CorCompC(const  Eigen::Ref<const Eigen::MatrixXd>& A,
                         const  Eigen::Ref<const Eigen::MatrixXd>& S,
                         int d)  
{
  try {
    return makeCorComp_int(A, S, d);
  }
  catch (std::exception& ex) {
    Rcpp::stop("CorCompC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("CorCompC failed: unknown C++ error");
  }
}

Eigen::MatrixXd cor_int(const Eigen::Ref<const Eigen::MatrixXd>& X,
                        bool center,
                        bool scale)
{
  try {
    
    Eigen::MatrixXd C = X;
    
    if (center)
      C.array().rowwise() -= C.colwise().mean().array();
    
    if (scale) {
      Eigen::VectorXd s = C.colwise().norm();
      
      if ((s.array() <= 0.0).any())
        Rcpp::stop("at least one column has zero norm");
      
      C.array().rowwise() /= s.array().transpose();
    }

    return C.transpose() * C;;
  }
  catch (std::exception& ex) {
    Rcpp::stop("corC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("corC failed: unknown C++ error");
  }
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
  try {
    if (X.rows() != X.cols())
      Rcpp::stop("eigvecPMC: X must be square");
    if (X.cols() < 1)
      Rcpp::stop("eigvecPMC: X must have positive dimension");
    if (eps <= 0.0)
      Rcpp::stop("eigvecPMC: eps must be positive");
    if (maxiter < 1)
      Rcpp::stop("eigvecPMC: maxiter must be positive");
    
    const int p = X.cols();
    double sqp = sqrt(double(p));
    Eigen::VectorXd v0 = VectorXd::Constant(p, 1.0/sqp);
    Eigen::VectorXd v = VectorXd::Constant(p, 0.0);
    double stp = 1.0;
    int k = 0;
    
    while (stp > eps){
      v = X * v0;
      val = v.norm();
      if (!std::isfinite(val) || val <= 0.0)
        throw std::runtime_error("power method failed in eigvecPMC");
      
      v = v.array()/val;
      stp = (v0.array() - v.array()).matrix().norm();
      v0 = v;
      k++;
      
      if (k % 200 == 0)
        Rcpp::checkUserInterrupt();
      
      if (k > maxiter) {
        Rcpp::warning("power method did not converge; returning last iterate");
        break;
      }
    }  
    
    return (v.array() * sqrt(val));  
  }
  catch (std::exception& ex) {
    Rcpp::stop("eigvecPMC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("eigvecPMC failed: unknown C++ error");
  }
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
//  the eigenvalue is returned by reference in val
// 
// @noRd
Eigen::VectorXd GeigvecPMC(const Eigen::MatrixXd& A,
                           const Eigen::MatrixXd& B,
                           double& val,
                           double eps, //1e-5,
                           int maxiter) // 100)
{
  try {
    if (A.rows() != A.cols() || B.rows() != B.cols())
      Rcpp::stop("GeigvecPMC: A and B must be square");
    if (A.rows() != B.rows())
      Rcpp::stop("GeigvecPMC: A and B must have the same dimensions");
    if (B.cols() < 1)
      Rcpp::stop("GeigvecPMC: matrices must have positive dimension");
    if (eps <= 0.0)
      Rcpp::stop("GeigvecPMC: eps must be positive");
    if (maxiter < 1)
      Rcpp::stop("GeigvecPMC: maxiter must be positive");
    
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
      
      if (k > maxiter) {
        Rcpp::warning("power method did not converge; returning last iterate");
        break;
      }
    }
    
    v = L.transpose() * v;
    const double nv = v.norm();
    if (!std::isfinite(nv) || nv <= 0.0)
      throw std::runtime_error("generalized eigenvector normalization failed");
    
    v /= nv;
    return v;
  }
  catch (std::exception& ex) {
    Rcpp::stop("GeigvecPMC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("GeigvecPMC failed: unknown C++ error");
  }
}
