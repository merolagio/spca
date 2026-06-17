#include <Rcpp.h>
#include <RcppEigen.h>
#include <set>
#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <string>

#include "support_shared.h"
#include "support_tall.h"

using namespace Rcpp;
using namespace Eigen;
using namespace std;

// [[Rcpp::depends(RcppEigen)]]

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::SelfAdjointEigenSolver;
using Rcpp::List;
using std::string;

static void orient_first_positive(Eigen::VectorXd& x)
{
  if (x.size() > 0 && x(0) < 0.0)
    x = -x;
}

// Compute the first ncomps eigenpairs of a covariance/correlation matrix.
// Uses power method followed by covariance deflation at each step.
Rcpp::List PMnEigenpairs_tall(const Eigen::Ref<const Eigen::MatrixXd>& S,
                           int ncomps,
                           double epsPM,
                           int maxiterPM)
{
  const int p = S.cols();
  Eigen::MatrixXd G = S;
  Eigen::MatrixXd eigvec(p, ncomps);
  Eigen::VectorXd eigval(ncomps);
  Eigen::VectorXi ind = Eigen::VectorXi::LinSpaced(p, 0, p - 1);

  for (int j = 0; j < ncomps; ++j) {
    double val = 0.0;
    Eigen::VectorXd a_scaled = eigvecPMC(G, val, epsPM, maxiterPM);
    if (!std::isfinite(val) || val <= 0.0)
      Rcpp::stop("PMnEigenpairs: non-positive eigenvalue in tall backend");

    Eigen::VectorXd a = a_scaled / std::sqrt(val);
    orient_first_positive(a);

    double defl_vexp = 0.0;
    const double failed = deflSC(a, G, ind, defl_vexp, epsPM);
    if (failed != 0.0)
      Rcpp::stop("PMnEigenpairs: deflation failed in tall backend");

    eigvec.col(j) = a;
    eigval(j) = val;
  }

  return Rcpp::List::create(
    Rcpp::Named("vec") = eigvec,
    Rcpp::Named("val") = eigval
  );
}

// ==============================================
// # Rank-1 deflates of the covariance matrix G of a new component with loadings a
// ==============================================
// @param a  ONLY NONZERO loadings
// @param G = deflated covariance matrix (input and output)
//   G <-- (G - Gaa'G/(a'Ga) 
// @param a nonzero loadings of new component
// @param ind indices of nonzero positions
// @param vexp output variance explained by new component 
// @param eps tolerance for norm new component
// 
// @returns 1 if failed (||component|| < eps), 0 otherwise
 // 
 // @noRd
double deflSC(
    const Eigen::Ref<const Eigen::VectorXd>& a, 
    Eigen::MatrixXd& G, 
    Eigen::VectorXi& ind, 
    double& vexp, 
    double eps){// = 1e-5

  const int n = ind.size();
  const int p = G.cols();
  double out = 0;

  // t = Ga  only elements in ind multiplied
  Eigen::VectorXd t = Eigen::VectorXd::Zero(p);
  for (int i = 0; i < p; i++)
    for(int k = 0; k < n; k++)
      t(i) += G(i, ind(k)) * a(k ); 
  
  // tt = a'Ga = t'a
    double tt = 0.0;
  for(int k = 0; k < n; k++){
    tt += a(k) * t(ind(k));
  }
  if (tt > eps){
    tt = 1/tt;
    // O = Ga/(tt)
    const Eigen::VectorXd O = (t.array()*tt).matrix();
    
    const double cvk = G.trace();
    // G = G - Gaa'G/(a'Ga) deflated S
    G = G - t * O.transpose(); //deflated S
    vexp =  cvk - G.trace() ;
  }
  else{
    out = 1;
    Rcout << "defSC: 1/tt = " << tt << endl;  
    Rf_warning("defSC: norm of component is not > 0");
    vexp = nan("1");
  }
  return out;
}

// Deflates the covariance matrix G and the M = G'G (deflated product matrix //  both are input and output pass only a nonzero loads
// @param G <-- (G - Gaa'G/(a'Ga) // deflated S matrix
// @param M deflated product matrix
//  M <-- M - Maa'G/(a'Ga) - 2Gaa'M/(a'Ga) + Gaa'Maa'G/(a'Ga)^2
// @param a  ONLY NONZERO loadings
// @param ind indices of nonzero positions
// @param vexp output variance explained by new component 
// @param eps tolerance for norm new component
// 
// @returns 0 if success, 1 if failed (a'Ga not > 0)
// 
// @noRd
 double deflSandDC(
    const Eigen::Ref<const Eigen::VectorXd>& a, 
    Eigen::MatrixXd& G, 
    Eigen::MatrixXd& M, 
    Eigen::VectorXi& ind, 
    double& vexp)
  {
  const int n = ind.size();
  const int p = G.cols();
  double out = 0;
  
  // t = Ga
  Eigen::VectorXd t = Eigen::VectorXd::Zero(p); 
  for (int i = 0; i < p; i++)
    for(int k = 0; k < n; k++) 
      t(i) += G(i, ind(k)) * a(k ); // only elements in ind
  // tt = a'Ga = t'a
  
  double tt = 0.0; 
  for(int k = 0; k < n; k++)
    tt += a(k) * t(ind(k));
  if (tt > 0){
    tt = 1/tt;
  }
  else{
    out = 1;
    Rf_warning("deflSandDC: a'Ga is not > 0");
    vexp = nan("1");
    return out;
  }
  
  // O = Ga/(tt)
  const Eigen::VectorXd O = (t.array()*tt).matrix();
  
  const double cvk = G.trace();
  // G = G - Gaa'G/(a'Ga) deflated S
  Eigen::MatrixXd L = t * O.transpose();
  G = G - t * O.transpose(); //deflated S
  vexp =  cvk - G.trace() ;
  
  // deflate M
  
  // N = aa'G/(tt) = a*t'/(tt) = a*O' (n x p)
  Eigen::MatrixXd N = Eigen::MatrixXd::Zero(n, p); 
  for (int i = 0; i < n; i++)
    for (int j = 0; j < p; j++)
      N(i, j) += a(i) * O(j);  
  
  //W = Maa'G/(a'Ga) = M.transpose() * N; // (p, p)
  Eigen::MatrixXd W = Eigen::MatrixXd::Zero(p,p); 
  for (int i = 0; i < p; i++)
    for (int j = 0; j < p; j++)
      for(int k = 0; k < n; k++) 
        W(i, j) += M(i, ind(k)) * N(k, j);  // (p, p)
  
  //  H = N.transpose() * W (p x p)
  Eigen::MatrixXd H = Eigen::MatrixXd::Zero(p,p); // 
  for (int i = 0; i < p; i++)
    for (int j = 0; j < p; j++)
      for(int k = 0; k < n; k++) 
        H(i, j) += N(k, i) * W(ind(k), j);  
  M = (M.array() - W.array() - W.transpose().array() + H.array()).matrix(); 
  return out;
} 

// computes the extra variance explained by a set of components
// @param A full cardinality loading matrix including current
// @param S covariance matrix
//
// @details extra variance explained is computed by difference 
//   vexp[k] = cvexp[k] - cvexp[(k - 1)]  
// @returns vectors of vexps and cvexps
// 
// @noRd
Rcpp::List makeVexpSC(
    const Eigen::Ref<const Eigen::MatrixXd>& A,
    const Eigen::Ref<const Eigen::MatrixXd>& S) {
  try {
    const int p = A.cols();
    
    if (p < 1)
      Rcpp::stop("makeVexpSC: A must have at least one column");
    if (A.rows() != S.rows() || S.rows() != S.cols())
      Rcpp::stop("makeVexpSC: incompatible dimensions of A and S");
    if (!A.allFinite() || !S.allFinite())
      Rcpp::stop("makeVexpSC: A and S must contain only finite values");
    
    Eigen::VectorXd vexp(p);
    Eigen::VectorXd cvexp(p);
    
    Eigen::MatrixXd M = S * A;
    
    const double denom0 = A.col(0).dot(M.col(0));
    if (!std::isfinite(denom0) || denom0 <= 0.0)
      Rcpp::stop("makeVexpSC: non-finite or zero denominator for first component");
    
    vexp(0) = M.col(0).squaredNorm() / denom0;
    cvexp(0) = vexp(0);
    
    for (int i = 1; i < p; i++) {
      const int k = i + 1;
      
      Eigen::MatrixXd G(k, k);
      G.noalias() = A.leftCols(k).transpose() * M.leftCols(k);
      
      Eigen::LDLT<Eigen::MatrixXd> ldlt(G);
      if (ldlt.info() != Eigen::Success || !ldlt.isPositive())
        Rcpp::stop("makeVexpSC: A' S A is singular or not positive definite");
      
      Eigen::MatrixXd H(k, k);
      H.noalias() = M.leftCols(k).transpose() * M.leftCols(k);
      
      Eigen::MatrixXd GH = ldlt.solve(H);
      if (!GH.allFinite())
        Rcpp::stop("makeVexpSC: solve failed; A' S A is singular");
      
      cvexp(i) = GH.trace();
      vexp(i) = cvexp(i) - cvexp(i - 1);
    }
    
    return Rcpp::List::create(
      Rcpp::Named("vexp") = vexp,
      Rcpp::Named("cvexp") = cvexp
    );
  }
  catch (std::exception& ex) {
    Rcpp::stop("makeVexpSC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeVexpSC failed: unknown C++ error");
  }
}


// computes  cum variance explained by one components
// @param A full cardinality loading matrix including current
// @param S covariance matrix
// @param cvexp input and output full cumulative vexp
// @returns 
// 
// @noRd
double makeCvexpOneCompSC_int(
    const Eigen::Ref<const Eigen::MatrixXd>& A,
    const Eigen::Ref<const Eigen::MatrixXd>& S,
    double& cvexp) {
  try {
    if (A.rows() != S.rows() || S.rows() != S.cols())
      Rcpp::stop("makeCvexpOneCompSC_int: incompatible dimensions of A and S");
    if (!A.allFinite() || !S.allFinite())
      Rcpp::stop("makeCvexpOneCompSC_int: A and S must contain only finite values");
    
    Eigen::MatrixXd B = A.transpose() * S;
    Eigen::MatrixXd G = B * A;
    Eigen::MatrixXd H = B * B.transpose();
    
    Eigen::LDLT<Eigen::MatrixXd> ldlt(G);
    if (ldlt.info() != Eigen::Success || !ldlt.isPositive())
      Rcpp::stop("makeCvexpOneCompSC_int: A' S A is singular or not positive definite");
    
    Eigen::MatrixXd GH = ldlt.solve(H);
    if (!GH.allFinite())
      Rcpp::stop("makeCvexpOneCompSC_int: solve failed; A' S A is singular");
    
    cvexp = GH.trace();
    if (!std::isfinite(cvexp))
      Rcpp::stop("makeCvexpOneCompSC_int: non-finite cumulative variance explained");
    
    return cvexp;
  }
  catch (std::exception& ex) {
    Rcpp::stop("makeCvexpOneCompSC_int failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("makeCvexpOneCompSC_int failed: unknown C++ error");
  }
}


void uspcaC(
    const Eigen::Ref<const Eigen::MatrixXd>& S,
    double& eigval,
    int p,
    int comp_number,
    Eigen::VectorXi& indj,
    int cardt,
    Eigen::VectorXi& indjm1,
    int cardm1,
    Eigen::MatrixXd& R,
    Eigen::VectorXd& a,
    bool& singular,
    bool PM,
    double epsPM,
    int maxiter) {
  if (cardt < comp_number + 1) {
    Rcpp::Rcout << "uspca, component " << comp_number + 1 << std::endl;
    Rcpp::stop("must pass cardt > comp_number + 1");
  }
  
  singular = false;
  eigval = 0.0;
  
  try {
    Eigen::VectorXi indt = indj.head(cardt);
    Eigen::MatrixXd Sd = makeSubS(S, indt);
    
    Eigen::MatrixXd D(p, p);
    Eigen::MatrixXd K(comp_number, cardt);
    Eigen::MatrixXd M(comp_number, comp_number);
    Eigen::MatrixXd C(cardt, cardt);
    Eigen::MatrixXd Q(cardt, cardt);
    Eigen::MatrixXd P(cardt, cardt);
    Eigen::VectorXd vec(cardt);
    double st;
    
    if (comp_number > 0) {
      Eigen::LDLT<Eigen::MatrixXd> ldltSd(Sd);
      if (ldltSd.info() != Eigen::Success || !ldltSd.isPositive())
        throw std::runtime_error("selected covariance submatrix is singular in uspca");
      
      for (int i = 0; i < p; i++) {
        st = 0.0;
        for (int ii = 0; ii < cardm1; ii++)
          st = st + a(ii) * S(indjm1(ii), i);
        R(comp_number - 1, i) = st;
      }
      
      for (int i = 0; i < comp_number; i++) {
        Eigen::VectorXd rhs(cardt);
        for (int ii = 0; ii < cardt; ii++)
          rhs(ii) = R(i, indj(ii));
        
        Eigen::VectorXd sol = ldltSd.solve(rhs);
        if (!sol.allFinite())
          throw std::runtime_error("solve with selected covariance submatrix failed in uspca");
        
        K.row(i) = sol.transpose();
      }
      
      for (int i = 0; i < comp_number; i++) {
        for (int ii = 0; ii < comp_number; ii++) {
          st = 0.0;
          for (int iii = 0; iii < cardt; iii++)
            st = st + K(i, iii) * R(ii, indj(iii));
          M(i, ii) = st;
        }
      }
      
      if (comp_number == 1) {
        if (!std::isfinite(M(0, 0)) || std::abs(M(0, 0)) <= 0.0)
          throw std::runtime_error("constraint denominator is zero in uspca");
        
        for (int i = 0; i < cardt; i++) {
          for (int ii = 0; ii < comp_number; ii++)
            Q(i, ii) = R(ii, indj(i)) / M(0, 0);
        }
        
        C = Eigen::MatrixXd::Identity(cardt, cardt) -
          Q.leftCols(comp_number) * K;
      } else {
        Eigen::LDLT<Eigen::MatrixXd> ldltM(M);
        if (ldltM.info() != Eigen::Success)
          throw std::runtime_error("constraint matrix factorization failed in uspca");
        
        Eigen::MatrixXd Rt(cardt, comp_number);
        for (int i = 0; i < cardt; i++) {
          for (int ii = 0; ii < comp_number; ii++)
            Rt(i, ii) = R(ii, indj(i));
        }
        
        Q.leftCols(comp_number) = ldltM.solve(Rt.transpose()).transpose();
        if (!Q.leftCols(comp_number).allFinite())
          throw std::runtime_error("constraint matrix solve failed in uspca");
        
        C = Eigen::MatrixXd::Identity(cardt, cardt) -
          Q.topLeftCorner(cardt, comp_number) *
          K.topLeftCorner(comp_number, cardt);
      }
      
      maked_loopColF(S, D, indt);
      P.topLeftCorner(cardt, cardt) =
        C * D.topLeftCorner(cardt, cardt) * C.transpose();
    } else {
      maked_loopColF(S, P, indt);
    }
    
    if (PM) {
      vec = GeigvecPMC(P, Sd, eigval, epsPM, maxiter);
    } else {
      Eigen::GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> es(P, Sd);
      if (es.info() != Eigen::Success)
        throw std::runtime_error("generalized eigen solver failed in uspca");
      vec = es.eigenvectors().col(cardt - 1);
      eigval = es.eigenvalues()(cardt - 1);
    }
    
    const double nv = vec.norm();
    if (!std::isfinite(eigval) || !std::isfinite(nv) || nv <= 0.0)
      throw std::runtime_error("invalid generalized eigen solution in uspca");
    
    a.head(cardt) = vec.array() / nv;
  }
  catch (...) {
    singular = true;
    eigval = 0.0;
    a.head(cardt).setZero();
  }
}

// cspca
// computes cSPCA loadings
// 
// @param S is original cov matrix S = X'X
// @param M = G'G  where G is the deflated covariance matrix
// @param indj vector of indices of the variables selected
//
// computes sparse loadings, a, as: define Hdot = H[, indj] then    
// Xdot'QQ'Xdot a = Xdot'Xdot lambda a  <==> M[indj, indj] a = lambda S[indj, indj] a 
// output by reference a loadings, eigval = la is vexp (approximated if sPCs correlated)
// singular (output) true if the eigen step failed
 // 
 // @noRd
 
 void cspcaC(const Eigen::Ref<const Eigen::MatrixXd>& S, Eigen::MatrixXd& M, double& eigval,
           int p,   int comp_number,
           Eigen::VectorXi& indj, 
           int cardt, 
           Eigen::VectorXd& a, 
           bool& singular,
           bool PM, // false, 
           double epsPM, // = 1e-5, 
           int maxiter) // = 100)
  {
  
  singular = false;
  eigval = 0.0;
  
  // single variable: no eigensolve needed
  if (cardt == 1) {
    try {
      a(0) = 1.0;
      eigval = M(indj(0), indj(0)) / S(indj(0), indj(0));
    } catch (...) {
      singular = true;
      eigval = 0.0;
      a(0) = 0.0;
    }
    return;
  }
  
  VectorXi indt = indj.head(cardt);
  Eigen::MatrixXd Sd =  makeSubS(S, indt);
  Eigen::MatrixXd Dd(cardt, cardt); 

    // computes 
      maked_loopColF(M, Dd, indt); // this makes G[, indj]'G[, indj] on fly from deflated S
    
  try {
    //  compute loadings        
    if (PM){
      a.head(cardt) =  GeigvecPMC(Dd, Sd, eigval, epsPM, maxiter);
      
    }
    else{
      GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> es(Dd, Sd);
      if (es.info() != Eigen::Success)
        throw std::runtime_error("generalized eigen solver failed in cspca");
      a.head(cardt) = es.eigenvectors().col(cardt - 1);    
      eigval = es.eigenvalues()(cardt - 1);
    }
    
    const double na = a.head(cardt).norm();
    if (!std::isfinite(eigval) || !std::isfinite(na) || na <= 0.0)
      throw std::runtime_error("invalid generalized eigen solution in cspca");
    
    a.head(cardt) = a.head(cardt).array()/na;
  } catch (...) {
    singular = true;
    eigval = 0.0;
    a.head(cardt).setZero();
  }
  return;
}



// pspca
// S = X'X is cov matrix of data matrix X
// si is first eigvec of S scaled to norm sqrt(lambda)
// indj the indices of the variables selected
//
// computes sparse loadings, a, by simply projecting the PC u onto Xd = X[, indj]
// uses the trick that Xd'u = lambda v[indj] with lambda = eigaval and v eigenvector of S
// singular (output) true if the submatrix inversion failed
//
void pspcaC(
    const Eigen::Ref<const Eigen::MatrixXd>& S,
    const Eigen::VectorXd& si,
    Eigen::VectorXi& indj,
    int cardt,
    Eigen::VectorXd& a,
    bool& singular) {
  singular = false;
  
  try {
    Eigen::VectorXi indt = indj.head(cardt);
    Eigen::MatrixXd Sd = makeSubS(S, indt);
    
    Eigen::LDLT<Eigen::MatrixXd> ldltSd(Sd);
    if (ldltSd.info() != Eigen::Success || !ldltSd.isPositive())
      throw std::runtime_error("selected covariance submatrix is singular in pspca");
    
    Eigen::VectorXd v(cardt);
    for (int i = 0; i < cardt; i++)
      v(i) = si(indj(i));
    
    Eigen::VectorXd u = ldltSd.solve(v);
    if (!u.allFinite())
      throw std::runtime_error("solve with selected covariance submatrix failed in pspca");
    
    const double nu = u.norm();
    if (!std::isfinite(nu) || nu <= 0.0)
      throw std::runtime_error("invalid solution in pspca");
    
    a.head(cardt) = u.array() / nu;
  }
  catch (...) {
    singular = true;
    a.head(cardt).setZero();
  }
}



// =====================================================================
// NOTE: Input validation has been moved to the main wrapper (lsspcaC).
// varsel_fbsC assumes all inputs are already validated by the caller.
// =====================================================================


// =====================================================================
// HELPERS (used by varsel_fbs_r2C)
// =====================================================================

// returns index with max vt(j) among available (status == -2).
// Returns -1 if none available.
static int find_best_available(
    const VectorXi& status,
    const VectorXd& vt,
    int p)
{
  int best = -1;
  double best_val = -1.0;
  for (int j = 0; j < p; j++) {
    if (status(j) == -2 && vt(j) > best_val) {
      best_val = vt(j);
      best = j;
    }
  }
  return best;
}

// rank-1 deflation of S_work and si_work after selecting variable j.
// Only available variables (status == -2) are updated in si_work.
static void deflate_selected(MatrixXd& S_work,
                             VectorXd& si_work,
                             const VectorXi& status,
                             int j,
                             int p)
{
  double tmp = si_work(j) / S_work(j, j);
  for (int k = 0; k < p; k++) {
    if (status(k) == -2)
      si_work(k) -= tmp * S_work(j, k);
    else
      si_work(k) = 0.0;
  }
  VectorXd ba = S_work.col(j) / std::sqrt(S_work(j, j));
  S_work -= ba * ba.transpose();
}

// builds initial active set for backward.
static int build_full_active_set(int p,
                                 VectorXi& indices)
{
  int cardinality = 0;
  for (int j = 0; j < p; j++)
    indices(cardinality++) = j;
  return cardinality;
}

// removes entry at position pos from indices[0..cardinality-1].
static void remove_index_pos(
    VectorXi& indices,
    int& cardinality,
    int pos)
{
  for (int i = pos; i < cardinality - 1; i++)
    indices(i) = indices(i + 1);
  cardinality--;
}


// cspca_varsel
// Makes correlated LS cSPCA for variable selection.
//
// S is original covariance matrix (p x p)
// M is deflated product matrix G'G (p x p)
// eigval (output) approximate variance explained
// indj indices of selected variables (0-based)
// cardt cardinality of selected subset
// a (output) loadings (only first cardt elements used)
// singular (output) true if the eigen step failed
//
// Singularity policy:
//   - No separate rank check is used in this layer.
//   - The generalized-eigen computation is attempted directly.
//   - If it fails numerically, singular is set to true, eigval is set to 0,
//     the returned loadings are zeroed, and the caller decides whether to skip
//     the candidate subset or treat the failure as fatal for the current stage.
static void cspca_varsel(
    const Eigen::Ref<const Eigen::MatrixXd>& S,
    const Eigen::Ref<const Eigen::MatrixXd>& M,
    double& eigval,
    Eigen::VectorXi& indj,
    int cardt,
    Eigen::VectorXd& a,
    bool& singular,
    bool PMSPC,// = false,
    double epsPMSPC, // = 1e-5,
    int maxiterPMSPC) // = 100)
{
  singular = false;
  eigval = 0.0;
  if (cardt <= 0)
    return;
  
  // single variable: no eigensolve needed
  if (cardt == 1) {
    try {
      a(0) = 1.0;
      eigval = M(indj(0), indj(0)) / S(indj(0), indj(0));
    } catch (...) {
      singular = true;
      eigval = 0.0;
      a(0) = 0.0;
    }
    return;
  }
  
  VectorXi indt = indj.head(cardt);
  Eigen::MatrixXd Sd = makeSubS(S, indt);
  Eigen::MatrixXd Dd = makeSubS(M, indt);
  
  try {
    if (PMSPC) {
      a.head(cardt) = GeigvecPMC(Dd, Sd, eigval, epsPMSPC, maxiterPMSPC);
    } else {
      GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> es(Dd, Sd);
      if (es.info() != Eigen::Success)
        throw std::runtime_error("generalized eigen solver failed in cspca_varsel");
      eigval = es.eigenvalues()(cardt - 1);
      a.head(cardt) = es.eigenvectors().col(cardt - 1);
    }
    
    const double na = a.head(cardt).norm();
    if (!std::isfinite(eigval) || !std::isfinite(na) || na <= 0.0)
      throw std::runtime_error("invalid generalized eigen solution in cspca_varsel");
    
    a.head(cardt) /= na;
  } catch (...) {
    singular = true;
    eigval = 0.0;
    a.head(cardt).setZero();
  }
}





// computes R^2 of a subset using sequential deflation.
static double compute_subset_r2(
    const Eigen::Ref<const Eigen::MatrixXd>& S,
    const VectorXd& si,
    const VectorXi& indices,
    int nsub,
    double rank_tol)
{
  const int p = S.cols();
  MatrixXd S_work = S;
  VectorXd si_work = si;
  VectorXi status = VectorXi::Constant(p, -1);
  double r2 = 0.0;

  for (int i = 0; i < nsub; i++)
    status(indices(i)) = -2;

  for (int i = 0; i < nsub; i++) {
    int j = indices(i);
    if (S_work(j, j) > rank_tol) {
      r2 += si_work(j) * si_work(j) / S_work(j, j);
      status(j) = 0;
      deflate_selected(S_work, si_work, status, j, p);
    } else {
      status(j) = 0;
    }
  }

  if (r2 < 0.0) r2 = 0.0;
  if (r2 > 1.0) r2 = 1.0;
  return r2;
}

// computes cvexp of a subset via cspca_varsel.
// Always calls cspca_varsel first. Returns prev_cvexp + eigval (approximate).
// If exact_cvexp = true and comp_number > 0, computes exact via B and
// makeCvexpOneCompSC_int.
//
// Singularity policy:
//   - if the generalized-eigen step fails, singmat is set to true;
//   - this helper returns 0.0 and lets the calling search decide whether to
//     skip the candidate subset or treat the failure as fatal.
static double compute_subset_cvexp(
    const Eigen::Ref<const Eigen::MatrixXd>& S,
    MatrixXd& M,
    VectorXi& indices,
    int cardinality,
    int p,
    int comp_number,
    double prev_cvexp,
    MatrixXd& B,
    VectorXd& loadings_tmp,
    bool& singmat,
    bool exact_cvexp,
    bool PMSPC,
    double epsPMSPC, // 1e-5
    int maxiterPMSPC) // 100
{
  singmat = false;
  if (cardinality == 0)
    return prev_cvexp;

  double eigval = 0.0;
  double cvexp_val = 0.0;

  cspca_varsel(S, M, eigval,
               indices, cardinality, loadings_tmp, singmat,
               PMSPC, epsPMSPC, maxiterPMSPC);

  if (singmat)
    return 0.0;

  cvexp_val = prev_cvexp + eigval;

  if (exact_cvexp && comp_number > 0) {
    B.col(comp_number).setZero();
    for (int i = 0; i < cardinality; i++)
      B(indices(i), comp_number) = loadings_tmp(i);

    MatrixXd Bleft = B.leftCols(comp_number + 1);
    makeCvexpOneCompSC_int(Bleft, S, cvexp_val);
  }

  return cvexp_val;
}


// =====================================================================
// 1. varsel_fbs_r2C
//
// Forward (selection_method=0), backward (selection_method=1), stepwise (selection_method=2).
// Ranking by partial R^2 (si/S deflation trick).
// Stopping: if stop_criterion=0, check R^2 >= alpha.
//           if stop_criterion=1, check cvexp >= alpha*target_cvexp.
//
// The algorithm is unchanged from v4.1.
// =====================================================================
static int varsel_fbs_r2C(const Eigen::Ref<const Eigen::MatrixXd>& S,
                          const VectorXd& si,
                          double pc_vexp,
                          double alpha,
                          int selection_method,
                          double rank_tol,
                          int mincard,
                          int stop_criterion,
                          MatrixXd& M,
                          double target_cvexp,
                          double prev_cvexp,
                          bool exact_cvexp,
                          int comp_number,
                          MatrixXd& B,
                          bool PMSPC,
                          double epsPMSPC,
                          int maxiterPMSPC,
                          VectorXi& indices,
                          int& cardinality,
                          double& criterion_value,
                          VectorXd& loadings_out,
                          double& vexp_out)
{
  int p = S.cols();
  double target = (stop_criterion == 0) ? alpha : alpha * target_cvexp;
  criterion_value = (stop_criterion == 0) ? 0.0 : prev_cvexp;
  VectorXd loadings_tmp(p);

  (void) pc_vexp;

  // FORWARD and STEPWISE
  if (selection_method == 0 || selection_method == 2) {
    MatrixXd S_work = S;
    VectorXd si_work = si;
    VectorXi status = VectorXi::Constant(p, -2);
    VectorXd vt = VectorXd::Zero(p);
    bool reached = false;

    if (criterion_value >= target && cardinality >= mincard)
      reached = true;

    // greedy forward
    while (!reached) {
      for (int j = 0; j < p; j++) {
        vt(j) = (status(j) == -2 && S_work(j, j) > rank_tol)
          ? si_work(j) * si_work(j) / S_work(j, j) : 0.0;
      }

      int chosen = -1;
      double chosen_cvexp = 0.0;
      int fallback_tries = (stop_criterion == 1) ? 3 : 1;
      int tried = 0;
      VectorXi status_try = status;

      while (tried < fallback_tries) {
        int cand = find_best_available(status_try, vt, p);
        if (cand < 0 || vt(cand) <= 0.0)
          break;

        if (stop_criterion == 0) {
          chosen = cand;
          break;
        }

        VectorXi trial = indices;
        trial(cardinality) = cand;
        bool singmat = false;
        double trial_cvexp = compute_subset_cvexp(S, M, trial, cardinality + 1, p, comp_number,
                                                  prev_cvexp, B, loadings_tmp, singmat,
                                                  exact_cvexp, PMSPC,
                                                  epsPMSPC, maxiterPMSPC);
        if (!singmat) {
          chosen = cand;
          chosen_cvexp = trial_cvexp;
          break;
        }

        status(cand) = -1;
        status_try(cand) = -1;
        ++tried;
      }

      if (chosen < 0) {
        if (stop_criterion == 1 && tried > 0)
          Rcpp::warning("varsel_fbs_r2C: unstable cvexp candidate subsets skipped");
        break;
      }

      indices(cardinality) = chosen;
      status(chosen) = 0;
      if (stop_criterion == 0)
        criterion_value += vt(chosen);
      else
        criterion_value = chosen_cvexp;
      deflate_selected(S_work, si_work, status, chosen, p);
      cardinality++;

      if (criterion_value >= target && cardinality >= mincard)
        reached = true;

      // stepwise backward cleanup
      if (selection_method == 2 && reached) {
        bool any_removed = true;
        while (any_removed) {
          any_removed = false;
          int best_remove = -1;
          double best_score = -1.0;

          for (int k = 0; k < cardinality; k++) {
            VectorXi trial(p);
            int ntrial = 0;
            for (int h = 0; h < cardinality; h++) {
              if (h != k)
                trial(ntrial++) = indices(h);
            }

            double trial_score = 0.0;
            if (stop_criterion == 0) {
              trial_score = compute_subset_r2(S, si, trial, ntrial, rank_tol);
            } else {
              bool singmat = false;
              trial_score = compute_subset_cvexp(S, M, trial, ntrial, p, comp_number,
                                                 prev_cvexp, B, loadings_tmp, singmat,
                                                 exact_cvexp, PMSPC,
                                                 epsPMSPC, maxiterPMSPC);
              if (singmat)
                continue;
            }

            if (trial_score >= target &&
                trial_score > best_score &&
                (cardinality - 1) >= mincard) {
              best_score = trial_score;
              best_remove = k;
            }
          }

          if (best_remove >= 0) {
            remove_index_pos(indices, cardinality, best_remove);
            criterion_value = best_score;
            any_removed = true;
          }
        }
      }
    }

    if (stop_criterion == 0 && criterion_value > 1.0)
      criterion_value = 1.0;

    if (!reached) {
      if (cardinality == 0)
        Rcpp::warning("varsel_fbs_r2C: no admissible variables available");
      else
        Rcpp::warning("varsel_fbs_r2C: target not reached");
    }

    // final cspca_varsel to get loadings (CVEXP mode)
    if (stop_criterion == 1 && cardinality > 0) {
      bool singular = false;
      cspca_varsel(S, M, vexp_out,
                   indices, cardinality, loadings_out, singular,
                   PMSPC, epsPMSPC, maxiterPMSPC);
      if (singular) {
        Rcpp::warning("varsel_fbs_r2C: final selected subset is singular");
        return 0;
      }
    }

    return reached ? 1 : 0;
  }

  // BACKWARD
  cardinality = build_full_active_set(p, indices);

  if (cardinality == 0) {
    Rcpp::warning("varsel_fbs_r2C: no admissible variables for backward");
    return 0;
  }

  if (stop_criterion == 0) {
    criterion_value = compute_subset_r2(S, si, indices, cardinality, rank_tol);
  } else {
    bool singmat = false;
    criterion_value = compute_subset_cvexp(S, M, indices, cardinality, p, comp_number,
                                           prev_cvexp, B, loadings_tmp, singmat,
                                           exact_cvexp, PMSPC,
                                           epsPMSPC, maxiterPMSPC);
    if (singmat) {
      Rcpp::warning("varsel_fbs_r2C: admissible subset is singular in backward search");
      return 0;
    }
  }

  if (criterion_value < target)
    Rcpp::warning("varsel_fbs_r2C: target not reachable with admissible variables");

  bool any_removed = true;
  while (any_removed && cardinality > mincard && criterion_value >= target) {
    any_removed = false;
    int best_remove = -1;
    double best_score = -1.0;

    for (int k = 0; k < cardinality; k++) {
      VectorXi trial(p);
      int ntrial = 0;
      for (int h = 0; h < cardinality; h++) {
        if (h != k)
          trial(ntrial++) = indices(h);
      }

      double trial_score = 0.0;
      if (stop_criterion == 0) {
        trial_score = compute_subset_r2(S, si, trial, ntrial, rank_tol);
      } else {
        bool singmat = false;
        trial_score = compute_subset_cvexp(S, M, trial, ntrial, p, comp_number,
                                           prev_cvexp, B, loadings_tmp, singmat,
                                           exact_cvexp, PMSPC,
                                           epsPMSPC, maxiterPMSPC);
        if (singmat)
          continue;
      }

      if (trial_score >= target && trial_score > best_score) {
        best_score = trial_score;
        best_remove = k;
      }
    }

    if (best_remove >= 0) {
      remove_index_pos(indices, cardinality, best_remove);
      criterion_value = best_score;
      any_removed = true;
    }
  }

  if (stop_criterion == 0 && criterion_value > 1.0)
    criterion_value = 1.0;

  if (stop_criterion == 1 && cardinality > 0) {
    bool singular = false;
    cspca_varsel(S, M, vexp_out,
                 indices, cardinality, loadings_out, singular,
                 PMSPC, epsPMSPC, maxiterPMSPC);
    if (singular) {
      Rcpp::warning("varsel_fbs_r2C: final selected subset is singular");
      return 0;
    }
  }

  return (criterion_value >= target) ? 1 : 0;
}


// =====================================================================
// 2. varsel_fwd_cvexpC
// Forward selection using exact cspca_varsel eigenvalue for both
// ranking and stopping.
//
// S is full cov matrix and M product of deflated G matrices M = G*G
//
// The first variable is chosen simply by max Rayleigh coeff  (cvexp card 1)

// if power method, the initial 30 + 3*(component order base 1) 
// done with full decomposition  because deflated matrix becomes more near singular
// =====================================================================
static int varsel_fwd_cvexpC(const Eigen::Ref<const Eigen::MatrixXd>& S,
                             const VectorXd& si,
                             MatrixXd& M,
                             double target_cvexp,
                             double prev_cvexp,
                             double alpha,
                             int mincard,
                             double rank_tol,
                             int comp_number,
                             bool PMSPC,
                             double epsPMSPC,
                             int maxiterPMSPC,
                             VectorXi& indices,
                             int& cardinality,
                             double& cur_cvexp,
                             VectorXd& loadings_out,
                             double& vexp_out)
{
  const int p = S.cols();
  const double target = alpha * target_cvexp;
  cur_cvexp = prev_cvexp;
  
  VectorXi var_status = VectorXi::Constant(p, -2);
  

  // Step 0: pick first variable by max si(j)^2 / S(j, j)
  int best0 = -1;
  double best0_val = -1.0;
  for (int j = 0; j < p; j++) {
    if (S(j, j) > rank_tol) {
      double val = si(j) * si(j) / S(j, j);
      if (val > best0_val) {
        best0_val = val;
        best0 = j;
      }
    }
  }
  
  if (best0 < 0) {
    Rcpp::warning("varsel_fwd_cvexpC: no admissible variables");
    cardinality = 0;
    return 0;
  }
  
  indices(0) = best0;
  var_status(best0) = 0;
  cardinality = 1;
  
  // For a single variable generalized eigensolver is needed:
  // eigval = M(i, i) / S(i, i), loading = 1
  double eigtmp = M(best0, best0) / S(best0, best0);
  if (!std::isfinite(eigtmp) || eigtmp <= 0.0) {
    Rcpp::warning("varsel_fwd_cvexpC: invalid initial single-variable cvexp");
    cardinality = 0;
    return 0;
  }
  
  cur_cvexp = prev_cvexp + eigtmp;
  vexp_out = eigtmp;
  loadings_out.setZero();
  loadings_out(0) = 1.0;
  
  VectorXd loadings_tmp(p);

  

  if (cur_cvexp >= target && cardinality >= mincard)
    return 1;

  bool PMSPC_tmp = PMSPC;  
  if(PMSPC){
    maxiterPMSPC = maxiterPMSPC + comp_number * 50;
  }
  
  // Steps 1..p: greedy on cspca eigval
  bool reached = false;
  while (!reached) {
    // define switching for power method. first 30++  geneigen computations always full 
    if(PMSPC){
      PMSPC_tmp = (cardinality >  27 + 3 * (comp_number + 1)) ;
    }
    
    int best_j = -1;
    double best_eigval = -1.0;
    VectorXd best_a = VectorXd::Zero(p);

    // try adding each available variable
    for (int j = 0; j < p; j++) {
      if (var_status(j) != -2)
        continue;
// FIX ind_tmp.head(cardinality) = indices.head(cardinality) and remove for(int....){...}
      VectorXi ind_tmp(cardinality + 1);
      for (int h = 0; h < cardinality; h++)
        ind_tmp(h) = indices(h);
      ind_tmp(cardinality) = j;
      
      
      double eigtmp = 0.0;
      VectorXd a_trial = VectorXd::Zero(p);
      bool singular_trial = false;
      cspca_varsel(S, M, eigtmp,
                   ind_tmp, cardinality + 1, a_trial, singular_trial,
                   PMSPC, epsPMSPC, maxiterPMSPC);
      if (singular_trial)
        continue;

      if (eigtmp > best_eigval) {
        best_eigval = eigtmp;
        best_j = j;
        best_a = a_trial;
      }
    }

    if (best_j < 0)
      break;

    indices(cardinality) = best_j;
    var_status(best_j) = 0;
    cardinality++;

    cur_cvexp = prev_cvexp + best_eigval;
    vexp_out = best_eigval;
    loadings_out.setZero();
    loadings_out.head(cardinality) = best_a.head(cardinality);

    if (cur_cvexp >= target && cardinality >= mincard)
      reached = true;

    if (cardinality >= p)
      break;
  }

  if (!reached)
    Rcpp::warning("varsel_fwd_cvexpC: target not reached");

  return reached ? 1 : 0;
}



// =====================================================================
// DISPATCHER: varsel_fbsC
//
// Routes to the appropriate helper based on stop_criterion and intensive.
//
// stop_criterion=0                  -> varsel_fbs_r2C
// stop_criterion=1, intensive=false -> varsel_fbs_r2C
// stop_criterion=1, intensive=true,
//   selection_method=0               -> varsel_fwd_cvexpC
// =====================================================================
int varsel_fbsC(const Eigen::Ref<const Eigen::MatrixXd>& S,
                     const VectorXd& si,
                     VectorXi& indices,
                     int& cardinality,
                     double& criterion_value,
                     VectorXd& loadings_out,
                     double& vexp_out,
                     double alpha,
                     int selection_method,
                     int stop_criterion,
                     bool intensive,
                     bool exact_cvexp,
                     double pc_vexp,
                     double target_cvexp,
                     int mincard,
                     MatrixXd& M,
                     double prev_cvexp,
                     int comp_number,
                     MatrixXd& B,
                     int ntrim,
                     int reducetrim,
                     double rank_tol,
                     bool PMSPC, // false
                     double epsPMSPC,//1e-5
                     int maxiterPMSPC) //100
{
  const int p = S.cols();
  (void) ntrim;
  (void) reducetrim;

  indices.resize(p);
  cardinality = 0;

  if (stop_criterion == 0 || !intensive) {
    return varsel_fbs_r2C(S, si, pc_vexp, alpha, selection_method,
                          rank_tol, mincard, stop_criterion,
                          M, target_cvexp, prev_cvexp, exact_cvexp, comp_number, B,
                          PMSPC, epsPMSPC, maxiterPMSPC,
                          indices, cardinality, criterion_value, loadings_out, vexp_out);
  }

  if (selection_method == 0) {
    return varsel_fwd_cvexpC(S, si, M, target_cvexp, prev_cvexp, alpha,
                             mincard, rank_tol, comp_number,
                             PMSPC, epsPMSPC, maxiterPMSPC,
                             indices, cardinality, criterion_value, loadings_out, vexp_out);
  }

  throw std::invalid_argument("intensive = TRUE is only available for selection_method = 0 (forward)");
}

