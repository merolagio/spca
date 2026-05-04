#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <algorithm>
#include <cmath>

#include "support_shared.h"
#include "support_fat.h"

using namespace Rcpp;
using namespace Eigen;
using namespace std;

// [[Rcpp::depends(RcppEigen)]]

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::SelfAdjointEigenSolver;
using Eigen::GeneralizedSelfAdjointEigenSolver;

// deflT_rank1
//
// Computed via rank-1 Woodbury update (O(n^2) instead of O(n^3)):
// Kx = K * x
// K -= (Kx x' + x Kx') / (x'x) - x (x'Kx) x' / (x'x)^2
// P = I_n - x x' / (x'x)
//   K <- P K P
//   vexp = trace_before - trace_after
//
// @param x vector of loadings size p
// @param K covariance matrix, typically already deflated of previous components
// @param vexp scalar (output)
// 
// @returns 
// K (by reference) the deflated matrix
// vexp the variance expalined by the component

void deflT_rank1(const Eigen::Ref<const Eigen::VectorXd>& x,
                        Eigen::MatrixXd& K,
                        double& vexp)
{
  double traceK = K.trace();
  double xnorm2 = x.squaredNorm();
  
  Eigen::VectorXd Kx = K * x;
  double xKx = x.dot(Kx);
  
  // K -= (Kx x' + x Kx') / (x'x) - x (x'Kx) x' / (x'x)^2
  K.noalias() -= (Kx * x.transpose() + x * Kx.transpose()) / xnorm2;
  K.noalias() += (xKx / (xnorm2 * xnorm2)) * (x * x.transpose());
  
  vexp = traceK - K.trace();
}


// =============================================================================
// deflT_F
//
// Deflates a cross-product matrix K = Q Q' by the score matrix PC.
//
// Input:
//   PC      - n x m matrix of score vectors.
//   K       - n x n cross-product matrix, Q Q', modified in place.
//   curvexp - trace(K) before deflation.
//   vexp    - output variance explained, curvexp - trace(K) after deflation.
//
// What it does:
//   Forms the orthogonal projector
//     P = I - PC (PC' PC)^(-1) PC',
//   and updates
//     K <- P K P.
//
// How it is used:
//   - with one score column, it deflates the Q Q' matrix of the new
//     component extracted;
//   - with several score columns, it computes exact cumulative variance
//     explained.
// =============================================================================
void deflT_F(Eigen::MatrixXd PC, Eigen::MatrixXd& K, double curvexp, double& vexp){
  // # pass only a nonzero loads
  // T = Q %*% Q'
  // #  K <-- (I - Xdaa'X'/(a'Xd'Xd a) * K// orth proj //deflated S matrix
  // ## ===
  const int n = K.cols();
  
  Eigen::MatrixXd P = MatrixXd::Identity(n, n) - 
    PC * (PC.transpose() * PC).inverse()  * PC.transpose();
  
  K = P * K * P;
  vexp = curvexp - K.trace();
  
  // deflate D
  
  return;// List::create(Named("Tdefl") = K, Named("vexp") = vexp);
}  

 void cspcaTC(const Eigen::Ref<const Eigen::MatrixXd>& X,
                    const Eigen::Ref<const Eigen::MatrixXd>& K,
                    double& eigval,
                    const Eigen::VectorXi& indj,
                    int cardt,
                    Eigen::VectorXd& a,
                    Eigen::MatrixXd& scores,
                    int comp_number,
                    bool PM,
                    double epsPM, //1e-5
                    int maxiter) //100
 {
   const int n = X.rows();
   
   Eigen::MatrixXd Xd(n, cardt);
   VectorXi indt = indj.head(cardt);
   makeXdC(X, indt, Xd);
   
   Eigen::MatrixXd Sd = Xd.transpose() * Xd;
   Eigen::MatrixXd P = Xd.transpose() * K * Xd;
   
   Eigen::VectorXd vec(cardt);
   if (PM) {
     vec = GeigvecPMC(P, Sd, eigval, epsPM, maxiter);
   } else {
     GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> es(P, Sd);
     vec = es.eigenvectors().col(cardt - 1);
     eigval = es.eigenvalues()(cardt - 1);
   }
   
   a.head(cardt) = vec.array() / vec.norm();
   scores.col(comp_number) = Xd * a.head(cardt);
 }

//====================================
// uncorrelated components
//====================================
// computes CSPCA for fat matrices    

// extra input
// B is marix of previous components XA[, j-1]
 void uspcaTC(const Eigen::Ref<const Eigen::MatrixXd>& X,
              const Eigen::Ref<const Eigen::MatrixXd>& T,
              double& eigval,
              const Eigen::VectorXi& indj,
              int cardt,
              Eigen::VectorXd& a,
              Eigen::MatrixXd& scores,
              int comp_number,
              bool PM,  //false,
              double epsPM, // 1e-5,
              int maxiter) // 100
  {
  
  if (cardt < comp_number + 1){
    Rcout << "uspca, component " << comp_number + 1 << endl;
    throw Rcpp::exception(" must pass cardt > comp_number + 1");
  }
  int n = X.rows(); //, p = X.cols();
  //Rcout << "uspca\n indj  " << indj.head(cardt).transpose()   << endl; 
  //Rcout << "uspca\n X[1:5,]\n" << X.topRows(5)   << endl; 
  
  // makes Xd, Sd and Sp      
  Eigen::MatrixXd Xd(n, cardt);
  makeXdC(X, indj.head(cardt), Xd);
  //Rcout << "uspca\nXd[1:5, 1:5]\n" << Xd.topLeftCorner(5, cardt)   << endl; 
  
  
  Eigen::MatrixXd Sd =  Xd.transpose() * Xd;
  Eigen::MatrixXd Sp;
  
  
  
  // Rcout << "Sp *********************" << endl;
  //   Rcout << Sp << endl;
  // Rcout << "done Sp" << endl; 
  
  Eigen::MatrixXd R(cardt, comp_number); // Xd' scores[, 1:comp_number]
  Eigen::MatrixXd K(cardt, comp_number);
  Eigen::MatrixXd M(comp_number, comp_number);
  Eigen::MatrixXd Mp(comp_number, comp_number);
  Eigen::MatrixXd C(cardt, cardt);
  Eigen::MatrixXd P(cardt, cardt);
  Eigen::MatrixXd Q(cardt, cardt);
  
  Eigen::VectorXd vec(cardt);
  // computes constraints
  if (comp_number > 0){
    // R = Xd' XA_[comp_number - 1, p]  (cardt, comp_number)
    Sp = Sd.inverse();
    R = Xd.transpose() * scores.leftCols(comp_number); 
    // K  = SD^-1 Xd' XA_[comp_number-i, p]  (cardt, comp_number)
    K = Sp * R;
    //  M = A'X' Xd Sp Xd' XA = R' K
    M = R.transpose() * K; //(comp_number, comp_number);
    
    // Now if comp_number == 1,  C = I - (R * K.transpose()).array()/M(0,0)
    // if comp_number > 1 Mp = M.solve() => C = I - R * Mp *K.transpose()
    // Mp = M^-1 different for second component because M^-1 is scalar    
    //  I - (R * K.transpose()).array()/M(0,0)
    if (comp_number == 1){  
      C = Eigen::MatrixXd::Identity(cardt, cardt) - 
        ((R * K.transpose()).array()/M(0,0)).matrix();
    } //end comp_number == 1   
    else{
      //C = I - R'M-1 R Sd-1 = I - M K'
      C = Eigen::MatrixXd::Identity(cardt, cardt) - 
        R * M.topLeftCorner(comp_number, comp_number).inverse() * K.transpose(); // (cardt, cardt) - (cardt, cardt)
    }// end comp_number> 1 
    
    P =  C * Xd.transpose() * T * Xd * C.transpose();    
  }// end if comp_number > 0 
  else{
    P =   Xd.transpose() * T * Xd;  
  }
  
  
  //  compute loadings        
  if (PM){
    vec =  GeigvecPMC(P, Sd, eigval, epsPM, maxiter);
  }
  else{
    GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> es(P, Sd);
    vec = es.eigenvectors().col(cardt - 1);    
    eigval = es.eigenvalues()(cardt - 1);
  }
  a.head(cardt) = vec.array()/vec.norm();
  
  //  Rcout << "uspca, loadings " << a.head(cardt).transpose() << endl;
  
  scores.col(comp_number) = Xd * a.head(cardt);
  return;
}

// projected components
// inputs 
// original data matrix
// r current PC to fit
// others are output as above 
void pspcaTC(const Eigen::Ref<const Eigen::MatrixXd>& X,
             const Eigen::VectorXd& r,
             const Eigen::VectorXi& indj,
             int cardt,
             Eigen::VectorXd& a,
             Eigen::MatrixXd& scores,
             int comp_number){
  int n = X.rows(); 
  // makes Xd, Sd
  Eigen::MatrixXd Xd(n, cardt);
  makeXdC(X, indj.head(cardt), Xd);

  Eigen::MatrixXd Sd =  Xd.transpose() * Xd;
  Eigen::MatrixXd Sp = Sd.inverse();
  
  Eigen::VectorXd vec(cardt);
  
  // computes loadings
    vec =   Sp * Xd.transpose() * r;  
  a.head(cardt) = vec.array()/vec.norm();
  
  scores.col(comp_number) = Xd * a.head(cardt);
  return;
}

double compute_r2_subset_T(const Eigen::Ref<const Eigen::MatrixXd>& X,
                                  const VectorXd& u,
                                  const VectorXi& indj,
                                  int cardt)
{
  if (cardt <= 0) return 0.0;

  MatrixXd Xd(X.rows(), cardt);
  VectorXi indt = indj.head(cardt);
  makeXdC(X, indt, Xd);

  MatrixXd XtX = Xd.transpose() * Xd;
  VectorXd Xtu = Xd.transpose() * u;
  VectorXd beta(cardt);

  LLT<MatrixXd> llt(XtX);
  if (llt.info() == Eigen::Success) beta = llt.solve(Xtu);
  else beta = LDLT<MatrixXd>(XtX).solve(Xtu);

  // u is unit norm so ||u||^2 = 1; no division needed
  double r2 = u.dot(Xd * beta);
  if (r2 < 0.0) r2 = 0.0;
  if (r2 > 1.0) r2 = 1.0;
  return r2;
}

double compute_cvexp_T(const MatrixXd& scores_all,
                              int ncomps,
                              const MatrixXd& D_orig,
                              double totvexp)
{
  double cvexp_val = 0.0;
  MatrixXd D_work = D_orig;
  deflT_F(scores_all.leftCols(ncomps), D_work, totvexp, cvexp_val);
  return cvexp_val;
}

void compute_vexp_cvexp_T_exact(const MatrixXd& scores,
                                       int nc,
                                       const MatrixXd& D_orig,
                                       double totvexp,
                                       VectorXd& vexp,
                                       VectorXd& cvexp)
{
  vexp = VectorXd::Zero(nc);
  cvexp = VectorXd::Zero(nc);

  for (int j = 0; j < nc; j++) {
    MatrixXd Dtmp = D_orig;
    double cv = 0.0;
    deflT_F(scores.leftCols(j + 1), Dtmp, totvexp, cv);
    cvexp(j) = cv;
    vexp(j) = (j == 0) ? cv : (cv - cvexp(j - 1));
  }
}

// cspcaTQ
//
// Correlated LS-SPCA eigensolve for fat matrices.
// Same math as cspcaTC but:
//   - does not write scores
//   - does not take comp_number
//
// Used when only eigval and loadings are needed during variable selection.
// =============================================================================
static void cspcaTQ(const Eigen::Ref<const Eigen::MatrixXd>& X,
             const MatrixXd& K,
             double& eigval,
             const VectorXi& indj,
             int cardt,
             Eigen::VectorXd& a,
             bool PM,
             double epsPM, //1e-5,
             int maxiter) // 100)
{
  const int n = X.rows();

  Eigen::MatrixXd Xd(n, cardt);
  VectorXi indt = indj.head(cardt);
  makeXdC(X, indt, Xd);

  Eigen::MatrixXd Sd = Xd.transpose() * Xd;
  Eigen::MatrixXd P = Xd.transpose() * K * Xd;

  Eigen::VectorXd vec(cardt);
  if (PM) {
    vec = GeigvecPMC(P, Sd, eigval, epsPM, maxiter);
  } else {
    GeneralizedSelfAdjointEigenSolver<Eigen::MatrixXd> es(P, Sd);
    vec = es.eigenvectors().col(cardt - 1);
    eigval = es.eigenvalues()(cardt - 1);
  }

  a.head(cardt) = vec.array() / vec.norm();
}

// =============================================================================
// fwd_selectT
//
// Forward variable selection for fat matrices (p > n).
//
// Let
//   Q_j    = X deflated by the first j - 1 selected components,
//   D      = Q_j Q_j'   = current deflated row-space matrix,
//   D_orig = X X'       = original row-space matrix,
//   u      = leading unit-norm eigenvector of D.
//
// The inputs X, D, and D_orig are not modified. The function creates local
// working copies only when they are needed.
//
// HOW SELECTION IS DONE
//   The function always performs forward selection and adds one variable at a
//   time to the current subset.
//
//   stop_rule = 0  ->  r2
//     Screening is based on partial r2. The function creates
//       Xw = working residualized copy of X,
//       uw = working residualized copy of u.
//     After each selected variable, the same one-variable projection is
//     removed from uw and from the remaining columns of Xw. For a candidate
//     variable xw_j, the screening value is
//       (uw' xw_j)^2 / (||u||^2 xw_j' xw_j).
//     The stopping rule is checked from the residual norm:
//       r2 = 1 - ||uw||^2 / ||u||^2.
//
//   stop_rule = 1  ->  cvexp
//     Screening is based on one-variable Rayleigh quotients. The function
//     creates a working copy
//       K_rank = D,
//     and deflates it by the variables selected so far. For a candidate
//     variable x_j, the screening value is
//       x_j' K_rank x_j / (x_j' x_j).
//     After each new variable is added, the current subset is evaluated on D.
//     If exact cvexp is needed for the stopping check, cspcaTC() is used so the
//     score column is available; otherwise cspcaTQ() is used to avoid writing
//     scores during the search.
//
// RETURNS
//   false if the target was reached.
//   true  if the search stopped without reaching the target.
//   @noRd
// =============================================================================
bool fwd_selectT(const Eigen::Ref<const Eigen::MatrixXd>& X,
                  const MatrixXd& D,
                  const MatrixXd& D_orig,
                  double totvexp,
                  const VectorXd& u,
                  double oldcvexp,
                  double maxcvexp,
                  double alpha,
                  int mincard,
                  int stop_rule,
                  Eigen::VectorXd& a,
                  Eigen::VectorXi& ind,
                  int& cardt,
                  double& vexp,
                  double& cvexp,
                  double& r2,
                  Eigen::MatrixXd& scores,
                  int comp_number,
                  bool exact_cvexp,
                  bool PM,
                  double epsPM, // 1e-5,
                  int maxiterPM) //100
{
  const int p = X.cols();

  if (stop_rule != 0 && stop_rule != 1)
    Rcpp::stop("fwd_selectT: stop_rule must be 0 (r2) or 1 (cvexp)");

  Eigen::VectorXi indnot = VectorXi::Constant(p, -2);
  double eigval = 0.0;
  double cur_cvexp = oldcvexp;
  double cur_r2 = 0.0;

  Eigen::MatrixXd K_rank;
  if (stop_rule == 1)
    K_rank = D;

  Eigen::MatrixXd Xw;
  Eigen::VectorXd uw;
  double u_norm2_0 = 0.0;
  if (stop_rule == 0) {
    Xw = X;
    uw = u;
    u_norm2_0 = u.squaredNorm();
    if (u_norm2_0 <= 0.0)
      Rcpp::stop("fwd_selectT: input u must have positive norm for stop_rule = 0");
  }

  int indmax = -1;
  double best_val = -1.0;

  for (int j = 0; j < p; j++) {
    double nor = X.col(j).squaredNorm();
    if (nor <= 0.0) {
      indnot(j) = -1;
      continue;
    }

    double val = 0.0;
    if (stop_rule == 0) {
      double uxj = uw.dot(Xw.col(j));
      val = (uxj * uxj) / (u_norm2_0 * nor);
    } else {
      double xKx = X.col(j).transpose() * K_rank * X.col(j);
      val = xKx / nor;
    }

    if (val > best_val) {
      best_val = val;
      indmax = j;
    }
  }

  if (indmax < 0 || best_val <= 0.0) {
    Rcpp::warning("fwd_selectT: no admissible variables");
    cardt = 0;
    vexp = 0.0;
    cvexp = oldcvexp;
    r2 = 0.0;
    return true;
  }

  ind(0) = indmax;
  indnot(indmax) = indmax;
  cardt = 1;

  if (stop_rule == 0) {
    const Eigen::VectorXd xj = Xw.col(indmax);
    const double norj = xj.squaredNorm();
    if (norj > 0.0) {
      const double uxj = uw.dot(xj);
      uw -= xj * (uxj / norj);
      for (int k = 0; k < p; k++) {
        if (indnot(k) != -2) continue;
        const double proj = Xw.col(k).dot(xj) / norj;
        Xw.col(k) -= xj * proj;
      }
    }
    cur_r2 = 1.0 - uw.squaredNorm() / u_norm2_0;
    if (cur_r2 < 0.0) cur_r2 = 0.0;
    if (cur_r2 > 1.0) cur_r2 = 1.0;
  }

  Eigen::VectorXi indSorted(1);
  indSorted(0) = ind(0);

  // Single-variable case: Rayleigh quotient directly, no eigensolve needed.
  // cspcaTC for cardt = 1 would give a(0) = 1, eigval = x'Dx / x'x, scores = x.
  a(0) = 1.0; 
  Eigen::VectorXd x0 = X.col(ind(0));
  scores.col(comp_number) = x0;
  double x0norm2 = x0.squaredNorm();
  eigval = (x0norm2 > 0.0) ? (x0.dot(D * x0) / x0norm2) : 0.0;

  vexp = eigval;
  if (exact_cvexp && comp_number > 0)
    cur_cvexp = compute_cvexp_T(scores, comp_number + 1, D_orig, totvexp);
  else
    cur_cvexp = oldcvexp + eigval;

  if ((stop_rule == 0 && cur_r2 >= alpha && mincard <= 1) ||
      (stop_rule == 1 && cur_cvexp >= alpha * maxcvexp && mincard <= 1)) {
    cvexp = cur_cvexp;
    r2 = (stop_rule == 0) ? cur_r2 : compute_r2_subset_T(X, u, indSorted, cardt);
    std::sort(ind.data(), ind.data() + cardt);
    return false;
  }

  bool stopSelect = false;
  while (!stopSelect) {

    if (stop_rule == 1) {
      double ignored_vexp = 0.0;
      deflT_rank1(X.col(ind(cardt - 1)), K_rank, ignored_vexp);
    }

    indmax = -1;
    best_val = -1.0;

    for (int j = 0; j < p; j++) {
      if (indnot(j) != -2) continue;

      if (stop_rule == 0) {
        double nor = Xw.col(j).squaredNorm();
        if (nor <= 0.0) {
          indnot(j) = -1;
          continue;
        }
        double uxj = uw.dot(Xw.col(j));
        double pr2 = (uxj * uxj) / (u_norm2_0 * nor);
        if (pr2 > best_val) {
          best_val = pr2;
          indmax = j;
        }
      } else {
        double nor = X.col(j).squaredNorm();
        if (nor <= 0.0) {
          indnot(j) = -1;
          continue;
        }
        double rayq = (X.col(j).transpose() * K_rank * X.col(j))(0, 0) / nor;
        if (rayq > best_val) {
          best_val = rayq;
          indmax = j;
        }
      }
    }

    if (indmax < 0 || best_val <= 0.0) {
      stopSelect = true;
      break;
    }

    ind(cardt) = indmax;
    indnot(indmax) = indmax;
    cardt++;

    if (stop_rule == 0) {
      const Eigen::VectorXd xj = Xw.col(indmax);
      const double norj = xj.squaredNorm();
      if (norj > 0.0) {
        const double uxj = uw.dot(xj);
        uw -= xj * (uxj / norj);
        for (int k = 0; k < p; k++) {
          if (indnot(k) != -2) continue;
          const double proj = Xw.col(k).dot(xj) / norj;
          Xw.col(k) -= xj * proj;
        }
      }

      cur_r2 = 1.0 - uw.squaredNorm() / u_norm2_0;
      if (cur_r2 < 0.0) cur_r2 = 0.0;
      if (cur_r2 > 1.0) cur_r2 = 1.0;

      if (cardt >= mincard && cur_r2 >= alpha)
        stopSelect = true;
      else if (cardt == p)
        stopSelect = true;

    } else {
      indSorted = ind.head(cardt);
      std::sort(indSorted.data(), indSorted.data() + cardt);
      if (exact_cvexp && comp_number > 0) {
        
        cspcaTC(X, D, eigval, indSorted, cardt, a, scores, comp_number,
               PM, epsPM, maxiterPM);
        cur_cvexp = compute_cvexp_T(scores, comp_number + 1, D_orig, totvexp);
        
      } else {
        cspcaTQ(X, D, eigval, indSorted, cardt, a,
                PM, epsPM, maxiterPM);
        cur_cvexp = oldcvexp + eigval;
      }

      vexp = eigval;

      if (cardt >= mincard && cur_cvexp >= alpha * maxcvexp)
        stopSelect = true;
      else if (cardt == p)
        stopSelect = true;
    }
  }

  indSorted = ind.head(cardt);
  std::sort(indSorted.data(), indSorted.data() + cardt);

  // Final loadings and scores are always computed with cspcaTC().
  // During forward cvexp selection, cspcaTQ() is used only when scores are not needed.
  cspcaTC(X, D, eigval, indSorted, cardt, a, scores, comp_number,
         PM, epsPM, maxiterPM);

  vexp = eigval;
  if (exact_cvexp && comp_number > 0)
    cur_cvexp = compute_cvexp_T(scores, comp_number + 1, D_orig, totvexp);
  else
    cur_cvexp = oldcvexp + eigval;

  cvexp = cur_cvexp;
  if (stop_rule == 0)
    r2 = cur_r2;
  else
    r2 = compute_r2_subset_T(X, u, indSorted, cardt);

  std::sort(ind.data(), ind.data() + cardt);
  return (stop_rule == 0) ? (r2 < alpha) : (cur_cvexp < alpha * maxcvexp);
}

// Variable selection for fat matrices — R wrapper for testing.
//
// Tests forward T-based variable selection on the first component. When
// stop_rule = 0, screening is based on partial \eqn{r^2} and the target is
// checked via \eqn{\|u_w\|^2 \le 1 - \alpha}. When stop_rule = 1, cvexp is
// checked after each forward step. The T-functions use the row-space /
// inverse-SVD approach based on the PC of
// \eqn{X_{\mathrm{def}} X_{\mathrm{def}}'} (\eqn{n \times n}), which is more
// efficient than working with \eqn{X_{\mathrm{def}}' X_{\mathrm{def}}}
// (\eqn{p \times p}) when \eqn{p > n}.
//
// @param X Data matrix (\eqn{n \times p}), centered, with \eqn{n < p}.
// @param alpha Target proportion of PC variance (default 0.95).
// @param mincard Minimum cardinality (default 1).
// @param varsel_method Variable-selection method. For fat matrices only
//   \code{0} (forward) is supported.
// @param stop_rule Stopping rule: \code{0} = \eqn{r^2},
//   \code{1} = CVEXP (default 1).
// @param exact_cvexp If TRUE, compute exact cumulative variance (default FALSE).
// @param PM Use power method (default TRUE).
// @param epsPM Power-method tolerance (default 1e-5).
// @param maxiterPM Power-method max iterations (default 150).
// @return A list with selected indices, cardinality, loadings, vexp, cvexp,
//   and r2.
