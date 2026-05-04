
// spca_Fat_main Forward variable selection only LS-SPCA for fat matrices (p > n).
//
// The T-functions work in row space, using the leading PC of 
// Xdef Xdef' (n x n), rather than
// eigendecomposing Xdef' Xdef (p x p). 
// This "reverse svd" approach is more efficient and lighter in
// RAM when p >> n.
//
// Dependencies: Support_spca
// =============================================================================

// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <set>
#include <algorithm>
#include <cmath>
#include <string>
#include <Rcpp/Benchmark/Timer.h>
#include <chrono>
#include <ctime>

#include "support_shared.h"
#include "support_fat.h"

using namespace Rcpp;
using namespace Eigen;
using namespace std;

using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::VectorXi;
using Eigen::SelfAdjointEigenSolver;
using Rcpp::List;
using std::string;


static inline long long wall_now_ns()
{
  return std::chrono::duration_cast<std::chrono::nanoseconds>(
    std::chrono::steady_clock::now().time_since_epoch()).count();
}

static inline double cpu_now_ns()
{
  return 1e9 * static_cast<double>(std::clock()) / static_cast<double>(CLOCKS_PER_SEC);
}


// =====================================================================
// HELPERS
// =====================================================================

static void validate_index_vector(const VectorXi& x,
                                  const std::string& x_name,
                                  int p)
{
  std::set<int> seen;
  for (int i = 0; i < x.size(); i++) {
    if (x(i) < 0 || x(i) >= p)
      Rcpp::stop("%s: index %d is out of range [0, %d)", x_name.c_str(), x(i), p);
    if (!seen.insert(x(i)).second)
      Rcpp::stop("%s: index %d appears more than once", x_name.c_str(), x(i));
  }
}

static void validate_lsspcaT_inputs(const Eigen::Ref<const Eigen::MatrixXd>& X,
                                    int n,
                                    int p,
                                    int stop_criterion,
                                    bool exact_cvexp,
                                    double alpha,
                                    double ncompbycvexp,
                                    double rank_tol,
                                    const VectorXi& forcein,
                                    const VectorXi& forceout)
{
  if (X.rows() == 0 || X.cols() == 0)
    Rcpp::stop("X must have at least one row and one column");
  if (p <= n)
    Rcpp::stop("lsspcaTC is intended for fat matrices with p > n");
  if (stop_criterion < 0 || stop_criterion > 1)
    Rcpp::stop("stop_criterion must be 0 (r2) or 1 (cvexp)");
  if (alpha <= 0.0 || alpha > 1.0)
    Rcpp::stop("alpha must be in (0, 1]");
  if (ncompbycvexp <= 0.0 || ncompbycvexp > 1.0)
    Rcpp::stop("ncompbycvexp must be in (0, 1]");
  if (rank_tol < 0.0)
    Rcpp::stop("rank_tol must be >= 0");

  (void)exact_cvexp;
  validate_index_vector(forcein, "force_in", p);
  validate_index_vector(forceout, "force_out", p);
}

// @title Least-Squares Sparse Principal Component Analysis for Fat Matrices
//
// @description Computes sparse principal components for fat matrices
//   (\eqn{p > n}) using row-space / inverse-SVD algorithms. In contrast to
//   the tall-matrix wrapper, which works with \eqn{X_{\mathrm{def}}'
//   X_{\mathrm{def}}} (\eqn{p \times p}), this wrapper works with the PC of
//   \eqn{X_{\mathrm{def}} X_{\mathrm{def}}'} (\eqn{n \times n}), which is
//   more efficient and uses less memory when \eqn{p \gg n}. Fat-matrix
//   variable selection is forward only.
//
// @param X Numeric \eqn{n \times p} centered data matrix with \eqn{p > n}.
// @param ncomps Integer. Maximum number of components (default 0). If 0, the
//   number of components is determined by \code{ncompbycvexp}.
// @param stop_criterion Integer. Stopping rule for variable selection:
//   \code{0} = \eqn{R^2}, \code{1} = cumulative variance explained (CVEXP).
// @param exact_cvexp Logical. If TRUE, exact cumulative explained variance is
//   used during CVEXP-based selection. In forward cvexp mode, the current
//   subset cvexp is checked after each step. Returned \code{vexp} and
//   \code{cvexp} are recomputed exactly from the final score matrix before
//   return.
// @param alpha Numeric in (0, 1]. Target retained proportion during variable
//   selection (default 0.95).
// @param ncompbycvexp Numeric in (0, 1]. Target cumulative proportion of total
//   variance for automatic component stopping (default 0.95).
// @param method Character vector. SPCA type per component: \code{"c"},
//   \code{"u"}, or \code{"p"}. Recycled if shorter than \code{ncomps}.
// @param force_in Nullable integer vector. Accepted for interface consistency
//   with \code{lsspcaCVC}; currently ignored for T-based forward selection.
// @param force_out Nullable integer vector. Accepted for interface consistency
//   with \code{lsspcaCVC}; currently ignored for T-based forward selection.
// @param indvec_in Nullable integer vector. 0-based variable indices for fixed
//   components (unlisted). Use with \code{cardvec_in}.
// @param cardvec_in Nullable integer vector. Cardinalities for each fixed
//   component. Bypasses variable selection.
// @param PMPC Logical. Use the power method for PCs of the row-space matrix
//   \eqn{X_{\mathrm{def}} X_{\mathrm{def}}'} (default FALSE).
// @param PMS Logical. Use the power method in variable selection 
// (default FALSE).
// @param epsPMPC Numeric. Convergence tolerance for the PC power method.
// @param epsPMS Numeric. Convergence tolerance for the power method 
// in variable selection. 
// @param maxiterPMPC Integer. Max iterations for the power method 
//   in variable selection (default 1000). 
//   For component \eqn{j > 0} the effective limit is
//   \code{maxiterPMPC + j * 50} to handle slower convergence when the
//   deflated matrix is more likely to have nearly equal leading eigenvalues.
// @param maxiterPMS Integer. Max iterations for the sparse-component 
// power method.
// @param rank_tol Numeric. Accepted for interface consistency; currently not
//   used by the T-based selectors.
//
// @return A list with components:
//   \describe{
//     \item{loadings}{Numeric matrix (p x ncomps). Sparse loading vectors.}
//     \item{loadlist}{List. Nonzero loadings per component.}
//     \item{ncomps}{Integer. Number of components computed.}
//     \item{ind}{List. Selected variable indices (1-based) per component.}
//     \item{card}{Integer vector. Cardinality per component.}
//     \item{vexp}{Numeric vector. Variance explained per component.}
//     \item{cvexp}{Numeric vector. Cumulative variance explained.}
//     \item{vexpPC} Numeric vector. Variance explained per principal
//      component.
//     \item{r2}{Numeric vector with the squared correlations PCs, sPCs.}
//     \item{scores}{A matrix with components scores.}
//     \item{method}{Character vector. Actual method used per component.}
//     \item{varSelection}{Character. Description of the selection method used.}
// }
// @details This functionis called from the R wrapper 
// @noRd
// [[Rcpp::export]]
List lsspcaTC(const Eigen::Map<Eigen::MatrixXd>& X,
              int ncomps = 0,
              int stop_criterion = 0,
              bool exact_cvexp = false,
              double alpha = 0.95,
              double ncompbycvexp = 0.95,
              Rcpp::CharacterVector method = Rcpp::CharacterVector::create("c"),
              Rcpp::Nullable<Rcpp::IntegerVector> force_in  = R_NilValue,
              Rcpp::Nullable<Rcpp::IntegerVector> force_out = R_NilValue,
              Rcpp::Nullable<Rcpp::IntegerVector> indvec_in  = R_NilValue,
              Rcpp::Nullable<Rcpp::IntegerVector> cardvec_in = R_NilValue,
              bool PMPC = false, bool PMS = false,
              double epsPMPC = 1E-5, double epsPMS = 1E-7,
              int maxiterPMPC = 300, int maxiterPMS = 200,
              double rank_tol = 0.0)
{
  const int n = X.rows();
  const int p = X.cols();

  const long long t_setup0_wall = wall_now_ns();
  const double t_setup0_cpu = cpu_now_ns();

  VectorXi forcein(0), forceout(0);
  if (force_in.isNotNull()) forcein = Rcpp::as<VectorXi>(force_in.get());
  if (force_out.isNotNull()) forceout = Rcpp::as<VectorXi>(force_out.get());

  validate_lsspcaT_inputs(X, n, p, stop_criterion,
                          exact_cvexp, alpha, ncompbycvexp, rank_tol,
                          forcein, forceout);

  if ((forcein.size() > 0 || forceout.size() > 0) &&
      !(indvec_in.isNotNull() && cardvec_in.isNotNull()))
    Rcpp::warning("lsspcaTC: force_in and force_out are currently ignored for T-based forward selection");

  if (ncomps == 0 && ncompbycvexp > 0.0 && ncompbycvexp < 1.0)
    ncomps = n;
  else if (ncomps <= 0 || ncomps > n)
    Rcpp::stop("ncomps must be between 1 and n for fat matrices");
  else
    ncompbycvexp = 1.0;

  bool fixedind = false;
  int startind = 0;
  VectorXi indvec(0), cardvec(0);
  if (indvec_in.isNotNull() && cardvec_in.isNotNull()) {
    indvec = Rcpp::as<VectorXi>(indvec_in.get());
    cardvec = Rcpp::as<VectorXi>(cardvec_in.get());
    fixedind = true;
    if (indvec.size() != cardvec.sum())
      Rcpp::stop("length of indvec_in must equal sum(cardvec_in)");
  }

  if (method.size() < ncomps) {
    const int oldsize = method.size();
    Rcpp::CharacterVector newmethod(ncomps);
    for (int i = 0; i < oldsize; i++) newmethod[i] = method[i];
    for (int i = oldsize; i < ncomps; i++) newmethod[i] = method[oldsize - 1];
    method = newmethod;
  }

  VectorXi mincard_vec(ncomps);
  for (int i = 0; i < ncomps; i++) {
    string mi = Rcpp::as<string>(method[i]);
    if (mi != "u" && mi != "c" && mi != "p")
      Rcpp::stop("method entries must be one of 'u', 'c', or 'p'");
    mincard_vec(i) = (mi == "u") ? (i + 1) : 1;
    if (fixedind && mi == "u" && ((cardvec(i) < i + 1) && (cardvec(i) > 0)))
      Rcpp::stop("fixed cardinality for uSPCA cannot be smaller than the component order");
  }

  // expensive O(n^2p) done just once 
  MatrixXd D_orig = X * X.transpose();
  MatrixXd D = D_orig;
  MatrixXd scores = MatrixXd::Zero(n, ncomps);
  MatrixXd A = MatrixXd::Zero(p, ncomps);
  List indout(ncomps), loadlist(ncomps);
  VectorXi card = VectorXi::Zero(ncomps);
  VectorXd r2vec = VectorXd::Zero(ncomps);

  MatrixXd TimeWall = MatrixXd::Zero(ncomps, 4);
  MatrixXd TimeCPU = MatrixXd::Zero(ncomps, 4);

  VectorXd PCvexp(n);
  double totvexp = D_orig.trace();
  double target_cvexp_j = 0.0;

  VectorXd a(p), r_scaled(n), u(n);
  VectorXi indj(p);
  int cardt = 0, nc = 0;
  bool stopComp = false;

  const long long setup_wall_ns = wall_now_ns() - t_setup0_wall;
  const double setup_cpu_ns = cpu_now_ns() - t_setup0_cpu;

  try {
    int j = 0;
    while (!stopComp) {
      const long long t_phase0_wall = wall_now_ns();
      const double t_phase0_cpu = cpu_now_ns();

      double maxvexp = 0.0;

      if (!PMPC) {
        SelfAdjointEigenSolver<MatrixXd> es(D);
        maxvexp = es.eigenvalues()(n - 1);
        u = es.eigenvectors().col(n - 1);
        r_scaled = u.array() * std::sqrt(maxvexp);
        if (j == 0) {
          PCvexp = es.eigenvalues().reverse();
          totvexp = PCvexp.sum();
        }
      } else {
        if (j == 0) {
          SelfAdjointEigenSolver<MatrixXd> es(D);
          maxvexp = es.eigenvalues()(n - 1);
          u = es.eigenvectors().col(n - 1);
          PCvexp = es.eigenvalues().reverse();
          totvexp = PCvexp.sum();
          r_scaled = u.array() * std::sqrt(maxvexp);
        } else {
          int maxiterPMPC_j = maxiterPMPC + j * 50;
          r_scaled = eigvecPMC(D, maxvexp, epsPMPC, maxiterPMPC_j);
          if (maxvexp > 0.0) u = r_scaled / std::sqrt(maxvexp);
          else Rcpp::stop("lsspcaTC: non-positive leading eigenvalue in row-space PC");
        }
      }

      target_cvexp_j += PCvexp(j);

      double prev_cvexp_j = 0.0;
      if (j > 0) {
        VectorXd vtmp(j), cvtmp(j);
        compute_vexp_cvexp_T_exact(scores.leftCols(j), j, D_orig, totvexp, vtmp, cvtmp);
        prev_cvexp_j = cvtmp(j - 1);
      }

      TimeWall(j, 0) = static_cast<double>(wall_now_ns() - t_phase0_wall);
      TimeCPU(j, 0) = cpu_now_ns() - t_phase0_cpu;

      const long long t_phase1_wall = wall_now_ns();
      const double t_phase1_cpu = cpu_now_ns();

      double vexp_sel = 0.0, cvexp_sel = prev_cvexp_j, r2_sel = 0.0;
      bool sel_fail = false;

      if (fixedind && cardvec(j) > 0) {
        indj.head(cardvec(j)) = indvec.segment(startind, cardvec(j));
        cardt = cardvec(j);
        startind += cardt;
        std::sort(indj.data(), indj.data() + cardt);
      } else {
        sel_fail = fwd_selectT(X, D, D_orig, totvexp, u,
                               prev_cvexp_j, target_cvexp_j, alpha,
                               mincard_vec(j), stop_criterion,
                               a, indj, cardt,
                               vexp_sel, cvexp_sel, r2_sel,
                               scores, j, exact_cvexp,
                               PMS, epsPMS, maxiterPMS);
        if (sel_fail)
          Rcpp::warning(("comp " + std::to_string(j + 1) + ": selection target not reached").c_str());
      }

      TimeWall(j, 1) = static_cast<double>(wall_now_ns() - t_phase1_wall);
      TimeCPU(j, 1) = cpu_now_ns() - t_phase1_cpu;

      const long long t_phase2_wall = wall_now_ns();
      const double t_phase2_cpu = cpu_now_ns();

      string method_j = Rcpp::as<string>(method[j]);
      if (method_j == "u" && cardt < mincard_vec(j)) {
        Rcpp::warning(("comp " + std::to_string(j + 1) + ": cannot reach desired cardinality for uSPCA, switching to cSPCA").c_str());
        method[j] = "c";
        method_j = "c";
      }

      VectorXi indt = indj.head(cardt);
      std::sort(indt.data(), indt.data() + cardt);

      if (method_j == "u") {
        uspcaTC(X, D, vexp_sel, indt, cardt, a, scores, j,
                PMS, epsPMS, maxiterPMS);
      } else if (method_j == "c") {
        cspcaTC(X, D, vexp_sel, indt, cardt, a, scores, j,
                PMS, epsPMS, maxiterPMS);
      } else {
        pspcaTC(X, r_scaled, indt, cardt, a, scores, j);
      }

      double anorm = a.head(cardt).norm();
      if (anorm > 0.0) a.head(cardt) /= anorm;

      A.col(j).setZero();
      for (int i = 0; i < cardt; i++) A(indt(i), j) = a(i);

      indout[j] = indt.array() + 1;
      loadlist[j] = a.head(cardt);
      card(j) = cardt;
      r2vec(j) = compute_r2_subset_T(X, u, indt, cardt);

      TimeWall(j, 2) = static_cast<double>(wall_now_ns() - t_phase2_wall);
      TimeCPU(j, 2) = cpu_now_ns() - t_phase2_cpu;

      const long long t_phase3_wall = wall_now_ns();
      const double t_phase3_cpu = cpu_now_ns();

      double cvt = 0.0;
      double curtrace = D.trace();
      MatrixXd pcj = scores.middleCols(j, 1);
      deflT_F(pcj, D, curtrace, cvt);

      nc = j + 1;
      VectorXd vtmp(nc), cvtmp(nc);
      compute_vexp_cvexp_T_exact(scores.leftCols(nc), nc, D_orig, totvexp, vtmp, cvtmp);

      if ((cvtmp(nc - 1) > ncompbycvexp * totvexp) || (nc == ncomps)) {
        stopComp = true;
      } else {
        j++;
      }

      TimeWall(nc - 1, 3) = static_cast<double>(wall_now_ns() - t_phase3_wall);
      TimeCPU(nc - 1, 3) = cpu_now_ns() - t_phase3_cpu;
    }
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("lsspcaTC component loop: ") + e.what());
  }

  VectorXd vexp_final(nc), cvexp_final(nc);
  compute_vexp_cvexp_T_exact(scores.leftCols(nc), nc, D_orig, totvexp, vexp_final, cvexp_final);

  IntegerVector idx = Rcpp::seq(0, nc - 1);
  CharacterVector meth(nc, "uSPCA");
  for (int j = 0; j < nc; j++) {
    if (Rcpp::as<string>(method[j]) == "c") meth[j] = "cSPCA";
    if (Rcpp::as<string>(method[j]) == "p") meth[j] = "pSPCA";
  }

  string varselection = (stop_criterion == 0) ? "forward R2" : "forward cvexp";
  CharacterVector Time_colnames = CharacterVector::create("setup", "varsel", "loadings", "deflation");

  return List::create(
    Named("loadings") = A.leftCols(nc),
    Named("loadlist") = loadlist[idx],
    Named("ncomps") = nc,
    Named("ind") = indout[idx],
    Named("card") = card.head(nc),
    Named("vexp") = vexp_final.head(nc),
    Named("cvexp") = cvexp_final.head(nc),
    Named("vexpPC") =  PCvexp.head(nc),
    Named("scores") = scores.leftCols(nc),
    Named("r2") = r2vec.head(nc),
    Named("totvar") = PCvexp.sum(),
    Named("Time_wall") = TimeWall.topRows(nc),
    Named("Time_cpu") = TimeCPU.topRows(nc),
    Named("Time_colnames") = Time_colnames,
    Named("setup_wall") = static_cast<double>(setup_wall_ns),
    Named("setup_cpu") = setup_cpu_ns,
    Named("time_unit_raw") = "nanoseconds",
    Named("method") = meth,
    Named("varSelection") = varselection
  );
}