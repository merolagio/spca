
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
#include "support_fat.h" // this with new var_sel

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

static void validate_lsspcaT_inputs(const Eigen::Ref<const Eigen::MatrixXd>& X,
                                    int n,
                                    int p,
                                    int stop_criterion,
                                    bool exact_cvexp,
                                    double alpha,
                                    double ncompbycvexp,
                                    double rank_tol)
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
}


// Least-Squares Sparse Principal Component Analysis, fat backend
//
// Computes LS-SPCA components for a centered data matrix with p > n. The
// backend works in row space, using X X' instead of X' X, and maps results back
// to column-space loading vectors.
//
// Parameters
// X: Numeric n x p centered data matrix with p > n.
// ncomps: Maximum number of components. If zero and ncompbycvexp < 1, the
//   backend computes components until the CVEXP target is reached or the
//   maximum possible number is reached.
// stop_criterion: Variable-selection stopping rule. 0 = squared correlation,
//   1 = cumulative variance explained.
// exact_cvexp: Accepted for consistency with the tall backend. Returned VEXP
//   and CVEXP are recomputed from the final score matrix.
// alpha: Target proportion used by variable selection.
// ncompbycvexp: Target cumulative variance explained for automatic component
//   stopping.
// method: Character vector of component methods. Entries must be "c", "u", or
//   "p" for cSPCA, uSPCA, and pSPCA.
// indvec_in: Optional 0-based fixed variable indices, concatenated across
//   components.
// cardvec_in: Optional cardinalities for fixed variable indices.
// PMPC: Use the power method for PCs of the row-space matrix.
// PMS: Use the power method for sparse-loading eigenvectors.
// epsPMPC: Convergence tolerance for row-space PC power-method iterations.
// epsPMS: Convergence tolerance for sparse-loading power-method iterations.
// maxiterPMPC: Maximum number of row-space PC power-method iterations.
// maxiterPMS: Maximum number of sparse-loading power-method iterations.
// rank_tol: Accepted for interface consistency; currently not used by the
//   fat variable-selection backend.
//
// Variable-selection combinations
// stop_criterion  Algorithm
// 0               Forward selection with squared-correlation stopping
// 1               Forward selection with CVEXP stopping
//
// Returns
// A list with loadings, loadlist, ncomps, ind, card, vexp, cvexp, vexpPC,
// scores, cor_comps, r, totvar, method, varSelection, Time_wall, Time_cpu, Time_colnames,
// setup_wall, setup_cpu, and time_unit_raw. The element r contains signed
// correlations between each sPC and the corresponding original PC.
// This function is called from the R wrapper spca()

// [[Rcpp::export]]
List lsspcaTC(const Eigen::Map<Eigen::MatrixXd>& X,
              int ncomps = 0,
              int stop_criterion = 0,
              bool exact_cvexp = false,
              double alpha = 0.95,
              double ncompbycvexp = 0.95,
              Rcpp::CharacterVector method = Rcpp::CharacterVector::create("c"),
              Rcpp::Nullable<Rcpp::IntegerVector> indvec_in  = R_NilValue,
              Rcpp::Nullable<Rcpp::IntegerVector> cardvec_in = R_NilValue,
              bool PMPC = false, bool PMS = false,
              double epsPMPC = 1E-5, double epsPMS = 1E-7,
              int maxiterPMPC = 300, int maxiterPMS = 200,
              double rank_tol = 0.0)
{
  const int n = X.rows();
  const int p = X.cols();

  try {

  const long long t_setup0_wall = wall_now_ns();
  const double t_setup0_cpu = cpu_now_ns();

  validate_lsspcaT_inputs(X, n, p, stop_criterion,
                          exact_cvexp, alpha, ncompbycvexp, rank_tol);

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
  VectorXd rvec = VectorXd::Zero(ncomps);

  MatrixXd TimeWall = MatrixXd::Zero(ncomps, 4);
  MatrixXd TimeCPU = MatrixXd::Zero(ncomps, 4);
  Timer timer;
  Timer timecomp;

  VectorXd PCvexp(n);
  MatrixXd PCscores = MatrixXd::Zero(n, ncomps);
  VectorXd pc_score_current = VectorXd::Zero(n);
  MatrixXd D_pc;
  int pc_computed = 0;
  if (PMPC)
    D_pc = D_orig;
  double totvexp = D_orig.trace();
  double target_cvexp_j = 0.0;

  VectorXd a(p), r_scaled(n), u(n);
  VectorXi indj(p);
  int cardt = 0, nc = 0;
  bool stopComp = false;

  const long long setup_wall_ns = wall_now_ns() - t_setup0_wall;
  const double setup_cpu_ns = cpu_now_ns() - t_setup0_cpu;

  timer.step("start");
  timecomp.step("start");

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
          MatrixXd row_pc = es.eigenvectors().rowwise().reverse().leftCols(ncomps);
          for (int k = 0; k < ncomps; k++) {
            if (PCvexp(k) <= 0.0) {
              PCscores.col(k).setZero();
              continue;
            }
            VectorXd loading_k = X.transpose() * row_pc.col(k) / std::sqrt(PCvexp(k));
            double nrm_pc = loading_k.norm();
            if (nrm_pc > 0.0) loading_k /= nrm_pc;
            if (loading_k(0) < 0.0) {
              row_pc.col(k) = -row_pc.col(k);
            }
            PCscores.col(k) = std::sqrt(PCvexp(k)) * row_pc.col(k);
          }
          u = row_pc.col(0);
          r_scaled = PCscores.col(0);
          totvexp = PCvexp.sum();
        }
        pc_score_current = PCscores.col(j);
      } else {
        while (pc_computed <= j) {
          double pc_val = 0.0;
          VectorXd u_scaled = eigvecPMC(D_pc, pc_val, epsPMPC, maxiterPMPC);
          if (!std::isfinite(pc_val) || pc_val <= 0.0)
            Rcpp::stop("lsspcaTC: non-positive original row-space PC eigenvalue");
          VectorXd u_pc = u_scaled / std::sqrt(pc_val);
          VectorXd loading_k = X.transpose() * u_pc / std::sqrt(pc_val);
          double nrm_pc = loading_k.norm();
          if (!std::isfinite(nrm_pc) || nrm_pc <= 0.0)
            Rcpp::stop("lsspcaTC: zero or non-finite original PC loading norm");
          loading_k /= nrm_pc;
          if (loading_k(0) < 0.0)
            u_pc = -u_pc;
          PCvexp(pc_computed) = pc_val;
          PCscores.col(pc_computed) = std::sqrt(pc_val) * u_pc;
          if (pc_computed == j)
            pc_score_current = PCscores.col(pc_computed);
          double pc_defl_vexp = 0.0;
          deflT_rank1(u_pc, D_pc, pc_defl_vexp);
          pc_computed++;
        }
        if (j == 0) {
          maxvexp = PCvexp(0);
          if (maxvexp <= 0.0)
            Rcpp::stop("lsspcaTC: non-positive leading eigenvalue in original row-space PC");
          u = pc_score_current / std::sqrt(maxvexp);
          r_scaled = pc_score_current;
          totvexp = D_orig.trace();
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
      timer.step("comp " + std::to_string(j + 1) + ":  PC");

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
      timer.step("comp " + std::to_string(j + 1) + "variable_selection");

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
      
// compute lsspca loadings      
      bool singular_fit = false;
      try {
        if (method_j == "u") {
          uspcaTC(X, D, vexp_sel, indt, cardt, a, scores, singular_fit, j,
                  PMS, epsPMS, maxiterPMS);
        } else if (method_j == "c") {
          cspcaTC(X, D, vexp_sel, indt, cardt, a, scores, singular_fit, j,
                  PMS, epsPMS, maxiterPMS);
        } else {
          pspcaTC(X, r_scaled, indt, cardt, a, scores, singular_fit, j);
        }
      } catch (const std::exception& e) {
        Rcpp::stop(std::string("lsspcaTC comp ") + std::to_string(j + 1) +
          ": loading computation failed: " + e.what());
      } catch (...) {
        Rcpp::stop(std::string("lsspcaTC comp ") + std::to_string(j + 1) +
          ": loading computation failed with unknown C++ error");
      }
      
      if (singular_fit) {
        Rcpp::stop(std::string("lsspcaTC comp ") + std::to_string(j + 1) +
          ": singular loading fit");
      }
      
      double anorm = a.head(cardt).norm();
      if (anorm > 0.0) a.head(cardt) /= anorm;

      A.col(j).setZero();
      for (int i = 0; i < cardt; i++) A(indt(i), j) = a(i);

      indout[j] = indt.array() + 1;
      loadlist[j] = a.head(cardt);
      card(j) = cardt;
      double den_r = scores.col(j).squaredNorm() * PCvexp(j);
      if (den_r > 0.0)
        rvec(j) = scores.col(j).dot(pc_score_current) / std::sqrt(den_r);
      else
        rvec(j) = 0.0;
      if (rvec(j) < -1.0) rvec(j) = -1.0;
      if (rvec(j) > 1.0) rvec(j) = 1.0;

      TimeWall(j, 2) = static_cast<double>(wall_now_ns() - t_phase2_wall);
      TimeCPU(j, 2) = cpu_now_ns() - t_phase2_cpu;
      timer.step("comp " + std::to_string(j + 1) + "Component");

      const long long t_phase3_wall = wall_now_ns();
      const double t_phase3_cpu = cpu_now_ns();

      double cvt = 0.0;
      deflT_rank1(scores.col(j), D, cvt);

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
      timer.step("comp " + std::to_string(nc) + "Deflation");
      timecomp.step("comp " + std::to_string(nc));
    }
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("lsspcaTC component loop: ") + e.what());
  }

  timer.step("end");
  try {
    NumericVector res(timer);

    MatrixXd Ti(nc, 4);
    NumericVector tt = diff(res);
    for (int i = 0; i < nc; i++)
      for (int j = 0; j < 4; j++)
        Ti(i, j) = tt(i * 4 + j);

    VectorXd vexp_final(nc), cvexp_final(nc);
    compute_vexp_cvexp_T_exact(scores.leftCols(nc), nc, D_orig, totvexp, vexp_final, cvexp_final);

    IntegerVector idx = Rcpp::seq(0, nc - 1);
    CharacterVector meth(nc, "uSPCA");
    for (int j = 0; j < nc; j++) {
      if (Rcpp::as<string>(method[j]) == "c") meth[j] = "cSPCA";
      if (Rcpp::as<string>(method[j]) == "p") meth[j] = "pSPCA";
    }

    string varselection = (stop_criterion == 0) ? "forward R2" : "forward cvexp";
    CharacterVector Time_colnames = CharacterVector::create("PC", "varsel", "loadings", "deflation");
    
    Eigen::MatrixXd cor_comps;
    if (nc > 1)
      cor_comps = cor_int(scores.leftCols(nc), false, true);
    else
      cor_comps = Eigen::MatrixXd::Ones(1, 1);   
    
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
      Named("cor_comps") = cor_comps,
      Named("r") = rvec.head(nc),
      Named("totvar") = totvexp,
      Named("Time") = Ti,
      Named("Time_wall") = TimeWall.topRows(nc),
      Named("Time_cpu") = TimeCPU.topRows(nc),
      Named("Time_colnames") = Time_colnames,
      Named("setup_wall") = static_cast<double>(setup_wall_ns),
      Named("setup_cpu") = setup_cpu_ns,
      Named("time_unit_raw") = "nanoseconds",
      Named("timevec") = res,
      Named("timecomp") = timecomp,
      Named("method") = meth,
      Named("varSelection") = varselection
    );
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("lsspcaTC output: ") + e.what());
  }
  return List();
  } catch (const std::exception& e) {
    Rcpp::stop(std::string("lsspcaTC: ") + e.what());
  } catch (...) {
    Rcpp::stop("lsspcaTC: unknown C++ error");
  }
  return List();
}
