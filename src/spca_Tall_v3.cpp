#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <set>
#include <Rcpp/Benchmark/Timer.h>

#include "support_shared_v3.h"
#include "support_tall_v3.h"

using namespace Rcpp;
using namespace Eigen;
using namespace std;


/*
May removed force_in/force_out, improved loop, and more
 New March 2026 rewritten variable selection, includes stop R2 and cvexp.
 Numerical failures in generalized-eigen steps are handled locally via the
 singular flag returned by the SPCA helpers.
 */
// [[Rcpp::depends(RcppEigen)]]

using Eigen::MatrixXd;                  // variable size matrix, double precision
using Eigen::VectorXd;                  // variable size vector, double precision
using Eigen::VectorXi;                  // variable size vector, integer
using Eigen::SelfAdjointEigenSolver;    // one of the eigenvalue solvers
using Rcpp::List;
using std::string;



// =========================================================================
// SPCA ALL TYPES: UNC, COR AND PROJ
// POWER METHOD AND EIGEN
// D = Q Q' COMPUTED ON THE FLY
// TIMER
// COMPUTE COR SPC/PC

// =========================================================================


// =====================================================================
// INPUT VALIDATION
//
// All user-facing input checks are performed here, once, before any
// computation begins. Internal helpers (varsel_fbsC, cspca, etc.)
// assume validated inputs.
//
// Runtime errors (singular matrices, non-convergence) are handled
// separately via exceptions thrown by the computational functions.
// @noRd
// =====================================================================

static void validate_lsspca_inputs(const Eigen::Ref<const Eigen::MatrixXd>& S,
                                   int p,
                                   int selection_method,
                                   int stop_criterion,
                                   bool exact_cvexp,
                                   double alpha,
                                   double ncompbycvexp,
                                   double rank_tol)
{
  // S must be square
  if (S.rows() != p)
    Rcpp::stop("S must be a square matrix");
  if (p == 0)
    Rcpp::stop("S must have at least one column");

  // variable selection parameters
  if (selection_method < 0 || selection_method > 3)
    Rcpp::stop("selection_method must be 0, 1, 2, or 3");
  if (stop_criterion < 0 || stop_criterion > 1)
    Rcpp::stop("stop_criterion must be 0 or 1");

  // selection_method == 3 (intensive forward CVEXP) requires stop_criterion == 1
  if (selection_method == 3 && stop_criterion != 1)
    Rcpp::stop("selection_method = 3 (intensive forward CVEXP) requires stop_criterion = 1");

  // alpha and ncompbycvexp
  if (alpha <= 0.0 || alpha > 1.0)
    Rcpp::stop("alpha must be in (0, 1]");
  if (ncompbycvexp <= 0.0 || ncompbycvexp > 1.0)
    Rcpp::stop("ncompbycvexp must be in (0, 1]");

  // rank_tol
  if (rank_tol < 0.0)
    Rcpp::stop("rank_tol must be >= 0");
}


// ================                   ================
//                     MAIN FUNCTION
// ================                   ================

// Least-Squares Sparse Principal Component Analysis, tall backend
//
// Computes LS-SPCA components from a covariance/correlation matrix. The backend
// supports cSPCA, uSPCA, and pSPCA loadings, fixed component supports, several
// variable-selection algorithms, and optional power-method eigensolvers.
//
// Parameters
// S: Numeric p x p covariance or correlation matrix.
// ncomps: Maximum number of components. If zero and ncompbycvexp < 1, the
//   backend computes components until the CVEXP target is reached or the
//   maximum possible number is reached.
// selection_method: Variable-selection code. 0 = forward, 1 = backward,
//   2 = forward-stepwise, 3 = intensive forward CVEXP.
// stop_criterion: Variable-selection stopping rule. 0 = squared correlation,
//   1 = cumulative variance explained.
// exact_cvexp: If true, use CVEXP values accumulated during the component loop.
//   If false, recompute VEXP and CVEXP from the final loading matrix before
//   returning.
// alpha: Target proportion used by variable selection.
// ncompbycvexp: Target cumulative variance explained for automatic component
//   stopping.
// method: Character vector of component methods. Entries must be "c", "u", or
//   "p" for cSPCA, uSPCA, and pSPCA.
// indvec_in: Optional 0-based fixed variable indices, concatenated across
//   components.
// cardvec_in: Optional cardinalities for fixed variable indices.
// PMPC: Use the power method for PC eigenvectors.
// PMS: Use the power method for sparse-loading eigenvectors in variable
//   selection and loading computation.
// epsPMPC: Convergence tolerance for PC power-method iterations.
// epsPMS: Convergence tolerance for sparse-loading power-method iterations.
// maxiterPMPC: Maximum number of PC power-method iterations.
// maxiterPMS: Maximum number of sparse-loading power-method iterations.
// rank_tol: Singularity tolerance for deflated covariance diagonals.
//
// Variable-selection combinations
// selection_method  stop_criterion  Algorithm
// 0   0   Forward selection with squared-correlation stopping
// 1   0   Backward elimination with squared-correlation stopping
// 2   0   Forward-stepwise selection with squared-correlation stopping
// 0   1   Forward selection with CVEXP stopping
// 1   1   Backward elimination with CVEXP stopping
// 2   1   Forward-stepwise selection with CVEXP stopping
// 3   1   Intensive forward CVEXP selection
// 3   0   Not allowed
//
// Returns
// A list with loadings, loadlist, ncomps, ind, card, vexp, cvexp, vexpPC, 
// cor_comps sPCs correlation matrix, r,
// totvar, method, varSelection, Time, Time_colnames, timevec, and timecomp.
// The element r contains signed correlations between each sPC and the
// corresponding original PC.
// [[Rcpp::export]]
 List lsspcaC(const Eigen::Map<Eigen::MatrixXd>& S,
              int ncomps = 0,
              int selection_method = 0,
              int stop_criterion = 0,
              bool exact_cvexp = false,
              double alpha = 0.95,
              double ncompbycvexp = 0.95,
              Rcpp::CharacterVector method = Rcpp::CharacterVector::create("c"),
              Rcpp::Nullable<Rcpp::IntegerVector> indvec_in  = R_NilValue,
              Rcpp::Nullable<Rcpp::IntegerVector> cardvec_in = R_NilValue,
              bool PMPC = false, bool PMS = false,
              double epsPMPC = 1E-5, double epsPMS = 1E-7,
              int maxiterPMPC = 100, int maxiterPMS = 200,
              double rank_tol = 0.0){
   int p = S.cols();

   // =====================================================================
   // PARSE NULLABLE ARGUMENTS (before validation, so we can check them)
   // =====================================================================

   // =====================================================================
   // INPUT VALIDATION (all checks before any computation)
   // =====================================================================

   validate_lsspca_inputs(S, p, selection_method, stop_criterion,
                          exact_cvexp, alpha,
                          ncompbycvexp, rank_tol);

   // ncomps validation
   if (ncomps == 0 && ncompbycvexp > 0.0 && ncompbycvexp < 1.0)
     ncomps = p;
   else if (ncomps <= 0 || ncomps >= p)
     Rcpp::stop("ncomps must be between 1 and p-1");
   else
     ncompbycvexp = 1;

   bool fixedind = false;
   int startind = 0;
   VectorXi indvec(0), cardvec(0);

   // fixed indices
   if (indvec_in.isNotNull() && cardvec_in.isNotNull()) {
     indvec = Rcpp::as<VectorXi>(indvec_in.get());
     cardvec = Rcpp::as<VectorXi>(cardvec_in.get());
     fixedind = true;

     if (indvec.size() != cardvec.sum())
       Rcpp::stop("length of indvec_in must be equal to sum cardvec_in");
   }

   // expand method vector to length ncomps
   if (method.size() < ncomps) {
     int oldsize = method.size();
     Rcpp::CharacterVector newmethod(ncomps);
     for (int i = 0; i < oldsize; i++)
       newmethod[i] = method[i];
     for (int i = oldsize; i < ncomps; i++)
       newmethod[i] = method[oldsize - 1];
     method = newmethod;
   }

   // validate method entries and build mincard_vec; check fixedind cardinalities
   VectorXi mincard_vec(ncomps);
   for (int i = 0; i < ncomps; i++) {
     if (Rcpp::as<std::string>(method[i]) != "u" &&
         Rcpp::as<std::string>(method[i]) != "c" &&
         Rcpp::as<std::string>(method[i]) != "p")
       Rcpp::stop("method entries must be one of 'u', 'c', or 'p'");
     if (Rcpp::as<std::string>(method[i]) == "u") {
       mincard_vec(i) = i + 1;
       if (fixedind && cardvec(i) > 0 && cardvec(i) < i + 1)
         Rcpp::stop("the fixed indices have a problem: for uncorrelated components the cardinality cannot be smaller than the component's order");
     } else {
       mincard_vec(i) = 1;
     }
   }

   // =====================================================================
   // ALLOCATE WORKING OBJECTS
   // =====================================================================

   Timer timer;
   Timer timecomp;

   Eigen::MatrixXd G = S; //deflated S matrix

   Eigen::VectorXd a(p);
   Eigen::MatrixXd A = Eigen::MatrixXd::Zero(p, ncomps);
   Rcpp::List indout(ncomps), loadlist(ncomps);

   Eigen::VectorXd vexp = Eigen::VectorXd::Zero(ncomps);
   Eigen::VectorXd cvexp = vexp;
   Eigen::VectorXd r = Eigen::VectorXd::Zero(ncomps);
   double cvt, donedefl = 0;
   double prev_cvexp_j = 0;       // previous cvexp, needed for stop_criterion == 1
   double target_cvexp_j = 0.0;   // cumulative PC variance, needed for stop_criterion == 1
   Eigen::VectorXi indj(p), indjm1(p);

   Eigen::MatrixXd R(p, p);
   // stores the original PC eigenvalues and, for full eigensolve, eigenvectors
   Eigen::VectorXd PCvexp(p), vec(p);
   Eigen::MatrixXd PCvec;
   Eigen::VectorXd pc_current = Eigen::VectorXd::Zero(p);
   Eigen::MatrixXd G_pc;
   Eigen::VectorXi ind_pc;
   int pc_computed = 0;
   if (PMPC) {
     G_pc = S;
     ind_pc = Eigen::VectorXi::LinSpaced(p, 0, p - 1);
   }
   std::string stringa;

   double totvexp = 0.0, maxvexp = 0.0,  eigval = 0.0;// total variance S
   // maxvexp this is vexp by first PC for fwd_select
   Eigen::VectorXd si(p);

   int cardt = 0;
   Eigen::VectorXi card(p);

   int cardtm1 = 0;
   int nc = 0;
   bool stopComp = false;

   // varsel_fbsC output variables
   double criterion_value = 0.0;
   Eigen::VectorXd loadings_out = Eigen::VectorXd::Zero(p);
   double vexp_out = 0.0;
   int varsel_reached = 0;

   // M = G'G matrix for CVEXP-based selection
   // When stop_criterion == 1, M is initialized to S*S and then deflated
   // incrementally via deflSandDC (avoids recomputing G*G every component)
   Eigen::MatrixXd M_varsel = Eigen::MatrixXd::Zero(1, 1);
   if (stop_criterion == 1)
     M_varsel = S * S;
   // B = loading matrix for exact_cvexp
   Eigen::MatrixXd B_varsel = Eigen::MatrixXd::Zero(p, ncomps);

   timer.step("start") ;   // START TIMING COMPONENTS ====================
   timecomp.step("start") ; // ---------------- START TIMER ==================
   int j = 0;
   try {
     while (stopComp == false){
       // passing j as minimum card because of orth constraints
       if (PMPC == false){
         SelfAdjointEigenSolver<Eigen::MatrixXd> es(G);
         maxvexp =  es.eigenvalues()(p - 1);
        si = es.eigenvectors().col(p - 1).array() * sqrt(maxvexp);
         if (j == 0){
           PCvexp = es.eigenvalues().reverse();
           PCvec = es.eigenvectors().rowwise().reverse().leftCols(ncomps);
           for (int k = 0; k < ncomps; k++) {
             if (PCvec(0, k) < 0.0)
               PCvec.col(k) = -PCvec.col(k);
           }
           si = PCvec.col(0).array() * sqrt(maxvexp);
           totvexp = PCvexp.sum();// total variance S
         }
         pc_current = PCvec.col(j);
         target_cvexp_j += PCvexp(j);

       }
       else{
         while (pc_computed <= j) {
           double pc_val = 0.0;
           Eigen::VectorXd pc_scaled = eigvecPMC(G_pc, pc_val, epsPMPC, maxiterPMPC);
           if (!std::isfinite(pc_val) || pc_val <= 0.0)
             Rcpp::stop("lsspcaC: non-positive original PC eigenvalue");
           Eigen::VectorXd pc_vec = pc_scaled / sqrt(pc_val);
           if (pc_vec(0) < 0.0)
             pc_vec = -pc_vec;
           PCvexp(pc_computed) = pc_val;
           if (pc_computed == j)
             pc_current = pc_vec;
           double pc_defl_vexp = 0.0;
           const double pc_failed = deflSC(pc_vec, G_pc, ind_pc, pc_defl_vexp, epsPMPC);
           if (pc_failed != 0.0)
             Rcpp::stop("lsspcaC: original PC deflation failed");
           pc_computed++;
         }
         if (j == 0){
           maxvexp = PCvexp(0);
           si = pc_current.array() * sqrt(maxvexp);
           totvexp = S.trace();// total variance S
         }
         else{
           si = eigvecPMC(G,  maxvexp, epsPMPC, maxiterPMPC);
           si = si.array();
         }
         target_cvexp_j += PCvexp(j);

      }

       stringa = "comp " +  std::to_string(j + 1);
       timer.step(stringa + ":  PC"); // ======================TIMER PC =======================

       //    Rcout << "comp " << j << " card " << cardt << endl;
       //==============================================
       // VARIABLE SELECTION
       //==============================================

       // fixedind NO VARIABLE SELECTION==
       if (fixedind == true && cardvec(j) > 0){
         indj = indvec.segment(startind, cardvec(j));
         cardt = cardvec(j);
         startind = startind + cardvec(j);
       }
       // VARIABLE SELECTION VIA varsel_fbsC =======================
       else {
         prev_cvexp_j = (j > 0) ? cvexp(j - 1) : 0.0;

         // M_varsel is initialized once before the loop and deflated
         // incrementally via deflSandDC after each component

         // reset outputs
         criterion_value = 0.0;
         loadings_out.setZero();
         vexp_out = 0.0;

         // translate selection_method == 3 to intensive forward for varsel_fbsC
         bool varsel_intensive = (selection_method == 3);
         int varsel_method = varsel_intensive ? 0 : selection_method;

         varsel_reached = varsel_fbsC(
           S, si,
           // outputs
           indj, cardt, criterion_value, loadings_out, vexp_out,
           // control parameters
           alpha, varsel_method, stop_criterion,
           varsel_intensive, exact_cvexp,
           maxvexp, target_cvexp_j,
           mincard_vec(j),
           M_varsel, prev_cvexp_j, j, B_varsel,
           1, -1,  // ntrim, reducetrim (unused, kept for varsel_fbsC signature)
           rank_tol, PMS, epsPMS, maxiterPMS);

         // when selection_method == 3 (intensive forward CVEXP),
         // loadings and eigval are computed during selection
         if (selection_method == 3 && cardt > 0) {
           a.head(cardt) = loadings_out.head(cardt);
           eigval = vexp_out;
         }
       }// end variable selection

       timer.step(stringa + "variable_selection") ; // ---------------- TIMER ==================

       if (Rcpp::as<std::string>(method[j]) == "u" && cardt < mincard_vec(j)){
         Rf_warning("Cannot reach desired cardinality for uSPCA, switching to cSPCA");
         method[j] = "c";
       }

       card(j) = cardt;

       // create submatrices for computing loadings
       // when selection_method == 3 (intensive forward CVEXP), loadings (a) and
       // eigval are already computed by varsel_fbsC via cspca_varsel
       if (!((selection_method == 3) && (method[j] == "c"))) {
         bool singular = false;
         if (method[j] == "u"){
             uspcaC(S, eigval, p, j, indj, cardt, indjm1, cardtm1,
                   R, a, singular, PMS, epsPMS, maxiterPMS);
             vexp(j) = eigval;
             if (singular){
               Rcout << "Numerical problems with uSPCA comp " << j + 1 << ", switching to cSPCA" << endl;
               method[j] = "c";
             }
         }

       if (Rcpp::as<std::string>(method[j]) == "c"){
           cspcaC(S, G, eigval, p, j, indj, cardt,
                 a, singular, PMS, epsPMS, maxiterPMS);
         if (singular){
           string stringa = "singular submatrix for component " +  std::to_string(j + 1);
           Rcout << stringa << endl;
           Rcpp::stop("error in CSPCA");
         }
       }
       else if (Rcpp::as<std::string>(method[j]) == "p"){
         pspcaC(S, si, indj, cardt, a, singular);
         if (singular){
           string stringa = "singular submatrix for component " +
             std::to_string(j + 1);
           Rcout << stringa << endl;
           Rcpp::stop("error in PSPCA");
         }
       }
       } // end loading computation (not intensive cvexp)

         timer.step(stringa + "Component") ; // ---------------- TIMER ==================

         //    Rcout << "fatte " << ncomps << " components" << endl;
         // save loadings in column j
         a.head(cardt) = a.head(cardt).array()/a.head(cardt).norm();
         for (int i = 0; i < cardt; i++){
           A(indj(i), j) = a(i);
         }
//  change sign to loadings if needed, Uses max loading but avoids
// matrix multiplication. Not perfect can be fixed in R


// compute signed correlation with the corresponding original PC
         VectorXi indt_sub = indj.head(cardt);
         MatrixXd Sdd(cardt, cardt);
         Sdd = makeSubS(S, indt_sub);
         VectorXd pc_sub(cardt);
         for (int h = 0; h < cardt; h++) pc_sub(h) = pc_current(indj(h));
         double dot_av = a.head(cardt).dot(pc_sub);
         double den_r = (a.head(cardt).transpose() * Sdd * a.head(cardt))(0, 0);
         if (den_r > 0.0 && PCvexp(j) > 0.0)
           r(j) = sqrt(PCvexp(j)) * dot_av / sqrt(den_r);
         else
           r(j) = 0.0;
         if (r(j) < -1.0) r(j) = -1.0;
         if (r(j) > 1.0) r(j) = 1.0;

         // save loadings in list

         Eigen::VectorXi indt(indj);
         std::sort(indt.data(),indt.data() + cardt);
         indout[j] = indt.head(cardt).array() + 1;

         loadlist[j] = a.head(cardt);
         indjm1.head(cardt) = indj.head(cardt);
         cardtm1 = cardt;
         nc = nc + 1;

         // deflate G (and M_varsel when stop_criterion == 1)
         VectorXd a_defl = a.head(cardt);
         VectorXi ind_defl = indj.head(cardt);
         if (stop_criterion == 1)
           donedefl = deflSandDC(a_defl, G, M_varsel, ind_defl, cvt);
         else
           donedefl = deflSC(a_defl, G, ind_defl, cvt);

         timer.step(stringa + "Deflation") ; // ---------------- TIMER ==================
         if(donedefl == 1){
           Rcout << "variance matrix exhausted. Computed " << j << endl;
           vexp(j) = nan("1");
           cvexp(j) = nan("1");
           if (ncomps == p)
             nc = j - 1;
           else{
             for(int i = j; i < ncomps; i++){
               vexp(i) = nan("1");
               cvexp(i) = nan("1");
             }
             nc = ncomps;
           }
           break;
         }


         vexp(j) = cvt;
         if (j > 0)
           cvexp(j) = vexp(j) + cvexp(j-1);
         else
           cvexp(j) = vexp(j);

         // update column j of B_varsel with current loadings for exact_cvexp
         if (exact_cvexp)
           B_varsel.col(j) = A.col(j);

         // checks if stopComp met
         if ((cvexp(j) > ncompbycvexp * totvexp) || ((j + 1) == ncomps)){
           stopComp = true;
           ncomps = nc;
         }
         else{
           j = j + 1;
         }
         // ======================TIMERcomp step =======================
         timecomp.step(stringa) ;
     }//end compute comps cacca
   } catch (const std::exception& e) {
     Rcpp::stop(std::string("lsspcaC component loop: ") + e.what());
   }

   timer.step("end") ;
   try {
     NumericVector res(timer);
     NumericVector rescomp(timecomp);

     // timer differences: columns are PC, variable_selection, Component, Deflation
     Eigen::MatrixXd Ti(ncomps, 4);
     NumericVector tt = diff(res);
     for(int i = 0; i < ncomps; i++)
       for(int j = 0; j < 4; j++)
         Ti(i, j) =  tt(i*4 + j);
     Rcpp::CharacterVector Ti_colnames = Rcpp::CharacterVector::create(
       "PC", "varsel", "loadings", "deflation");

     // returned vexp/cvexp come directly from the fitting loop when
     // exact_cvexp = TRUE; otherwise recompute them exactly from the final
     // loading matrix before returning.
     Eigen::VectorXd vexp_final(nc);
     Eigen::VectorXd cvexp_final(nc);
     if (exact_cvexp) {
       vexp_final = vexp.head(nc);
       cvexp_final = cvexp.head(nc);
     } else {
       Rcpp::List vexp_list = makeVexpSC(A.leftCols(nc), S);
       vexp_final = vexp_list["vexp"];
       cvexp_final = vexp_list["cvexp"];
     }
     Eigen::MatrixXd cor_comps;
     if(nc > 1)
       cor_comps = makeCorComp_int(A.leftCols(nc), S, nc); 
     else 
       cor_comps = Eigen::MatrixXd::Ones(1, 1);
     
     IntegerVector idx = Rcpp::seq(0, nc - 1);

     Rcpp::CharacterVector meth(nc, "uSPCA");
     for (int j = 0; j < nc; j++){
       if (Rcpp::as<std::string>(method[j]) == "c")
         meth[j] = "cSPCA";
       if (Rcpp::as<std::string>(method[j]) == "p")
         meth[j] = "pSPCA";
     }

     std::string varselection;
     if (stop_criterion == 0) {
       if (selection_method == 0) varselection = "forward R2";
       else if (selection_method == 1) varselection = "backward R2";
       else if (selection_method == 2) varselection = "fwd stepwise R2";
     } else {
       if (selection_method == 0) varselection = "forward cvexp";
       else if (selection_method == 1) varselection = "backward cvexp";
       else if (selection_method == 2) varselection = "fwd stepwise cvexp";
       else if (selection_method == 3) varselection = "forward cvexp intensive";
     }

       return
       List::create(
         Named("loadings")= A.topLeftCorner(p,nc),
         Named("loadlist") = loadlist[idx],
         Named("ncomps") = nc,
         Named("ind") = indout[idx],
         Named("card") = card.head(nc),
         Named("vexp") = vexp_final.head(nc),
         Named("cvexp") = cvexp_final.head(nc),
         Named("vexpPC") =  PCvexp.head(nc),
         Named("cor_comps") = cor_comps,
         Named("r") = r.head(nc),
         Named("totvar") = totvexp,
         Named("method") = meth,
         Named("varSelection") = varselection,
         Named("Time") = Ti,
         Named("Time_colnames") = Ti_colnames,
         Named("timevec") = res,
         Named("timecomp") = timecomp
       );

   } catch (const std::exception& e) {
     Rcpp::stop(std::string("lsspcaC output: ") + e.what());
   }
   return List();
 }
