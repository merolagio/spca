#include <Rcpp.h>
#include <RcppEigen.h>
#include <Eigen/Dense>
#include <cmath>
#include <stdexcept>

#include "support_shared.h"
#include "support_tall.h"
#include "support_fat.h"

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace Eigen;
using namespace std;

// Basic validation shared by the full-eigen and power-method PCA paths.
// More detailed data handling is done by the R wrapper.
static void validate_pcaC_inputs(const Eigen::Ref<const Eigen::MatrixXd>& M,
                                 int ncomps,
                                 bool data_matrix,
                                 bool fat_matrix)
{
  const int n = M.rows();
  const int p = M.cols();

  if (n < 1 || p < 1)
    Rcpp::stop("M must have at least one row and one column");

  if (!M.allFinite())
    Rcpp::stop("M contains non-finite values");

  if (!data_matrix) {
    if (n != p)
      Rcpp::stop("covariance/correlation input must be square");

    const double nrm = M.norm();
    if (nrm > 0.0 && (M - M.transpose()).norm() > 1e-6 * nrm)
      Rcpp::stop("covariance/correlation input must be symmetric");
  }

  if (fat_matrix) {
    if (!data_matrix)
      Rcpp::stop("fat_matrix = TRUE requires a data matrix");

    if (n >= p)
      Rcpp::stop("fat_matrix = TRUE requires n < p");
  }

  const int max_comps = fat_matrix ? n : p;
  if (ncomps < 1 || ncomps > max_comps)
    Rcpp::stop("ncomps is outside the allowed range for this backend");
}

// Exported low-level helper for computing leading eigenpairs by power method.
// For fat_matrix = TRUE, M is a data matrix; otherwise M is a covariance matrix.
// [[Rcpp::export]]
Rcpp::List PMnEigenpairs(const Eigen::Map<Eigen::MatrixXd>& M,
                      int ncomps,
                      bool fat_matrix = false,
                      double epsPM = 1e-5,
                      int maxiterPM = 100)
{
  try {
    if (ncomps < 1)
      Rcpp::stop("ncomps must be positive");
    if (epsPM <= 0.0 || !std::isfinite(epsPM))
      Rcpp::stop("epsPM must be positive and finite");
    if (maxiterPM < 1)
      Rcpp::stop("maxiterPM must be positive");
    if (!M.allFinite())
      Rcpp::stop("M contains non-finite values");

    if (fat_matrix) {
      if (M.rows() >= M.cols())
        Rcpp::stop("fat_matrix = TRUE requires n < p");
      if (ncomps > M.rows())
        Rcpp::stop("ncomps cannot exceed n in the fat backend");
      return PMnEigenpairs_fat(M, ncomps, epsPM, maxiterPM);
    }

    if (M.rows() != M.cols())
      Rcpp::stop("tall PMnEigenpairs expects a square covariance/correlation matrix");
    if (ncomps > M.cols())
      Rcpp::stop("ncomps cannot exceed matrix dimension");
    return PMnEigenpairs_tall(M, ncomps, epsPM, maxiterPM);
  } catch (std::exception& ex) {
    Rcpp::stop(std::string("PMnEigenpairs: ") + ex.what());
  } catch (...) {
    Rcpp::stop("PMnEigenpairs: unknown C++ exception");
  }
}

// pcaC
//
// Main C++ PCA backend used by the R wrapper.
// Computes ordinary PCA for tall/covariance inputs and row-space PCA for fat
// data matrices. If PM = true, only the requested leading eigenpairs are
// computed by power method and rank-one deflation.
//
// @param M Numeric matrix. If data_matrix = true, M is a data matrix. If
//   data_matrix = false, M is a covariance or correlation matrix.
// @param ncomps Number of principal components/eigenpairs to return.
// @param data_matrix Logical. TRUE when M is a data matrix; FALSE when M is a
//   covariance or correlation matrix.
// @param fat_matrix Logical. TRUE uses the fat matrix backend based on M M';
//   FALSE uses the tall/covariance backend.
// @param PM Logical. TRUE uses power method plus deflation; FALSE uses full
//   eigendecomposition.
// @param epsPM Convergence tolerance for the power method.
// @param maxiterPM Maximum number of power-method iterations.
//
// @return List with:
//   loadings: p x ncomps matrix of unit L2 loading vectors.
//   scores: n x ncomps matrix of PC scores when M is a data matrix, otherwise
//     an empty matrix.
//   vexpPC: variance explained by the returned PCs, scaled by total variance.
//   eigenvalues: eigenvalues returned by the selected backend. With PM = true,
//     only ncomps eigenvalues are returned.
//   totvar: total variance, equal to trace of the covariance/row-space matrix.
//   ncomps: number of returned components.
//   fat_matrix: backend flag used by the computation.
//   PM: power-method flag used by the computation.
// [[Rcpp::export]]
Rcpp::List pcaC(const Eigen::Map<Eigen::MatrixXd>& M,
                int ncomps,
                bool data_matrix = true,
                bool fat_matrix = false,
                bool PM = false,
                double epsPM = 1e-5,
                int maxiterPM = 100)
{
  try {
    validate_pcaC_inputs(M, ncomps, data_matrix, fat_matrix);
    if (epsPM <= 0.0 || !std::isfinite(epsPM))
      Rcpp::stop("epsPM must be positive and finite");
    if (maxiterPM < 1)
      Rcpp::stop("maxiterPM must be positive");

    Eigen::VectorXd eigenvalues;
    Eigen::MatrixXd loadings;
    Eigen::MatrixXd scores(0, 0);
    double totvar = 0.0;

    if (fat_matrix) {
      // Fat path: solve PCA in row space and return column-space loadings.
      if (PM) {
        Rcpp::List pm = PMnEigenpairs_fat(M, ncomps, epsPM, maxiterPM);
        loadings = Rcpp::as<Eigen::MatrixXd>(pm["vec"]);
        scores = Rcpp::as<Eigen::MatrixXd>(pm["scores"]);
        eigenvalues = Rcpp::as<Eigen::VectorXd>(pm["val"]);
        totvar = (M * M.transpose()).trace();
      } else {
        // Full row-space eigendecomposition.
        Eigen::MatrixXd K = M * M.transpose();
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(K);
        if (es.info() != Eigen::Success)
          Rcpp::stop("pcaC: eigendecomposition failed in fat backend");

        eigenvalues = Eigen::VectorXd(es.eigenvalues()).reverse();
        Eigen::MatrixXd row_vec = Eigen::MatrixXd(es.eigenvectors()).rowwise().reverse();

        totvar = eigenvalues.sum();
        loadings = M.transpose() * row_vec.leftCols(ncomps);

        // Normalize loadings and keep score signs consistent with them.
        for (int j = 0; j < ncomps; ++j) {
          const double nrm = loadings.col(j).norm();
          if (!std::isfinite(nrm) || nrm <= 0.0)
            Rcpp::stop("pcaC: zero or non-finite loading norm in fat backend");
          loadings.col(j) /= nrm;

          if (loadings(0, j) < 0.0) {
            loadings.col(j) = -loadings.col(j);
            row_vec.col(j) = -row_vec.col(j);
          }
        }

        scores = row_vec.leftCols(ncomps);
        for (int j = 0; j < ncomps; ++j)
          scores.col(j) *= std::sqrt(eigenvalues(j));
      }
    } else {
      // Tall path: use the covariance/correlation matrix directly.
      Eigen::MatrixXd S;
      if (data_matrix)
        S = M.transpose() * M;
      else
        S = M;

      if (PM) {
        // Leading eigenpairs by power method and covariance deflation.
        Rcpp::List pm = PMnEigenpairs_tall(S, ncomps, epsPM, maxiterPM);
        loadings = Rcpp::as<Eigen::MatrixXd>(pm["vec"]);
        eigenvalues = Rcpp::as<Eigen::VectorXd>(pm["val"]);
        totvar = S.trace();
      } else {
        // Full covariance eigendecomposition.
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(S);
        if (es.info() != Eigen::Success)
          Rcpp::stop("pcaC: eigendecomposition failed in tall backend");

        eigenvalues = Eigen::VectorXd(es.eigenvalues()).reverse();
        Eigen::MatrixXd eigvec = Eigen::MatrixXd(es.eigenvectors()).rowwise().reverse();
        totvar = eigenvalues.sum();
        loadings = eigvec.leftCols(ncomps);

        for (int j = 0; j < ncomps; ++j) {
          if (loadings(0, j) < 0.0)
            loadings.col(j) = -loadings.col(j);
        }
      }

      if (data_matrix)
        scores = M * loadings;
    }

    return Rcpp::List::create(
      Rcpp::Named("loadings") = loadings,
      Rcpp::Named("scores") = scores,
      Rcpp::Named("vexpPC") = eigenvalues.head(ncomps) / totvar,
      Rcpp::Named("eigenvalues") = eigenvalues,
      Rcpp::Named("totvar") = totvar,
      Rcpp::Named("ncomps") = ncomps,
      Rcpp::Named("fat_matrix") = fat_matrix,
      Rcpp::Named("PM") = PM
    );
  } catch (std::exception& ex) {
    Rcpp::stop(std::string("pcaC: ") + ex.what());
  } catch (...) {
    Rcpp::stop("pcaC: unknown C++ exception");
  }
}
