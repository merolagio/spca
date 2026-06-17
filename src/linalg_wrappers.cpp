#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

#include "linalg_wrappers.h"  // get validators

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
  try {
    const int p = A.rows();
    MatrixXd M(p, p);
    M.noalias() = A * A.transpose();
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("aatC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("aatC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd ataC(const Eigen::Map<Eigen::MatrixXd>& A) {
  try {
    const int p = A.cols();
    MatrixXd M(p, p);
    //  M.noalias() = A.transpose() * A;
    M.noalias() = A.transpose() * A;
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("ataC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("ataC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd atbC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::MatrixXd>& B) {
  try {
    const int p = A.cols();
    const int q = B.cols();
    if (A.rows() != B.rows())
      stop("atbC: A and B must have the same number of rows");
    MatrixXd M(p, q);
    M.noalias() = A.transpose() * B;
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("atbC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("atbC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd abC(const Eigen::Map<Eigen::MatrixXd>& A,
                    const Eigen::Map<Eigen::MatrixXd>& B) {
  try {
    const int p = A.rows();
    const int q = B.cols();
    if (A.cols() != B.rows())
      stop("abC: A and B must be compatible for multiplication A * B");
    MatrixXd M(p, q);
    M.noalias() = A * B;
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("abC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("abC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd abtC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::MatrixXd>& B) {
  try {
    const int p = A.rows();
    const int q = B.rows();
    if (A.cols() != B.cols())
      stop("abtC: A and B must have the same number of columns");
    MatrixXd M(p, q);
    M.noalias() = A * B.transpose();
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("abtC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("abtC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd atdaC(const Eigen::Map<Eigen::MatrixXd>& A,
                      const Eigen::Map<Eigen::MatrixXd>& D) {
  try {
    const int p = A.cols();
    if (D.rows() != D.cols())
      stop("atdaC: D must be square");
    if (A.rows() != D.rows())
      stop("atdaC: A and D have incompatible dimensions");
    MatrixXd M(p, p);
    M.noalias() = A.transpose() * D * A;
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("atdaC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("atdaC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd atdbC(const Eigen::Map<Eigen::MatrixXd>& A,
                      const Eigen::Map<Eigen::MatrixXd>& D,
                      const Eigen::Map<Eigen::MatrixXd>& B) {
  try {
    const int p = A.cols();
    const int q = B.cols();
    if (D.rows() != D.cols())
      stop("atdbC: D must be square");
    if (A.rows() != D.rows() || B.rows() != D.cols())
      stop("atdbC: A, D, and B have incompatible dimensions");
    MatrixXd M(p, q);
    M.noalias() = A.transpose() * D * B;
    return M;
  }
  catch (std::exception& ex) {
    Rcpp::stop("atdbC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("atdbC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::VectorXd avC(const Eigen::Map<Eigen::MatrixXd>& A,
                    const Eigen::Map<Eigen::VectorXd>& v) {
  try {
    if (A.cols() != v.size())
      stop("avC: A and v have incompatible dimensions");
    return A * v;
  }
  catch (std::exception& ex) {
    Rcpp::stop("avC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("avC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::VectorXd atvC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::VectorXd>& v) {
  try {
    if (A.rows() != v.size())
      stop("atvC: A and v have incompatible dimensions");
    // returns (v' A)' as a column vector
    return A.transpose() * v;
  }
  catch (std::exception& ex) {
    Rcpp::stop("atvC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("atvC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::VectorXd vtaC(
    const Eigen::Map<Eigen::VectorXd>& v,
    const Eigen::Map<Eigen::MatrixXd>& A) {
  try {
    if (A.rows() != v.size())
      stop("vtaC: v and A have incompatible dimensions");
    return A.transpose() * v;
  }
  catch (std::exception& ex) {
    Rcpp::stop("vtaC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("vtaC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
double vtauC(const Eigen::Map<Eigen::VectorXd>& v,
             const Eigen::Map<Eigen::MatrixXd>& A,
             const Eigen::Map<Eigen::VectorXd>& u) {
  try {
    if (A.rows() != v.size() || A.cols() != u.size())
      stop("vtauC: v, A, and u have incompatible dimensions");
    return (v.dot(A * u));
  }
  catch (std::exception& ex) {
    Rcpp::stop("vtauC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("vtauC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
double vtvC(const Eigen::Map<Eigen::VectorXd>& v) {
  try {
    return v.squaredNorm();
  }
  catch (std::exception& ex) {
    Rcpp::stop("vtvC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("vtvC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
double vtuC(const Eigen::Map<Eigen::VectorXd>& v,
            const Eigen::Map<Eigen::VectorXd>& u) {
  try {
    if (v.size() != u.size())
      stop("vtuC: v and u must have the same length");
    return v.dot(u);
  }
  catch (std::exception& ex) {
    Rcpp::stop("vtuC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("vtuC failed: unknown C++ error");
  }
}


// [[Rcpp::export]]
double traceC(const Eigen::Map<Eigen::MatrixXd>& S) {
  try {
    return S.trace();
  }
  catch (std::exception& ex) {
    Rcpp::stop("traceC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("traceC failed: unknown C++ error");
  }
}


//======================================
//        helpers for lsspca
//======================================

// [[Rcpp::export]]
Rcpp::List EigenC(const Eigen::Map<Eigen::MatrixXd>& S) {
  try {
    if (S.cols() != S.rows())
      stop("S must be square");

    if ((S - S.transpose()).norm() > 1e-6 * S.norm())
      stop("S must be symmetric");

    SelfAdjointEigenSolver<MatrixXd> es(S);
    if (es.info() != Eigen::Success)
      Rcpp::stop("eigen decomposition failed");

    return List::create(
      Named("vec") = MatrixXd(es.eigenvectors()).rowwise().reverse(),
      Named("val") = VectorXd(es.eigenvalues()).reverse()
    );
  }
  catch (std::exception& ex) {
    Rcpp::stop("EigenC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("EigenC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::VectorXd comp_eigvalC(const Eigen::Map<Eigen::MatrixXd>& S) {
  try {
    if (S.cols() != S.rows())
      stop("S must be square");

    if ((S - S.transpose()).norm() > 1e-6 * S.norm())
      stop("S must be symmetric");

    SelfAdjointEigenSolver<MatrixXd> es(S, EigenvaluesOnly);
    if (es.info() != Eigen::Success)
      Rcpp::stop("eigenvalue decomposition failed");

    return VectorXd(es.eigenvalues().reverse());
  }
  catch (std::exception& ex) {
    Rcpp::stop("comp_eigvalC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("comp_eigvalC failed: unknown C++ error");
  }
}


// [[Rcpp::export]]
Rcpp::List GenEigenC(const Eigen::Map<Eigen::MatrixXd>& A,
                     const Eigen::Map<Eigen::MatrixXd>& B) {
  try {
    if (A.cols() != A.rows())
      stop("A must be square");
    if ((A - A.transpose()).norm() > 1e-6 * A.norm())
      stop("A must be symmetric");

    if (B.cols() != B.rows())
      stop("B must be square");
    if (A.rows() != B.rows())
      stop("A and B must have the same dimensions");
    if ((B - B.transpose()).norm() > 1e-6 * B.norm())
      stop("B must be symmetric");

    GeneralizedSelfAdjointEigenSolver<MatrixXd> es(A, B);
    if (es.info() != Eigen::Success)
      Rcpp::stop("generalized eigen decomposition failed");

    return List::create(
      Named("vec") = MatrixXd(es.eigenvectors()).rowwise().reverse(),
      Named("val") = VectorXd(es.eigenvalues()).reverse()
    );
  }
  catch (std::exception& ex) {
    Rcpp::stop("GenEigenC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("GenEigenC failed: unknown C++ error");
  }
}

// [[Rcpp::export]]
Eigen::MatrixXd solveC(const Eigen::Map<Eigen::MatrixXd>& S) {
  try {
    const int d = S.cols();
    if (S.cols() != S.rows())
      stop("S must be square");

    LLT<MatrixXd> llt(S);
    if (llt.info() != Eigen::Success)
      Rcpp::stop("LLT factorization failed; S must be positive definite");

    MatrixXd out = llt.solve(MatrixXd::Identity(d, d));
    if (!out.allFinite())
      Rcpp::stop("solve produced non-finite values");

    return out;
  }
  catch (std::exception& ex) {
    Rcpp::stop("solveC failed: " + std::string(ex.what()));
  }
  catch (...) {
    Rcpp::stop("solveC failed: unknown C++ error");
  }
}

