#include <Rcpp.h>
#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::interfaces(r, cpp)]] 

using namespace Rcpp;
using namespace Eigen;
using namespace std;


// multiple correlation of variable i with all others in X
void vifX_int(Eigen::MatrixXd X, int ind, double& vifout){
  try {
    // computes R2 for var with index ind
    // takes indices base 0
    int ind0 = ind;
    int p = X.cols();
    int n = X.rows();
    Eigen::VectorXd y(n); 
    Eigen::MatrixXd P(n, p - 1);
    
    if (p == 2){
      vifout = (X.col(0).transpose() * X.col(1))(0)/
        (X.col(0).norm() * X.col(1).norm());
      vifout = vifout * vifout; 
    }
    // creates matrix without col j maybe do with left/rightCols?
    if (p > 2){
      int k = 0;  
      for (int j = 0; j < p; ++j){
        if (j == ind0){
          y = X.col(j);
        }
        else{
          P.col(k) = X.col(j);
          k = k + 1;
        }
      }  
      Eigen::ColPivHouseholderQR<Eigen::MatrixXd> qr(P);
      
      // same pivots const Eigen::FullPivHouseholderQR<Eigen::MatrixXd> qr(X);
      
      const int r = qr.rank();
      const Eigen::MatrixXd Q = qr.matrixQ();
      
      vifout = ((Q.leftCols(r)).transpose() * y).squaredNorm()/
        y.squaredNorm(); 
    }
  }
  catch (std::exception &ex) {
    Rcpp::stop(ex.what());
  }
  catch (...) {
    Rcpp::stop("vifX_int: unknown C++ exception");
  }
}

// replaced with QR method, doesn't need intercept
// fixed in spca 2026
// makes vif for variables with index in ind
// takes indices base 1 from R
// [[Rcpp::export]]
Eigen::VectorXd vifXC(Eigen::Map<Eigen::MatrixXd> X, Eigen::VectorXi ind,
                      bool intercept = false){
  try {
    int p = X.cols();
    
    if (p == 1)
      Rcpp::stop("vifXC: must pass a matrix with at least 2 columns");
    
    int nind = ind.size();
    
    if (nind == 0)
      Rcpp::stop("vifXC: ind cannot be empty");
    if (nind > p)
      Rcpp::stop("vifXC: ind cannot contain more indices than columns of X");
    if (ind.minCoeff() < 1 || ind.maxCoeff() > p)
      Rcpp::stop("vifXC: indices out of range, the function takes indices base 1");
    
    Eigen::VectorXd vif(nind);
    for (int i = 0; i < nind; ++i){
      vifX_int(X, ind(i) - 1, vif(i));
    }
    
    vif = (1.0 - vif.array()).inverse();
    return(vif);
  }
  catch (std::exception &ex) {
    Rcpp::stop(ex.what());
  }
  catch (...) {
    Rcpp::stop("vifXC: unknown C++ exception");
  }
  return(Eigen::VectorXd());
}

// computes R2 for var with index ind
// takes indices base 0
void vifS_int(Eigen::MatrixXd S, int ind, double& vifout){
  try {
    int ind0 = ind;
    int p = S.cols();
    if (ind0 < 0 || ind0 > (p - 1))
      Rcpp::stop("vifS_int: index out of range");
    
    Eigen::MatrixXd T(p - 1, p - 1);
    Eigen::VectorXd s(p - 1), sc(p);
    if (ind0 == 0){
      T = S.bottomRightCorner(p - 1, p - 1);
      sc = S.col(0);
      s = sc.tail(p - 1);
    }
    if (ind0 == (p - 1)){
      T = S.topLeftCorner(p - 1, p - 1);
      sc = S.col(p - 1);
      s = sc.head(p - 1);
    }
    if((ind0 > 0) & (ind0 < (p - 1))){
      Eigen::MatrixXd A = S.topLeftCorner(ind0, ind0);
      Eigen::MatrixXd B = S.topRightCorner(ind0, p - ind0 - 1);
      Eigen::MatrixXd C = S.bottomRightCorner(p - ind0 - 1, p - ind0 - 1);
      
      T.topLeftCorner(ind0, ind0) = A;
      T.topRightCorner(ind0, p - ind0 - 1) = B;
      T.bottomLeftCorner(p - ind0 - 1, ind0) = B.transpose();
      T.bottomRightCorner(p - ind0 - 1, p - ind0 - 1) = C;
      sc = S.col(ind0);
      s.head(ind0) = sc.head(ind0);
      s.tail(p - ind0 - 1) = sc.tail(p - ind0 - 1);
    }
    const Eigen::MatrixXd Tm(T.llt().solve(MatrixXd::Identity(p - 1, p - 1)));
    
    vifout = (s.transpose() * Tm * s)(0, 0)/sc(ind0);
  }
  catch (std::exception &ex) {
    Rcpp::stop(ex.what());
  }
  catch (...) {
    Rcpp::stop("vifS_int: unknown C++ exception");
  }
}

// computes R2 for var with index ind
// takes indices base 0
void vifS_intPseudo(Eigen::MatrixXd S, int ind, double& vifout){
  try {
    int ind0 = ind;
    int p = S.cols();
    if (ind0 < 0 || ind0 > (p - 1))
      Rcpp::stop("vifS_intPseudo: index out of range");
    
    Eigen::MatrixXd T(p - 1, p - 1);
    Eigen::VectorXd s(p - 1), sc(p);
    if (ind0 == 0){
      T = S.bottomRightCorner(p - 1, p - 1);
      sc = S.col(0);
      s = sc.tail(p - 1);
    }
    if (ind0 == (p - 1)){
      T = S.topLeftCorner(p - 1, p - 1);
      sc = S.col(p - 1);
      s = sc.head(p - 1);
    }
    if((ind0 > 0) & (ind0 < (p - 1))){
      Eigen::MatrixXd A = S.topLeftCorner(ind0, ind0);
      Eigen::MatrixXd B = S.topRightCorner(ind0, p - ind0 - 1);
      Eigen::MatrixXd C = S.bottomRightCorner(p - ind0 - 1, p - ind0 - 1);
      
      T.topLeftCorner(ind0, ind0) = A;
      T.topRightCorner(ind0, p - ind0 - 1) = B;
      T.bottomLeftCorner(p - ind0 - 1, ind0) = B.transpose();
      T.bottomRightCorner(p - ind0 - 1, p - ind0 - 1) = C;
      sc = S.col(ind0);
      s.head(ind0) = sc.head(ind0);
      s.tail(p - ind0 - 1) = sc.tail(p - ind0 - 1);
    }
    
    SelfAdjointEigenSolver<Eigen::MatrixXd> es(T);
    
    // remember size S is p - 1
    int rank = 0;
    Eigen::VectorXd la = es.eigenvalues();
    Eigen::VectorXd lai(p - 1);
    for(int i = 0; i < p - 1; ++i){
      if(la(i) > 1e-5){
        rank = rank + 1;
        lai(p - i - 2) = 1/la(i);
      }
    }
    
    Eigen::MatrixXd V = es.eigenvectors().rightCols(rank);
    
    const Eigen::MatrixXd Tm = V * lai.head(rank).reverse().asDiagonal() * V.transpose(); 
    vifout = (s.transpose() * Tm * s)(0, 0)/sc(ind0);
  }
  catch (std::exception &ex) {
    Rcpp::stop(ex.what());
  }
  catch (...) {
    Rcpp::stop("vifS_intPseudo: unknown C++ exception");
  }
}

// [[Rcpp::export]]
Eigen::VectorXd vifSC(Eigen::Map<Eigen::MatrixXd> S, Eigen::VectorXi ind){
  try {
    // computes vif for variables in S, cov or cor matrix
    // takes indices base 1 from R
    int p = S.cols(), n = S.rows();
    
    if (p == 1)
      Rcpp::stop("vifSC: must pass a matrix with at least 2 columns");
    if (p != n)
      Rcpp::stop("vifSC: must pass a variance or correlation matrix");
    
    int nind = ind.size();
    
    if (nind == 0)
      Rcpp::stop("vifSC: ind cannot be empty");
    if (nind > p)
      Rcpp::stop("vifSC: ind cannot contain more indices than columns of S");
    if (ind.minCoeff() < 1 || ind.maxCoeff() > p)
      Rcpp::stop("vifSC: indices out of range, the function takes indices base 1");
    
    Eigen::VectorXd vif(nind);
    for (int i = 0; i < nind; ++i){
      vifS_int(S, ind(i) - 1, vif(i));
    }
    
    vif = (1.0 - vif.array()).inverse();
    return(vif);
  }
  catch (std::exception &ex) {
    Rcpp::stop(ex.what());
  }
  catch (...) {
    Rcpp::stop("vifSC: unknown C++ exception");
  }
  return(Eigen::VectorXd());
}

// [[Rcpp::export]]
Eigen::VectorXd vifSPseudoC(Eigen::Map<Eigen::MatrixXd> S, Eigen::VectorXi ind){
  try {
    // computes vif for variables in S, cov or cor matrix
    // takes indices base 1 from R
    int p = S.cols(), n = S.rows();
    
    if (p == 1)
      Rcpp::stop("vifSPseudoC: must pass a matrix with at least 2 columns");
    if (p != n)
      Rcpp::stop("vifSPseudoC: must pass a variance or correlation matrix");
    
    int nind = ind.size();
    
    if (nind == 0)
      Rcpp::stop("vifSPseudoC: ind cannot be empty");
    if (nind > p)
      Rcpp::stop("vifSPseudoC: ind cannot contain more indices than columns of S");
    if (ind.minCoeff() < 1 || ind.maxCoeff() > p)
      Rcpp::stop("vifSPseudoC: indices out of range, the function takes indices base 1");
    
    Eigen::VectorXd vif(nind);
    for (int i = 0; i < nind; ++i){
      vifS_intPseudo(S, ind(i) - 1, vif(i));
    }
    
    vif = (1.0 - vif.array()).inverse();
    return(vif);
  }
  catch (std::exception &ex) {
    Rcpp::stop(ex.what());
  }
  catch (...) {
    Rcpp::stop("vifSPseudoC: unknown C++ exception");
  }
  return(Eigen::VectorXd());
}
