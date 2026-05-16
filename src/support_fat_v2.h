#ifndef SUPPORT_FAT_V2_H
#define SUPPORT_FAT_V2_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include "support_shared_v2.h"

double compute_r2_subset_T(const Eigen::Ref<const Eigen::MatrixXd>& X,
                           const Eigen::VectorXd& u,
                           const Eigen::VectorXi& indj,
                           int cardt);

Rcpp::List PMAllEigen_fat(const Eigen::Ref<const Eigen::MatrixXd>& X,
                          int ncomps,
                          double epsPM,
                          int maxiterPM);

double compute_cvexp_T(const Eigen::MatrixXd& scores_all,
                       int ncomps,
                       const Eigen::MatrixXd& D_orig,
                       double totvexp);

void compute_vexp_cvexp_T_exact(const Eigen::MatrixXd& scores,
                                int nc,
                                const Eigen::MatrixXd& D_orig,
                                double totvexp,
                                Eigen::VectorXd& vexp,
                                Eigen::VectorXd& cvexp);

void deflT_rank1(const Eigen::Ref<const Eigen::VectorXd>& x,
                 Eigen::MatrixXd& K,
                 double& vexp);

void deflT_F(Eigen::MatrixXd PC,
             Eigen::MatrixXd& K,
             double curvexp,
             double& vexp);

void cspcaTC(const Eigen::Ref<const Eigen::MatrixXd>& X,
             const Eigen::Ref<const Eigen::MatrixXd>& K,
             double& eigval,
             const Eigen::VectorXi& indj,
             int cardt,
             Eigen::VectorXd& a,
             Eigen::MatrixXd& scores,
             int comp_number,
             bool PM,
             double epsPM = 1e-5,
             int maxiter = 100);

void uspcaTC(const Eigen::Ref<const Eigen::MatrixXd>& X,
             const Eigen::Ref<const Eigen::MatrixXd>& T,
             double& eigval,
             const Eigen::VectorXi& indj,
             int cardt,
             Eigen::VectorXd& a,
             Eigen::MatrixXd& scores,
             int comp_number,
             bool PM = false,
             double epsPM = 1e-5,
             int maxiter = 100);

void pspcaTC(const Eigen::Ref<const Eigen::MatrixXd>& X,
             const Eigen::VectorXd& r,
             const Eigen::VectorXi& indj,
             int cardt,
             Eigen::VectorXd& a,
             Eigen::MatrixXd& scores,
             int comp_number);

bool fwd_selectT(const Eigen::Ref<const Eigen::MatrixXd>& X,
                 const Eigen::MatrixXd& D,
                 const Eigen::MatrixXd& D_orig,
                 double totvexp,
                 const Eigen::VectorXd& u,
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
                 double epsPM = 1e-5,
                 int maxiterPM = 100);

Rcpp::List varsel_cvexp_T_wrap(Eigen::MatrixXd X,
                               double alpha = 0.95,
                               int mincard = 1,
                               int varsel_method = 0,
                               int stop_rule = 1,
                               bool exact_cvexp = false,
                               bool PM = true,
                               double epsPM = 1e-5,
                               int maxiterPM = 150);

#endif
