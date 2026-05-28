#ifndef SUPPORT_SHARED_V3_H
#define SUPPORT_SHARED_V3_H

#include <RcppEigen.h>

Eigen::MatrixXd makeCorComp_int(const Eigen::Ref<const Eigen::MatrixXd>& A,
                                const Eigen::Ref<const Eigen::MatrixXd>& S,
                                int ncomps);

Eigen::MatrixXd makeSubS(const Eigen::Ref<const Eigen::MatrixXd>& S,
                         Eigen::VectorXi& e);

void maked_loopColF(const Eigen::Ref<const Eigen::MatrixXd>& S,
                    Eigen::MatrixXd& M,
                    Eigen::VectorXi& e);

void makeXdC(const Eigen::Ref<const Eigen::MatrixXd>& X,
             const Eigen::Ref<const Eigen::VectorXi>& ind,
             Eigen::MatrixXd& Xd);

Eigen::MatrixXd CorCompC(const Eigen::Ref<const Eigen::MatrixXd>& A,
                         const Eigen::Ref<const Eigen::MatrixXd>& S,
                         int d = 0);
Eigen::MatrixXd cor_int(const Eigen::Ref<const Eigen::MatrixXd>& X,
                        bool center = true,
                        bool scale = true);

Eigen::VectorXd eigvecPMC(const Eigen::MatrixXd& X,
                          double& val,
                          double eps = 1e-5,
                          int maxiter = 100);

Eigen::VectorXd GeigvecPMC(const Eigen::MatrixXd& A,
                           const Eigen::MatrixXd& B,
                           double& val,
                           double eps = 1e-5,
                           int maxiter = 100);

#endif
