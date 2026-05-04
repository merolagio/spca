#ifndef SUPPORT_TALL_H
#define SUPPORT_TALL_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include "support_shared.h"

double deflSC(const Eigen::Ref<const Eigen::VectorXd>& a,
              Eigen::MatrixXd& G,
              Eigen::VectorXi& ind,
              double& vexp,
              double eps = 1e-5);

double deflSandDC(const Eigen::Ref<const Eigen::VectorXd>& a,
                  Eigen::MatrixXd& G,
                  Eigen::MatrixXd& M,
                  Eigen::VectorXi& ind,
                  double& vexp);

Rcpp::List makeVexpSC(const Eigen::Ref<const Eigen::MatrixXd>& A,
                      const Eigen::Ref<const Eigen::MatrixXd>& S);

double makeCvexpOneCompSC_int(const Eigen::Ref<const Eigen::MatrixXd>& A,
                              const Eigen::Ref<const Eigen::MatrixXd>& S,
                              double& cvexp);

void uspcaC(const Eigen::Ref<const Eigen::MatrixXd>& S,
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
            bool PM = false,
            double epsPM = 1e-5,
            int maxiter = 100);

void cspcaC(const Eigen::Ref<const Eigen::MatrixXd>& S,
            Eigen::MatrixXd& M,
            double& eigval,
            int p,
            int comp_number,
            Eigen::VectorXi& indj,
            int cardt,
            Eigen::VectorXd& a,
            bool& singular,
            bool PM = false,
            double epsPM = 1e-5,
            int maxiter = 100);

void pspcaC(const Eigen::Ref<const Eigen::MatrixXd>& S,
            const Eigen::VectorXd& si,
            Eigen::VectorXi& indj,
            int cardt,
            Eigen::VectorXd& a,
            bool& singular);

int varsel_fbsC(const Eigen::Ref<const Eigen::MatrixXd>& S,
                const Eigen::VectorXd& si,
                Eigen::VectorXi& indices,
                int& cardinality,
                double& criterion_value,
                Eigen::VectorXd& loadings_out,
                double& vexp_out,
                double alpha,
                int selection_method,
                int stop_criterion,
                bool intensive,
                bool exact_cvexp,
                double pc_vexp,
                double target_cvexp,
                Eigen::VectorXi force_in,
                Eigen::VectorXi force_out,
                int mincard,
                Eigen::MatrixXd& M,
                double prev_cvexp,
                int comp_number,
                Eigen::MatrixXd& B,
                int ntrim,
                int reducetrim,
                double rank_tol,
                bool PMSPC = false,
                double epsPMSPC = 1e-5,
                int maxiterPMSPC = 100);


#endif
