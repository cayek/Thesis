#include <RcppEigen.h>
using namespace Eigen;

//' Y ~ X_j + CoVar for all j
// [[Rcpp::export]]
Rcpp::List phenotypeWayReg_lm(const Eigen::Map<Eigen::MatrixXd> & Y,
                              const Eigen::Map<Eigen::MatrixXd> & X,
                              const Eigen::Map<Eigen::MatrixXd> & CoVar) {

  //param
  const int n = Y.rows();
  const int L = X.cols();
  const int d = 1 + CoVar.cols();

  // res
  MatrixXd B(1, L);
  MatrixXd B_sigma2(1, L);

  // aux variable
  MatrixXd cov_aux(n, d);
  cov_aux.rightCols(d - 1) = CoVar;
  MatrixXd covt_cov_aux(d, d);
  MatrixXd covt_cov_inv_aux(d, d);
  double epsilon_sigma2_aux;
  MatrixXd B_aux(d,1);
  MatrixXd B_sigma2_aux(d,1);
  LDLT<MatrixXd> llt_aux;

  // main loop for all loci
  for (int j = 0; j < L; j++) {
    cov_aux.col(0) = X.col(j);
    covt_cov_aux = cov_aux.transpose() * cov_aux;

    llt_aux = (covt_cov_aux).ldlt();
    // compute B_aux
    B_aux = llt_aux.solve(cov_aux.transpose() * Y);

    // epsilon_sigma2
    epsilon_sigma2_aux = (Y - B_aux.transpose() * cov_aux).squaredNorm() / (n - d);

    // compute B.sigma2
    covt_cov_inv_aux = covt_cov_aux.inverse();
    B_sigma2_aux = covt_cov_inv_aux.diagonal() *
      epsilon_sigma2_aux;

    // res
    B(0,j) = B_aux(0,0);
    B_sigma2(0,j) = B_sigma2_aux(0,0);
  }
  
  return Rcpp::List::create(Rcpp::Named("B") = B,
                            Rcpp::Named("B.sigma2") = B_sigma2,
                            Rcpp::Named("a") = B_aux
                            );
}
