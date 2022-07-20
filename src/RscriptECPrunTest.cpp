#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector e_divisivecpp(NumericMatrix mat, int N, double P) {
  int nrow = mat.nrow();
  IntegerVector RunECP(nrow);
  Environment ecp = Environment::namespace_env("ecp");//importation de la fonction e.divisive du package ECP
  Function edivisive = ecp["e.divisive"];
  Environment utils = Environment::namespace_env("utils");
  Function txtProgressBar = utils["txtProgressBar"];
  Function setTxtProgressBar = utils["setTxtProgressBar"];
  CharacterVector ch = colnames(mat);
 for(int i = 0; i<nrow; i++){
    NumericVector vec = mat(i,_);
    vec.attr("dim") = Dimension(1,vec.length());
    NumericMatrix mvec = as<NumericMatrix>(vec);
    NumericMatrix tmvec = transpose(mvec);
    rownames(tmvec) = ch;
    List res = edivisive(tmvec, Named("sig.lvl")=P, _["min.size"]=N);
    IntegerVector vres = res["estimates"];//on recupere le resultat estimates de e.divisive
    RunECP[i] = vres.length();
    setTxtProgressBar(txtProgressBar(Named("min") = 1, _["max"] = nrow, _["style"] = 3),i);
  }
  return RunECP;
}
