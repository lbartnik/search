#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

struct EditDist {
  NumericMatrix dist;
  const CharacterVector & x, & y;

  EditDist(const CharacterVector & _x, const CharacterVector & _y)
    : dist(_x.size()+1, _y.size()+1), x(_x), y(_y)
  {}

  size_t compute_dist() {
    for (size_t i=0; i<=x.size(); ++i) {
      for (size_t j=0; j<=y.size(); ++j) {
        if (!i) {
          dist(i, j) = j;
        }
        else if (!j) {
          dist(i, j) = i;
        }
        else if (x[i-1] == y[j-1]) {
          dist(i, j) = dist(i-1, j-1);
        }
        else {
          dist(i, j) = 1 + std::min(dist(i, j-1),       // Insert
                             std::min(dist(i-1, j),     // Remove
                                      dist(i-1, j-1))); // Replace
        }
      }
    }

    return dist(x.size(), y.size());
  }
};

//' @param a tokenized R expression; output of `tokenize()`.
//' @param b tokenized R expression; output of `tokenize()`.
//'
//' @rdname edit_dist
// [[Rcpp::export]]
size_t edit_dist (const CharacterVector & a, const CharacterVector & b) {
  if (!a.inherits("tokens") || !b.inherits("tokens")) {
    stop("both arguments to edit_dist must be values returned from `tokenize`");
  }

  EditDist ed(a, b);
  return ed.compute_dist();
}
