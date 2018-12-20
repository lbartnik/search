#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;


NumericVector stl_sort(NumericVector x) {
  NumericVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y;
}

// [[Rcpp::export]]
NumericMatrix unwrap_array_impl (const NumericMatrix & array, double dAlpha, double rMax, double dR, double missing) {

  int iCols = Rf_ncols(array);
  int iRows = Rf_nrows(array);

  const int oRows = rMax/dR, oCols = 2*M_PI/dAlpha;
  // printf("oRows = %d, oCols = %d\n", oRows, oCols);

  NumericMatrix ans(oRows, oCols);

  // iterate over alpha and r
  double alpha = 0;
  for (int oCol = 0; oCol < oCols; ++oCol, alpha += dAlpha) {
    assert(alpha <= M_PI * 2);
    double cosAlpha = std::cos(alpha), sinAlpha = std::sin(alpha);

    double r = 0;
    for (int oRow = 0; oRow < oRows; ++oRow, r += dR) {
      int iCol = iCols/2 + cosAlpha * r,
          iRow = iRows/2 - sinAlpha * r;

      // printf("output[%d, %d] -> (%f, %f) -> array[%d, %d]", oRow, oCol, alpha, r, iRow, iCol);
      if (0 <= iCol && iCol < iCols && 0 <= iRow && iRow < iRows) {
        double value = array(iRow, iCol);
        // printf(" = %f\n", value);
        ans(oRow, oCol) = value;
      } else {
        ans(oRow, oCol) = missing;
        // printf(" out-of-bound\n");
      }
    }
  }

  return ans;
}


// [[Rcpp::export]]
double cdf_area (const NumericVector & a, const NumericVector & b) {
  NumericVector x = stl_sort(a);
  NumericVector y = stl_sort(b);

  const double * px = x.begin(),
               * py = y.begin();
  double vx = 0, vy = 0, diff = 0;
  double cx = *px < *py ? *px : *py;

#define DEBUG_PRINT do { printf("cx=%f d=%f x=%f y=%f vx=%f vy=%f\n", cx, diff, *px, *py, vx, vy); } while (0);

  #define DO_X do {                                              \
    diff += (*px - cx) * fabs(vx - vy);                          \
    cx = *px;                                                    \
    ++px;                                                        \
    vx += 1.0/x.size();                                          \
  } while (0);                                                   \

  #define DO_Y do {                                              \
    diff += (*py - cx) * fabs(vx - vy);                          \
    cx = *py;                                                    \
    ++py;                                                        \
    vy += 1.0/y.size();                                          \
  } while (0);                                                   \

  while (px < x.end() || py < y.end()) {
    // 1. find the next point to measure cdf against: either px or py
    // 2. delta is the area between this new point and values of both cdfs
    if (py == y.end()) {
      DO_X;
      continue;
    }
    if (px == x.end()) {
      DO_Y;
      continue;
    }

    if (*px < *py) {
      DO_X;
    } else {
      DO_Y;
    }
  }

  #undef DO_X
  #undef DO_Y

  return diff;
}


// [[Rcpp::export]]
double cdf_max (const NumericVector & a, const NumericVector & b) {
  NumericVector x = stl_sort(a);
  NumericVector y = stl_sort(b);

  const double * px = x.begin(), * py = y.begin();
  double vx = 0, vy = 0, max_diff = 0;
  double cx = *px < *py ? *px : *py;

  #define DEBUG_PRINT do { printf("cx=%f d=%f x=%f y=%f vx=%f vy=%f\n", cx, diff, *px, *py, vx, vy); } while (0);

  #define DO_X do {                                              \
    ++px;                                                        \
    vx += 1.0/x.size();                                          \
  } while (0);                                                   \

  #define DO_Y do {                                              \
    ++py;                                                        \
    vy += 1.0/y.size();                                          \
  } while (0);                                                   \

  while (px < x.end() || py < y.end()) {
    // 1. find the next point to measure cdf against: either px or py
    // 2. delta is the area between this new point and values of both cdfs
    double tmp = fabs(vx - vy);
    if (tmp > max_diff) max_diff = tmp;

    if (py == y.end()) {
      DO_X;
      continue;
    }
    if (px == x.end()) {
      DO_Y;
      continue;
    }

    if (*px < *py) {
      DO_X;
    } else {
      DO_Y;
    }
  }

  #undef DO_X
  #undef DO_Y

  return max_diff;
}
