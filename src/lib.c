// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

#include <assert.h>
#include <math.h>

SEXP C_unwrap_array (SEXP _array, SEXP _dAlpha, SEXP _rMax, SEXP _dR, SEXP _missing);
SEXP C_cdf_diff (SEXP _x, SEXP _y);
static int is_single_numeric(SEXP _obj);
static int is_numeric (SEXP _obj);

static const R_CallMethodDef callMethods[]  = {
  { "C_unwrap_array", (DL_FUNC) &C_unwrap_array, 5 },
  { "C_cdf_diff",     (DL_FUNC) &C_cdf_diff, 2 },
  { NULL, NULL, 0 }
};

void R_init_subprocess(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

SEXP C_unwrap_array (SEXP _array, SEXP _dAlpha, SEXP _rMax, SEXP _dR, SEXP _missing) {
  if (!isArray(_array) || !isReal(_array)) {
    Rf_error("`_array` has to be a two-dimensional numeric array");
  }

  SEXP dims = PROTECT(Rf_getAttrib(_array, R_DimSymbol));
  if (!isInteger(dims) || Rf_length(dims) != 2) {
    UNPROTECT(1);
    Rf_error("`_array` has to have exactly two dimensions");
  }
  UNPROTECT(1);

  if (!is_single_numeric(_dAlpha)) {
    Rf_error("`_dAlpha` needs to be a single numeric value");
  }
  if (!is_single_numeric(_rMax)) {
    Rf_error("`_rMax` needs to be a single numeric value");
  }
  if (!is_single_numeric(_dR)) {
    Rf_error("`_dR` needs to be a single numeric value");
  }
  if (!is_single_numeric(_missing)) {
    Rf_error("`_missing` needs to be a single numeric value");
  }

  double * array = NUMERIC_DATA(_array);
  double dAlpha = NUMERIC_DATA(_dAlpha)[0];
  double rMax = NUMERIC_DATA(_rMax)[0];
  double dR = NUMERIC_DATA(_dR)[0];
  double missing = NUMERIC_DATA(_missing)[0];

  int iCols = Rf_ncols(_array);
  int iRows = Rf_nrows(_array);

  int oRows = rMax/dR, oCols = 2*M_PI/dAlpha;
//  printf("oRows = %d, oCols = %d\n", oRows, oCols);

  SEXP ans = PROTECT(allocMatrix(REALSXP, oRows, oCols));
  double * output = NUMERIC_DATA(ans);

  // iterate over alpha and r
  double alpha = 0;
  for (int oCol = 0; oCol < oCols; ++oCol, alpha += dAlpha) {
    assert(alpha <= M_PI * 2);
    double cosAlpha = cos(alpha), sinAlpha = sin(alpha);

    double r = 0;
    for (int oRow = 0; oRow < oRows; ++oRow, r += dR) {
      int iCol = iCols/2 + cosAlpha * r,
          iRow = iRows/2 - sinAlpha * r;

//      printf("output[%d, %d] -> (%f, %f) -> array[%d, %d]", oRow, oCol, alpha, r, iRow, iCol);
      if (0 <= iCol && iCol < iCols && 0 <= iRow && iRow < iRows) {
        double value = array[iRow + iCol * iRows];
//        printf(" = %f\n", value);
        output[oRow + oCol * oRows] = value;
      } else {
        output[oRow + oCol * oRows] = missing;
//        printf(" out-of-bound\n");
      }
    }
  }

  UNPROTECT(1);
  return ans;
}

SEXP C_cdf_diff (SEXP _x, SEXP _y) {
  if (!is_numeric(_x)) {
    Rf_error("`_x` needs to be a sorted numeric vector");
  }
  if (!is_numeric(_y)) {
    Rf_error("`_y` needs to be a sorted numeric vector");
  }

  int lx = LENGTH(_x), ly = LENGTH(_y);
  double * x = NUMERIC_DATA(_x), * y = NUMERIC_DATA(_y);
  double * px = x, * py = y, * ex = x+lx, * ey = y+ly,
           vx = 0, vy = 0, diff = 0;
  double cx = *px < *py ? *px : *py;

#define DEBUG_PRINT do { printf("cx=%f d=%f x=%f y=%f vx=%f vy=%f\n", cx, diff, *px, *py, vx, vy); } while (0);

  #define DO_X do {                                              \
    diff += (*px - cx) * fabs(vx - vy);                          \
    cx = *px;                                                    \
    ++px;                                                        \
    vx += 1.0/lx;                                                \
  } while (0);                                                   \

  #define DO_Y do {                                              \
    diff += (*py - cx) * fabs(vx - vy);                          \
    cx = *py;                                                    \
    ++py;                                                        \
    vy += 1.0/ly;                                                \
  } while (0);                                                   \

  while (px < ex || py < ey) {
    // 1. find the next point to measure cdf against: either px or py
    // 2. delta is the area between this new point and values of both cdfs
    if (py == ey) {
      DO_X;
      continue;
    }
    if (px == ex) {
      DO_Y;
      continue;
    }

    if (*px < *py) {
      DO_X;
    } else {
      DO_Y;
    }
  }

  SEXP ans = PROTECT(NEW_NUMERIC(1));
  NUMERIC_DATA(ans)[0] = diff;

  UNPROTECT(1);
  return ans;
}



static int is_single_numeric (SEXP _obj) {
  return isReal(_obj) && (LENGTH(_obj) == 1);
}

static int is_numeric (SEXP _obj) {
  return isReal(_obj);
}
