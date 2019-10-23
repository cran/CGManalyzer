#include <R.h>
#include <Rinternals.h>

void Mse(char** cmdstrp);

static R_NativePrimitiveArgType Mse_t[] = {
  STRSXP
};

static const R_CMethodDef cMethods[] = {
  {"Mse", (DL_FUNC) &Mse, 1, Mse_t},
  { 0}
};

void
R_init_CGManalyzer(DllInfo *info)
{
  /* Register routines,
     allocate resources. */
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}

void
R_unload_CGManalyzer(DllInfo *info)
{
  /* Release resources. */
}
