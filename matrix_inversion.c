#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_cblas.h>

#include "matrix_inversion.h"

#define HAVE_INLINE

double *invert_matrix(int n, double *values) {
    int s;
    double *inverted_values = (double *) malloc (sizeof(double) * (n * n));

    gsl_matrix *matrix = gsl_matrix_alloc(n, n);
    gsl_matrix *inverse = gsl_matrix_alloc(n, n);
    gsl_permutation *perm = gsl_permutation_alloc(n);

    int i, j;
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            gsl_matrix_set(matrix, i, j, values[i * n + j]);
        }
    }

    gsl_linalg_LU_decomp(matrix, perm, &s);
    gsl_linalg_LU_invert(matrix, perm, inverse);

    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            inverted_values[i * n + j] = gsl_matrix_get(inverse, i, j);
        }
    }

    gsl_matrix_free(matrix);
    gsl_matrix_free(inverse);
    gsl_permutation_free(perm);

    return inverted_values;
}