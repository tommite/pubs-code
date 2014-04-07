#include <R.h>
#include <Rmath.h>
#include <R_ext/RS.h>
#include <R_ext/BLAS.h>

#include <Rinternals.h>
#include <R_ext/Rdynload.h>

typedef struct Matrix {
	double * const data;
	int const nRow;
	int const nCol;
} Matrix;

/**
 * Get an element of a matrix.
 * @param i Row index.
 * @param j Column index.
 */
inline double *get(Matrix *m, int i, int j) {
	return m->data + j * (m->nRow) + i;
}

/**
 * Write a row to a matrix.
 */
inline void writeRow(Matrix *m, int i, double *x) {
	for (int j = 0; j < m->nCol; ++j) {
		*get(m, i, j) = x[j];
	}
}

/**
 * Partitions weights according to planes given in point/normal
 * representation.
 * 
 * @param N The number of points to partition
 * @param n Dimension of the points
 * @param points N * n matrix of points
 * @param x0 a point in the partitioning plane
 * @param normal Normal vector of the partitioning plane
 * @param partition An N-dim array of 0.0/1.0 indicating whether the weights belong to one of the two partitions (1.0 = on the plane or on the side of the normal)
 */
void partitionPointsForR(int *N, int *n, double* points, double* x0, double* normal, double* partition);
