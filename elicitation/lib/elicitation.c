#include "elicitation.h"

void partitionPointsForR(int *_N, int *_n, double* _points, double* x0, double* normal, double* partition) {
  int n = *_n;
  int N = *_N;
  Matrix points = {_points, N, n};
  
  for (int i=0;i<points.nRow;i++) {
    double sum = 0.0;
    for (int j=0;j<points.nCol;j++) {
      double ptVal = (*get(&points, i, j));
      double ptMinusX0 = ptVal - x0[j];
      sum += ptMinusX0 * normal[j];
    }
    partition[i] = (sum >= 0.0 ? 1.0 : 0.0);
  }
}

 
