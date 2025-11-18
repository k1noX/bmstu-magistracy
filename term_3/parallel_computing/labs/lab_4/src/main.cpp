#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>

double vector_norm_diff(double *x_new, double *x_old, int n) {
    double sum = 0.0;
    #pragma omp parallel for reduction(+:sum)
    for (int i = 0; i < n; i++) {
        double diff = x_new[i] - x_old[i];
        sum += diff * diff;
    }
    return sqrt(sum);
}

int jacobi_method(double **A, double *b, double *x, int n, double tolerance, int max_iter) {
    double *x_new = (double*)malloc(n * sizeof(double));
    if (!x_new) {
        perror("malloc x_new");
        return -1;
    }

    int iter = 0;
    double error = tolerance + 1.0;

    while (iter < max_iter && error > tolerance) {
        #pragma omp parallel for
        for (int i = 0; i < n; i++) {
            double sum = 0.0;
            for (int j = 0; j < n; j++) {
                if (j != i) {
                    sum += A[i][j] * x[j];
                }
            }
            x_new[i] = (b[i] - sum) / A[i][i];
        }

        error = vector_norm_diff(x_new, x, n);

        #pragma omp parallel for
        for (int i = 0; i < n; i++) {
            x[i] = x_new[i];
        }

        iter++;
    }

    free(x_new);
    return iter;
}

double** allocate_matrix(int n) {
    double **A = (double**)malloc(n * sizeof(double*));
    for (int i = 0; i < n; i++) {
        A[i] = (double*)malloc(n * sizeof(double));
    }
    return A;
}

void free_matrix(double **A, int n) {
    for (int i = 0; i < n; i++) {
        free(A[i]);
    }
    free(A);
}

void create_diagonally_dominant_matrix(double **A, double *b, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i == j) {
                A[i][j] = 2.0 * n;
            } else {
                A[i][j] = 1.0;
            }
        }
        b[i] = 1.0;
    }
}

int main() {
    int n = 1000;
    double tolerance = 1e-6;
    int max_iter = 10000;

    double **A = allocate_matrix(n);
    double *b = (double*)malloc(n * sizeof(double));
    double *x = (double*)calloc(n, sizeof(double));

    if (!A || !b || !x) {
        perror("malloc");
        return 1;
    }

    create_diagonally_dominant_matrix(A, b, n);

    double start_time = omp_get_wtime();
    int iterations = jacobi_method(A, b, x, n, tolerance, max_iter);
    double end_time = omp_get_wtime();

    if (iterations == -1) {
        fprintf(stderr, "Error in Jacobi Method\n");
        return 1;
    }

    if (iterations >= max_iter) {
        printf("Exceeded max iteration count (%d). Accuracy have not met requirements.\n", max_iter);
    } else {
        printf("Found solution by %d iterations.\n", iterations);
    }

    printf("Time: %f s\n", end_time - start_time);

    printf("First 5 elements:\n");
    for (int i = 0; i < 5 && i < n; i++) {
        printf("x[%d] = %f\n", i, x[i]);
    }

    free_matrix(A, n);
    free(b);
    free(x);

    return 0;
}
