#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

#define N 500
#define K 300
#define M 400

int main()
{
    double (*A)[K] = (double(*)[K])malloc(N * K * sizeof(double));
    double (*B)[M] = (double(*)[M])malloc(K * M * sizeof(double));
    double (*C)[M] = (double(*)[M])malloc(N * M * sizeof(double));

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < K; j++) {
            A[i][j] = i + j + 1;
        }
    }
    for (int i = 0; i < K; i++) {
        for (int j = 0; j < M; j++) {
            B[i][j] = i - j + 2;
        }
    }

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < M; j++) {
            C[i][j] = 0.0;
        }
    }

    double start = omp_get_wtime();

    #pragma omp parallel for
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < M; j++) {
            for (int k = 0; k < K; k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }

    double end = omp_get_wtime();

    printf("Time elapsed - %.4f s\n", end - start);
    printf("C[0][0] = %.2f\n", C[0][0]);

    free(A);
    free(B);
    free(C);

    return 0;
}
