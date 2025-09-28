#include <omp.h>
#include <stdio.h>

#define M 10

int main()
{
    float A[M][M], b[M], c[M];
    int i, j, rank;

    for (i = 0; i < M; i++) {
        for (j = 0; j < M; j++) {
            A[i][j] = (j + 1) * 1.0;
        }
        b[i] = 1.0 * (i + 1);
        c[i] = 0.0;
    }

    printf("\nMatrix A and vector b:\n");
    for (i = 0; i < M; i++) {
        printf("  A[%d] = ", i);
        for (j = 0; j < M; j++) {
            printf("%.1f ", A[i][j]);
        }
        printf("  b[%d] = %.1f\n", i, b[i]);
    }

    #pragma omp parallel shared(A, b, c) private(rank, i)
    {
        rank = omp_get_thread_num();

        #pragma omp for private(j)
        for (i = 0; i < M; i++) {
            for (j = 0; j < M; j++) {
                c[i] += (A[i][j] * b[j]);
            }

            #pragma omp critical
            {
                printf(" rank = %d i = %d c[%d] = %.2f\n", rank, i, i, c[i]);
            }
        }
    }

    return 0;
}
