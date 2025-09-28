#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include <time.h>

int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <N>\n", argv[0]);
        return 1;
    }

    int N = atoi(argv[1]);
    const int REPS = 100;

    double *A = (double*)malloc(N * N * sizeof(double));
    double *B = (double*)malloc(N * sizeof(double));
    double *C = (double*)calloc(N, sizeof(double));

    srand(42);
    for (int i = 0; i < N * N; i++) {
        A[i] = (double)rand() / RAND_MAX;
    }
    for (int i = 0; i < N; i++) {
        B[i] = (double)rand() / RAND_MAX;
    }

    double t1 = omp_get_wtime();
    for (int rep = 0; rep < REPS; rep++) {
        #pragma omp parallel for
        for (int i = 0; i < N; i++) {
            C[i] = 0.0;
            for (int j = 0; j < N; j++) {
                C[i] += A[i * N + j] * B[j];
            }
        }
    }

    double t2 = omp_get_wtime();
    double avg_time = (t2 - t1) / REPS;

    // Используем результат
    double checksum = 0.0;
    for (int i = 0; i < N; i++) checksum += C[i];
    (void)checksum; // подавляем предупреждение

    printf("%d %.9f\n", N, avg_time);

    free(A);
    free(B);
    free(C);
    return 0;
}
