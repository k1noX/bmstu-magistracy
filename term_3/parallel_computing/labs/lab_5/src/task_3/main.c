#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


int main(int argc, char** argv) {
    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (argc < 2) {
        if (rank == 0) {
            fprintf(stderr, "Usage: %s <matrix_size>\n", argv[0]);
        }
        MPI_Finalize();
        return 1;
    }

    int n = atoi(argv[1]);
    if (n <= 0) {
        if (rank == 0) {
            fprintf(stderr, "Error: matrix size must be a positive integer.\n");
        }
        MPI_Finalize();
        return 1;
    }

    int local_n = (n / size) + ((rank < n % size) ? 1 : 0);

    double *A = (double*)malloc(local_n * n * sizeof(double));
    double *b = (double*)malloc(local_n * sizeof(double));
    double *x = (double*)calloc(n, sizeof(double));

    int idx = 0;
    for (int global_i = 0; global_i < n; global_i++) {
        if (global_i % size != rank) continue;

        for (int j = 0; j < n; j++) {
            A[idx * n + j] = 0.0;
        }

        if (global_i == 0) {
            A[idx * n + 0] = 4.0;
            A[idx * n + 1] = 1.0;
            b[idx] = 9.0;
        } else if (global_i == n - 1) {
            A[idx * n + n - 2] = 1.0;
            A[idx * n + n - 1] = 4.0;
            b[idx] = 5.0;
        } else {
            A[idx * n + global_i - 1] = 1.0;
            A[idx * n + global_i    ] = 4.0;
            A[idx * n + global_i + 1] = 1.0;
            b[idx] = (global_i == 1) ? 7.0 : 6.0;
        }
        idx++;
    }

    double start_time = MPI_Wtime();
    for (int k = 0; k < n; k++) {
        int owner = k % size;
        int local_k = k / size;

        if (rank == owner) {
            if (local_k >= local_n) {
                MPI_Finalize();
                return 1;
            }

            double pivot = A[local_k * n + k];
            for (int j = k; j < n; j++) {
                A[local_k * n + j] /= pivot;
            }
            b[local_k] /= pivot;

            for (int p = 0; p < size; p++) {
                if (p == owner) continue;
                MPI_Send(&A[local_k * n + k], n - k, MPI_DOUBLE, p, 0, MPI_COMM_WORLD);
                MPI_Send(&b[local_k], 1, MPI_DOUBLE, p, 0, MPI_COMM_WORLD);
            }
        } else {
            double *pivot_row = (double*)malloc((n - k) * sizeof(double));
            double pivot_b;
            MPI_Recv(pivot_row, n - k, MPI_DOUBLE, owner, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            MPI_Recv(&pivot_b, 1, MPI_DOUBLE, owner, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

            for (int i = 0; i < local_n; i++) {
                int global_i = (i * size + rank);
                if (global_i <= k) continue;

                double factor = A[i * n + k];
                for (int j = k; j < n; j++) {
                    A[i * n + j] -= factor * pivot_row[j - k];
                }
                b[i] -= factor * pivot_b;
            }
            free(pivot_row);
        }
    }

    for (int k = n - 1; k >= 0; k--) {
        int owner = k % size;
        int local_k = k / size;

        if (rank == owner) {
            if (local_k >= local_n) {
                MPI_Finalize();
                return 1;
            }

            double sum = b[local_k];
            for (int j = k + 1; j < n; j++) {
                sum -= A[local_k * n + j] * x[j];
            }
            x[k] = sum;

            for (int p = 0; p < size; p++) {
                if (p == owner) continue;
                MPI_Send(&x[k], 1, MPI_DOUBLE, p, 0, MPI_COMM_WORLD);
            }
        } else {
            MPI_Recv(&x[k], 1, MPI_DOUBLE, owner, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
    }

    double end_time = MPI_Wtime();
    double total_time = end_time - start_time;

    if (rank == 0) {
        printf("Solution x[:8]:\n");
        for (int i = 0; i < 8; i++) {
            printf("x[%d] = %.6f\n", i, x[i]);
        }
        printf("Total execution time: %.6f seconds\n", total_time);
    }

    free(A);
    free(b);
    free(x);
    MPI_Finalize();
    return 0;
}
