#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char **argv)
{
    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (size != 2) {
        if (rank == 0) fprintf(stderr, "Execute with 2 processes!\n");
        MPI_Finalize();
        return 1;
    }

    if (argc != 2) {
        if (rank == 0) fprintf(stderr, "Usage: %s <N>\n", argv[0]);
        MPI_Finalize();
        return 1;
    }

    int N = atoi(argv[1]);
    int local_rows = N / size + (rank < N % size ? 1 : 0);
    int offset = rank * (N / size) + (rank < N % size ? rank : N % size);

    double *A_local = (double*)malloc(local_rows * N * sizeof(double));
    double *B = (double*)malloc(N * sizeof(double));
    double *C_local = (double*)calloc(local_rows, sizeof(double));

    for (int i = 0; i < local_rows; i++)
        for (int j = 0; j < N; j++)
            A_local[i * N + j] = 1.0;

    for (int j = 0; j < N; j++)
        B[j] = 2.0;

    MPI_Barrier(MPI_COMM_WORLD);
    double t1 = MPI_Wtime();

    for (int i = 0; i < local_rows; i++) {
        C_local[i] = 0.0;
        for (int j = 0; j < N; j++) {
            C_local[i] += A_local[i * N + j] * B[j];
        }
    }

    double t2 = MPI_Wtime();
    double local_time = t2 - t1;

    double max_time;
    MPI_Reduce(&local_time, &max_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        printf("%d %.6f\n", N, max_time);
    }

    free(A_local);
    free(B);
    free(C_local);
    MPI_Finalize();
    return 0;
}
