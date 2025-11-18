#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

int main(int argc, char **argv)
{
    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (argc != 2) {
        if (rank == 0) fprintf(stderr, "Usage: %s <N>\n", argv[0]);
        MPI_Finalize();
        return 1;
    }

    int N = atoi(argv[1]);

    if (N % size != 0) {
        if (rank == 0) fprintf(stderr, "N must be divisible by number of processes!\n");
        MPI_Finalize();
        return 1;
    }

    const int local_rows = N / size;
    const int block = N / size;

    double *A_local = (double*)malloc(local_rows * N * sizeof(double));
    double *B_block = (double*)malloc(block * sizeof(double));
    double *C_local = (double*)calloc(local_rows, sizeof(double));

    for (int i = 0; i < local_rows; i++)
        for (int j = 0; j < N; j++)
            A_local[i * N + j] = 3.0;

    for (int j = 0; j < block; j++)
        B_block[j] = 2.0;

    int dims[1] = {0};
    int periods[1] = {1};
    MPI_Dims_create(size, 1, dims);
    MPI_Comm ring;
    MPI_Cart_create(MPI_COMM_WORLD, 1, dims, periods, 0, &ring);

    int src, dst;
    MPI_Cart_shift(ring, 0, 1, &src, &dst);

    MPI_Barrier(MPI_COMM_WORLD);
    double t1 = MPI_Wtime();

    for (int step = 0; step < size; step++) {
        int col_offset = ((rank - step + size) % size) * block;
        for (int i = 0; i < local_rows; i++) {
            for (int j = 0; j < block; j++) {
                C_local[i] += A_local[i * N + col_offset + j] * B_block[j];
            }
        }
        if (size > 1) {
            MPI_Sendrecv_replace(B_block, block, MPI_DOUBLE, dst, 0, src, 0, ring, MPI_STATUS_IGNORE);
        }
    }

    double t2 = MPI_Wtime();
    double local_time = t2 - t1;

    double max_time;
    MPI_Reduce(&local_time, &max_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        printf("ring %d %.6f\n", N, max_time);
    }

    free(A_local);
    free(B_block);
    free(C_local);
    MPI_Comm_free(&ring);
    MPI_Finalize();
    return 0;
}
