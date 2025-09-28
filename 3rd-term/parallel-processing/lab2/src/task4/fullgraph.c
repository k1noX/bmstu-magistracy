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

    double *A_local = (double*)malloc(local_rows * N * sizeof(double));
    double *B = (double*)malloc(N * sizeof(double));
    double *C_local = (double*)calloc(local_rows, sizeof(double));

    for (int i = 0; i < local_rows; i++)
        for (int j = 0; j < N; j++)
            A_local[i * N + j] = 1.0;

    for (int j = 0; j < N; j++)
        B[j] = 2.0;

    int *index = (int*)malloc(size * sizeof(int));
    int *edges = (int*)malloc(size * (size - 1) * sizeof(int));
    int pos = 0;
    for (int i = 0; i < size; i++) {
        index[i] = size - 1;
        for (int j = 0; j < size; j++) {
            if (i != j) edges[pos++] = j;
        }
    }

    MPI_Comm graph_comm;
    MPI_Graph_create(MPI_COMM_WORLD, size, index, edges, 0, &graph_comm);
    MPI_Comm_rank(graph_comm, &rank);

    MPI_Barrier(MPI_COMM_WORLD);
    double t1 = MPI_Wtime();

    for (int i = 0; i < local_rows; i++) {
        C_local[i] = 0.0;
        for (int j = 0; j < N; j++) {
            C_local[i] += A_local[i * N + j] * B[j];
        }
    }

    MPI_Allgather(C_local, local_rows, MPI_DOUBLE, B, local_rows, MPI_DOUBLE, graph_comm);

    double t2 = MPI_Wtime();
    double local_time = t2 - t1;

    double max_time;
    MPI_Reduce(&local_time, &max_time, 1, MPI_DOUBLE, MPI_MAX, 0, MPI_COMM_WORLD);

    if (rank == 0) {
        printf("graph %d %.6f\n", N, max_time);
    }

    free(index);
    free(edges);
    free(A_local);
    free(B);
    free(C_local);
    MPI_Comm_free(&graph_comm);
    MPI_Finalize();
    return 0;
}
