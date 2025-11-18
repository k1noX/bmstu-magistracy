#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <time.h>
#include <sys/time.h>

#define M 20
#define N 5

static double A[N][M], B[M], C[N];

int main(int argc, char **argv)
{
    int rank, size;
    int *index, *edges;
    int i, j;
    int reord = 0;
    MPI_Comm comm_gr;
    double t1, t2, rt;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    index = (int *)malloc(size * sizeof(int));
    edges = (int *)malloc(size * (size - 1) * sizeof(int));

    int edge_pos = 0;
    for (i = 0; i < size; i++) {
        index[i] = size - 1;
        for (j = 0; j < size; j++) {
            if (j != i) {
                edges[edge_pos++] = j;
            }
        }
    }

    MPI_Graph_create(MPI_COMM_WORLD, size, index, edges, reord, &comm_gr);
    MPI_Comm_rank(comm_gr, &rank);

    for (j = 0; j < N; j++) {
        for (i = 0; i < M; i++) {
            A[j][i] = 3.0;
        }
        C[j] = 0.0;
    }
    for (i = 0; i < M; i++) {
        B[i] = 2.0;
    }

    t1 = MPI_Wtime();

    for (j = 0; j < N; j++) {
        for (i = 0; i < M; i++) {
            C[j] += A[j][i] * B[i];
        }
    }

    MPI_Allgather(C, N, MPI_DOUBLE, B, N, MPI_DOUBLE, comm_gr);

    t2 = MPI_Wtime();
    rt = t2 - t1;

    printf("rank = %d Time = %f\n", rank, rt);
    if (rank == 0) {
        for (i = 0; i < M; i++) {
            printf("B = %6.2f\n", B[i]);
        }
    }

    free(index);
    free(edges);
    MPI_Comm_free(&comm_gr);
    MPI_Finalize();
    return 0;
}
