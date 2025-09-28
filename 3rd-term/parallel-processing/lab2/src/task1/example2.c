#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <time.h>
#include <sys/time.h>

#define M 20
#define N 5

#define DIMS 1

#define EL(x) (sizeof(x) / sizeof((x)[0]))

static double A[N][M], B[N], C[N];

int main(int argc, char **argv)
{
    int rank, size, i, j, k, i1, d, sour, dest;
    int dims[DIMS];
    int periods[DIMS];
    int reorder = 0;
    MPI_Comm ring;
    MPI_Status st;
    double t1, t2, rt;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    for (i = 0; i < DIMS; i++) {
        dims[i] = 0;
        periods[i] = 1;
    }

    MPI_Dims_create(size, DIMS, dims);
    MPI_Cart_create(MPI_COMM_WORLD, DIMS, dims, periods, reorder, &ring);
    MPI_Comm_rank(ring, &rank);
    MPI_Cart_shift(ring, 0, -1, &sour, &dest);

    for (j = 0; j < N; j++) {
        for (i = 0; i < M; i++) {
            A[j][i] = 3.0;
        }
        B[j] = 2.0;
        C[j] = 0.0;
    }

    t1 = MPI_Wtime();

    for (k = 0; k < size; k++) {
        d = ((rank + k) % size) * N;

        for (j = 0; j < N; j++) {
            for (i1 = 0, i = d; i < d + N; i++, i1++) {
                C[j] += A[j][i] * B[i1];
            }
        }

        MPI_Sendrecv_replace(B, EL(B), MPI_DOUBLE, dest, 12, sour, 12, ring, &st);
    }

    t2 = MPI_Wtime();
    rt = t2 - t1;

    printf("rank = %d Time = %f\n", rank, rt);

    for (i = 0; i < N; i++) {
        printf("rank = %d RM = %6.2f\n", rank, C[i]);
    }

    MPI_Comm_free(&ring);
    MPI_Finalize();
    return 0;
}
