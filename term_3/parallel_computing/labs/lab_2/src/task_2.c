#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define ROWS 20
#define COLS 20

int main(int argc, char **argv)
{
    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (ROWS % size != 0) {
        if (rank == 0)
            fprintf(stderr, "Error: ROWS (%d) can't be divided by size (%d) returning whole number\n", ROWS, size);
        MPI_Finalize();
        return 1;
    }

    const int local_rows = ROWS / size;
    const int block = COLS / size;

    double *A_local = (double *)malloc(local_rows * COLS * sizeof(double));
    double *B_block = (double *)malloc(block * sizeof(double));
    double *C_local = (double *)calloc(local_rows, sizeof(double));

    for (int i = 0; i < local_rows; i++) {
        for (int j = 0; j < COLS; j++) {
            A_local[i * COLS + j] = 1.0;
        }
    }

    for (int i = 0; i < local_rows; i++) {
        for (int j = 0; j < COLS; j++) {
            C_local[i] += 3.0;
        }
    }
    for (int j = 0; j < block; j++) {
        B_block[j] = 2.0;
    }

    // Создание топологии "кольцо"
    int dims[1] = {0};
    int periods[1] = {1}; // периодическое кольцо
    MPI_Dims_create(size, 1, dims);
    MPI_Comm ring_comm;
    MPI_Cart_create(MPI_COMM_WORLD, 1, dims, periods, 0, &ring_comm);

    int src, dst;
    MPI_Cart_shift(ring_comm, 0, 1, &src, &dst);
    
    double start_time = MPI_Wtime();

    for (int step = 0; step < size; step++) {
        int col_offset = ((rank - step + size) % size) * block;

        // Накопление: C_local += A_local[:, col_offset : col_offset+block] * B_block
        for (int i = 0; i < local_rows; i++) {
            for (int j = 0; j < block; j++) {
                C_local[i] += A_local[i * COLS + col_offset + j] * B_block[j];
            }
        }

        // Циклический сдвиг блока вектора по кольцу
        if (size > 1) {
            MPI_Sendrecv_replace(B_block, block, MPI_DOUBLE, 
                                 dst, 0, src, 0, ring_comm, MPI_STATUS_IGNORE);
        }
    }

    double end_time = MPI_Wtime();
    double elapsed = end_time - start_time;

    // Сбор полного результата на процессе 0
    double *C_full = NULL;
    if (rank == 0) {
        C_full = (double *)malloc(ROWS * sizeof(double));
    }

    MPI_Gather(C_local, local_rows, MPI_DOUBLE,
               C_full, local_rows, MPI_DOUBLE,
               0, MPI_COMM_WORLD);

    printf("rank = %d: time = %.6f s\n", rank, elapsed);

    if (rank == 0) {
        printf("\nC = A * B (first 10 elements):\n");
        for (int i = 0; i < (ROWS < 10 ? ROWS : 10); i++) {
            printf("C[%d] = %.2f\n", i, C_full[i]);
        }
        free(C_full);
    }

    free(A_local);
    free(B_block);
    free(C_local);
    MPI_Comm_free(&ring_comm);
    MPI_Finalize();
    return 0;
}
