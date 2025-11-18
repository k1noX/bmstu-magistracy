#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#define ROWS_A 31
#define COLS_A 20
#define COLS_B 31

int main(int argc, char **argv)
{
    int rank, size;
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    // Создание топологии "кольцо" (обязательно по условию)
    int dims[1] = {0};
    int periods[1] = {1};
    MPI_Dims_create(size, 1, dims);
    MPI_Comm ring_comm;
    MPI_Cart_create(MPI_COMM_WORLD, 1, dims, periods, 0, &ring_comm);

    // Неравномерное распределение строк A
    int local_rows = ROWS_A / size;
    int remainder = ROWS_A % size;
    if (rank < remainder) {
        local_rows++;  // первые 'остаток' процессов получают +1 строку
    }

    // Смещение начала строк для текущего процесса
    int offset = rank * (ROWS_A / size) + (rank < remainder ? rank : remainder);

    double *A_local = (double *)malloc(local_rows * COLS_A * sizeof(double));
    double *B = (double *)malloc(COLS_A * COLS_B * sizeof(double));
    double *C_local = (double *)calloc(local_rows * COLS_B, sizeof(double));

    // Инициализация данных
    for (int i = 0; i < local_rows; i++) {
        for (int j = 0; j < COLS_A; j++) {
            A_local[i * COLS_A + j] = (double)(offset + i + j + 1);
        }
    }

    for (int i = 0; i < COLS_A; i++) {
        for (int j = 0; j < COLS_B; j++) {
            B[i * COLS_B + j] = (double)(i + j + 2);
        }
    }

    // Локальное умножение
    double start_time = MPI_Wtime();

    for (int i = 0; i < local_rows; i++) {
        for (int k = 0; k < COLS_A; k++) {
            double a_ik = A_local[i * COLS_A + k];
            for (int j = 0; j < COLS_B; j++) {
                C_local[i * COLS_B + j] += a_ik * B[k * COLS_B + j];
            }
        }
    }

    double end_time = MPI_Wtime();
    double elapsed = end_time - start_time;

    int *recvcounts = NULL;
    int *displs = NULL;
    double *C_full = NULL;

    if (rank == 0) {
        recvcounts = (int *)malloc(size * sizeof(int));
        displs = (int *)malloc(size * sizeof(int));
        C_full = (double *)malloc(ROWS_A * COLS_B * sizeof(double));

        int total = 0;
        for (int r = 0; r < size; r++) {
            int lr = ROWS_A / size + (r < (ROWS_A % size) ? 1 : 0);
            recvcounts[r] = lr * COLS_B;
            displs[r] = total;
            total += recvcounts[r];
        }
    }

    int my_count = local_rows * COLS_B;
    MPI_Gatherv(C_local, my_count, MPI_DOUBLE,
                C_full, recvcounts, displs, MPI_DOUBLE,
                0, MPI_COMM_WORLD);

    printf("rank = %d: local_rows = %d, time = %.6f s\n", rank, local_rows, elapsed);

    if (rank == 0) {
        printf("\nC = A * B (first 5x5 elements):\n");
        for (int i = 0; i < 5; i++) {
            for (int j = 0; j < 5; j++) {
                printf("%8.1f ", C_full[i * COLS_B + j]);
            }
            printf("\n");
        }
        free(recvcounts);
        free(displs);
        free(C_full);
    }

    free(A_local);
    free(B);
    free(C_local);
    MPI_Comm_free(&ring_comm);
    MPI_Finalize();
    return 0;
}
