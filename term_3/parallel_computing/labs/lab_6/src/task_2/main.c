#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>

#define X 2.0
#define Y 2.0
#define Z 2.0
#define EPS 1e-5

double Fresh(double x, double y, double z) {
    return x*x + y*y + z*z;
}

double Ro(double x, double y, double z) {
    return 6.0;
}

int main(int argc, char** argv) {
    MPI_Init(&argc, &argv);
    int rank, size;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (argc < 4) {
        if (rank == 0) {
            fprintf(stderr, "Usage: %s <in> <jn> <kn>\n", argv[0]);
        }
        MPI_Finalize();
        return 1;
    }

    int in = atoi(argv[1]);
    int jn = atoi(argv[2]);
    int kn = atoi(argv[3]);

    if (in <= 0 || jn <= 0 || kn <= 0) {
        if (rank == 0) {
            fprintf(stderr, "Error: all dimensions must be positive integers.\n");
        }
        MPI_Finalize();
        return 1;
    }

    double hx = X / in;
    double hy = Y / jn;
    double hz = Z / kn;
    double owx = hx * hx;
    double owy = hy * hy;
    double owz = hz * hz;
    double c = 2.0/owx + 2.0/owy + 2.0/owz;

    int local_i = in / size;
    int remainder = in % size;
    if (rank < remainder) local_i++;
    int i_start = (in / size) * rank + (rank < remainder ? rank : remainder);
    int i_end = i_start + local_i - 1;

    double (*F)[jn+1][kn+1] = malloc((local_i + 2) * sizeof(*F));
    double (*F_old)[jn+1][kn+1] = malloc((local_i + 2) * sizeof(*F));

    for (int i = 0; i < local_i + 2; i++)
        for (int j = 0; j <= jn; j++)
            for (int k = 0; k <= kn; k++)
                F[i][j][k] = 0.0;

    for (int i = 0; i < local_i + 2; i++) {
        int i_global = i_start + i - 1;
        for (int j = 0; j <= jn; j++) {
            for (int k = 0; k <= kn; k++) {
                double x = (i_global >= 0 && i_global <= in) ? i_global * hx : 0.0;
                double y = j * hy;
                double z = k * hz;

                if (i_global == 0 || i_global == in || j == 0 || j == jn || k == 0 || k == kn) {
                    F[i][j][k] = Fresh(x, y, z);
                }
            }
        }
    }

    if (rank == 0) {
        for (int j = 0; j <= jn; j++)
            for (int k = 0; k <= kn; k++)
                F[0][j][k] = Fresh(0.0, j*hy, k*hz);
    }
    if (rank == size - 1) {
        for (int j = 0; j <= jn; j++)
            for (int k = 0; k <= kn; k++)
                F[local_i+1][j][k] = Fresh(X, j*hy, k*hz);
    }

    double start_time = MPI_Wtime();
    double global_max;
    int iter = 0;

    do {
        if (rank > 0) {
            MPI_Recv(F[0], (jn+1)*(kn+1), MPI_DOUBLE, rank-1, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        }
        if (rank < size - 1) {
            MPI_Send(F[local_i], (jn+1)*(kn+1), MPI_DOUBLE, rank+1, 0, MPI_COMM_WORLD);
        }

        for (int i = 1; i <= local_i; i++)
            for (int j = 0; j <= jn; j++)
                for (int k = 0; k <= kn; k++)
                    F_old[i][j][k] = F[i][j][k];

        double local_max = 0.0;
        for (int i = 1; i <= local_i; i++) {
            int i_global = i_start + i - 1;
            double x = i_global * hx;
            for (int j = 1; j < jn; j++) {
                double y = j * hy;
                for (int k = 1; k < kn; k++) {
                    double z = k * hz;
                    double Fi = (F[i+1][j][k] + F[i-1][j][k]) / owx;
                    double Fj = (F[i][j+1][k] + F[i][j-1][k]) / owy;
                    double Fk = (F[i][j][k+1] + F[i][j][k-1]) / owz;
                    F[i][j][k] = (Fi + Fj + Fk - Ro(x, y, z)) / c;

                    double diff = fabs(F[i][j][k] - F_old[i][j][k]);
                    if (diff > local_max) local_max = diff;
                }
            }
        }

        MPI_Allreduce(&local_max, &global_max, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);
        iter++;
        MPI_Barrier(MPI_COMM_WORLD);
    } while (global_max > EPS);

    double end_time = MPI_Wtime();
    double total_time = end_time - start_time;

    if (rank == 0) {
        printf(
            "Grid: %dx%dx%d, Processes: %d, Iterations: %d, Eps: %e, Time: %.6f s\n",
            in, jn, kn, size, iter, EPS, total_time
        );
    }

    free(F);
    free(F_old);
    MPI_Finalize();
    return 0;
}
