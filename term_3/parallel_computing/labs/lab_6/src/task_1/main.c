#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>

#define a 1

double Fresh(double x, double y, double z) {
    return x + y + z;
}

double Ro(double x, double y, double z) {
    return -a * (x + y + z);
}

int main(int argc, char** argv) {
    if (argc < 4) {
        fprintf(stderr, "Usage: %s <in> <jn> <kn>\n", argv[0]);
        return 1;
    }

    int in = atoi(argv[1]);
    int jn = atoi(argv[2]);
    int kn = atoi(argv[3]);

    if (in <= 0 || jn <= 0 || kn <= 0) {
        fprintf(stderr, "Error: all dimensions must be positive integers.\n");
        return 1;
    }

    double *F_data = (double*)calloc((in + 1) * (jn + 1) * (kn + 1), sizeof(double));
    if (!F_data) {
        perror("Failed to allocate F");
        return 1;
    }

    #define F(i, j, k) F_data[(i)*(jn+1)*(kn+1) + (j)*(kn+1) + (k)]

    double X = 2.0, Y = 2.0, Z = 2.0;
    double e = 1e-5;

    double hx = X / in;
    double hy = Y / jn;
    double hz = Z / kn;

    double owx = hx * hx;
    double owy = hy * hy;
    double owz = hz * hz;

    double c = 2.0 / owx + 2.0 / owy + 2.0 / owz + a;

    for (int i = 0; i <= in; i++) {
        for (int j = 0; j <= jn; j++) {
            for (int k = 0; k <= kn; k++) {
                if (i == 0 || i == in || j == 0 || j == jn || k == 0 || k == kn) {
                    F(i, j, k) = Fresh(i * hx, j * hy, k * hz);
                } else {
                    F(i, j, k) = 0.0;
                }
            }
        }
    }

    struct timeval tv1, tv2;
    gettimeofday(&tv1, NULL);

    int it = 0;
    int converged = 0;

    while (!converged) {
        converged = 1;
        for (int i = 1; i < in; i++) {
            for (int j = 1; j < jn; j++) {
                for (int k = 1; k < kn; k++) {
                    double F_old = F(i, j, k);
                    double Fi = (F(i+1, j, k) + F(i-1, j, k)) / owx;
                    double Fj = (F(i, j+1, k) + F(i, j-1, k)) / owy;
                    double Fk = (F(i, j, k+1) + F(i, j, k-1)) / owz;
                    F(i, j, k) = (Fi + Fj + Fk - Ro(i * hx, j * hy, k * hz)) / c;

                    if (fabs(F(i, j, k) - F_old) > e) {
                        converged = 0;
                    }
                }
            }
        }
        it++;
    }

    gettimeofday(&tv2, NULL);
    double elapsed_sec = (tv2.tv_sec - tv1.tv_sec) + (tv2.tv_usec - tv1.tv_usec) / 1000000.0;

    double max_diff = 0.0;
    int mi = 0, mj = 0, mk = 0;

    for (int i = 1; i < in; i++) {
        for (int j = 1; j < jn; j++) {
            for (int k = 1; k < kn; k++) {
                double exact = Fresh(i * hx, j * hy, k * hz);
                double diff = fabs(F(i, j, k) - exact);
                if (diff > max_diff) {
                    max_diff = diff;
                    mi = i; mj = j; mk = k;
                }
            }
        }
    }

    printf("\nin = %d, jn = %d, kn = %d\n", in, jn, kn);
    printf("Iter = %d, Eps = %e, Time = %.6f ms\n", it, e, elapsed_sec);
    printf("Max differ = %e at point (%d, %d, %d)\n", max_diff, mi, mj, mk);

    free(F_data);
    return 0;
}
