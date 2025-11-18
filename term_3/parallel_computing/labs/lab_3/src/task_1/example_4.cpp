#include <omp.h>
#include <stdio.h>

#define N 50

int main()
{
    int i, size, rank;
    float a[N], b[N], c[N];

    for (i = 0; i < N; i++) {
        a[i] = b[i] = i * 1.0;
    }

    #pragma omp parallel shared(a, b, c) private(i, rank, size)
    {
        rank = omp_get_thread_num();
        printf("Thread %d starting...\n", rank);

        #pragma omp sections nowait
        {
            #pragma omp section
            {
                for (i = 0; i < N / 2; i++) {
                    c[i] = a[i] + b[i];
                    printf("rank = %d i = %d c[i] = %f\n", rank, i, c[i]);
                }
            }

            #pragma omp section
            {
                for (i = N / 2; i < N; i++) {
                    c[i] = a[i] + b[i];
                    printf("rank = %d i = %d c[i] = %f\n", rank, i, c[i]);
                }
            }
        }

        if (rank == 0) {
            size = omp_get_num_threads();
            printf("Number of threads = %d\n", size);
        }
    }

    return 0;
}
