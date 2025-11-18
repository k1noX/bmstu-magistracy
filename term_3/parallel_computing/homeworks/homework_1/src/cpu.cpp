#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#ifdef _WIN32
    #include <windows.h>
    double get_time_sec() {
        LARGE_INTEGER freq, count;
        QueryPerformanceFrequency(&freq);
        QueryPerformanceCounter(&count);
        return (double)count.QuadPart / (double)freq.QuadPart;
    }
#else
    #include <sys/time.h>
    double get_time_sec() {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        return tv.tv_sec + tv.tv_usec * 1e-6;
    }
#endif

void compute_row_sum_squares(const float* A, float* row_sums, int m, int n) {
    for (int i = 0; i < m; i++) {
        float sum = 0.0f;
        for (int j = 0; j < n; j++) {
            float val = A[i * n + j];
            sum += val * val;
        }
        row_sums[i] = sum;
    }
}

float* generate_random_matrix(int m, int n) {
    float* A = (float*)malloc(m * n * sizeof(float));
    if (!A) {
        fprintf(stderr, "Error: Failed to allocate matrix memory.\n");
        exit(EXIT_FAILURE);
    }

    srand(42);
    for (int i = 0; i < m * n; i++) {
        A[i] = ((float)rand() / RAND_MAX) * 10.0f - 5.0f;
    }
    return A;
}

int main(int argc, char* argv[]) {
    int m = 2000;
    int n = 3000;

    if (argc >= 3) {
        m = atoi(argv[1]);
        n = atoi(argv[2]);
        if (m <= 0 || n <= 0) {
            fprintf(stderr, "Error: Matrix dimensions must be positive integers.\n");
            return EXIT_FAILURE;
        }
    }

    printf("CPU Row Sum of Squares\n");
    printf("Matrix size: %d x %d\n", m, n);

    float* A = generate_random_matrix(m, n);
    float* row_sums = (float*)calloc(m, sizeof(float));
    if (!row_sums) {
        fprintf(stderr, "Error: Failed to allocate result vector.\n");
        free(A);
        return EXIT_FAILURE;
    }

    double start = get_time_sec();
    compute_row_sum_squares(A, row_sums, m, n);
    double elapsed = get_time_sec() - start;

    printf("Computation time: %.4f seconds\n", elapsed);

    printf("First 5 row sums:\n");
    for (int i = 0; i < 5 && i < m; i++) {
        printf("Row %d: %.2f\n", i, row_sums[i]);
    }

    free(A);
    free(row_sums);

    return 0;
}
