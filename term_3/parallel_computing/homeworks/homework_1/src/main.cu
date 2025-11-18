#include <cuda_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define CUDA_CHECK(call) \
    do { \
        cudaError_t err = call; \
        if (err != cudaSuccess) { \
            fprintf(stderr, "CUDA error at %s:%d - %s\n", __FILE__, __LINE__, \
                    cudaGetErrorString(err)); \
            exit(EXIT_FAILURE); \
        } \
    } while(0)

__global__ void sum_of_squares_kernel(const float* A, float* row_sums, int m, int n) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= m) return;

    float sum = 0.0f;
    for (int j = 0; j < n; j++) {
        float val = A[i * n + j];
        sum += val * val;
    }
    row_sums[i] = sum;
}

void compute_row_sum_squares(const float* h_A, float* h_row_sums, int m, int n) {
    size_t matrix_size = m * n * sizeof(float);
    size_t sums_size = m * sizeof(float);

    float *d_A = nullptr, *d_row_sums = nullptr;
    CUDA_CHECK(cudaMalloc(&d_A, matrix_size));
    CUDA_CHECK(cudaMalloc(&d_row_sums, sums_size));

    CUDA_CHECK(cudaMemcpy(d_A, h_A, matrix_size, cudaMemcpyHostToDevice));

    int threads_per_block = 256;
    int blocks = (m + threads_per_block - 1) / threads_per_block;
    sum_of_squares_kernel<<<blocks, threads_per_block>>>(d_A, d_row_sums, m, n);

    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());

    CUDA_CHECK(cudaMemcpy(h_row_sums, d_row_sums, sums_size, cudaMemcpyDeviceToHost));

    CUDA_CHECK(cudaFree(d_A));
    CUDA_CHECK(cudaFree(d_row_sums));
}

void print_matrix(const float* A, int m, int n) {
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            printf("%6.2f ", A[i * n + j]);
        }
        printf("\n");
    }
}

void print_vector(const float* v, int n) {
    for (int i = 0; i < n; i++) {
        printf("%.2f ", v[i]);
    }
    printf("\n");
}

int main() {
    const int m = 4;
    const int n = 5;

    float h_A[] = {
        1.0f, 2.0f, 3.0f, 4.0f, 5.0f,
        2.0f, 2.0f, 2.0f, 2.0f, 2.0f,
        0.0f, 1.0f, 0.0f, -1.0f, 0.0f,
        3.0f, 4.0f, 0.0f, 0.0f, 0.0f
    };

    float* h_row_sums = (float*)malloc(m * sizeof(float));

    printf("Input matrix (%dx%d):\n", m, n);
    print_matrix(h_A, m, n);

    compute_row_sum_squares(h_A, h_row_sums, m, n);

    printf("\nSum of squares for each row:\n");
    print_vector(h_row_sums, m);

    free(h_row_sums);
    return 0;
}