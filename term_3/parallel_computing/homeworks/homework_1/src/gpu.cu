#include <cuda_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

void compute_row_sum_squares_cpu(const float* A, float* row_sums, int m, int n) {
    for (int i = 0; i < m; i++) {
        float sum = 0.0f;
        for (int j = 0; j < n; j++) {
            float val = A[i * n + j];
            sum += val * val;
        }
        row_sums[i] = sum;
    }
}

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

double compute_row_sum_squares_gpu(const float* h_A, float* h_row_sums, int m, int n) {
    size_t matrix_size = m * n * sizeof(float);
    size_t sums_size = m * sizeof(float);

    float *d_A = nullptr, *d_row_sums = nullptr;
    cudaMalloc(&d_A, matrix_size);
    cudaMalloc(&d_row_sums, sums_size);

    cudaMemcpy(d_A, h_A, matrix_size, cudaMemcpyHostToDevice);

    int threads_per_block = 256;
    int blocks = (m + threads_per_block - 1) / threads_per_block;

    sum_of_squares_kernel<<<blocks, threads_per_block>>>(d_A, d_row_sums, m, n);
    cudaDeviceSynchronize();

    cudaEvent_t start, stop;
    cudaEventCreate(&start);
    cudaEventCreate(&stop);

    cudaEventRecord(start);
    sum_of_squares_kernel<<<blocks, threads_per_block>>>(d_A, d_row_sums, m, n);
    cudaEventRecord(stop);
    cudaEventSynchronize(stop);

    float milliseconds = 0;
    cudaEventElapsedTime(&milliseconds, start, stop);

    cudaMemcpy(h_row_sums, d_row_sums, sums_size, cudaMemcpyDeviceToHost);

    cudaFree(d_A);
    cudaFree(d_row_sums);
    cudaEventDestroy(start);
    cudaEventDestroy(stop);

    return milliseconds / 1000.0;
}

float* generate_random_matrix(int m, int n) {
    float* A = (float*)malloc(m * n * sizeof(float));
    srand(42);
    for (int i = 0; i < m * n; i++) {
        A[i] = ((float)rand() / RAND_MAX) * 10.0f - 5.0f;
    }
    return A;
}

int main() {
    const int m = 2000; 
    const int n = 3000; 

    printf("GPU Row Sum of Squares (CUDA)\n");
    printf("Matrix size: %d x %d\n", m, n);

    float* h_A = generate_random_matrix(m, n);
    float* h_sums_gpu = (float*)malloc(m * sizeof(float));
    float* h_sums_cpu = (float*)malloc(m * sizeof(float));

    double gpu_time = compute_row_sum_squares_gpu(h_A, h_sums_gpu, m, n);

    compute_row_sum_squares_cpu(h_A, h_sums_cpu, m, n);

    printf("GPU computation time: %.6f seconds\n", gpu_time);
    printf("\nFirst 5 row sums (GPU):\n");
    for (int i = 0; i < 5 && i < m; i++) {
        printf("Row %d: %.2f\n", i, h_sums_gpu[i]);
    }

    free(h_A);
    free(h_sums_gpu);
    free(h_sums_cpu);

    return 0;
}
