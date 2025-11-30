#define CL_TARGET_OPENCL_VERSION 120
#include <CL/cl.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <string>
#include <chrono>
#include <stdexcept>
#include <cstdlib>

#define CL_CHECK(err) do { \
    if (err != CL_SUCCESS) { \
        std::cerr << "OpenCL error at line " << __LINE__ << ": " << err << std::endl; \
        throw std::runtime_error("OpenCL error"); \
    } \
} while(0)

class OpenCLArrayProcessor {
private:
    cl_context context = nullptr;
    cl_command_queue queue = nullptr;
    cl_kernel kernel = nullptr;
    cl_program program = nullptr;
    const size_t MAX_BLOCK_SIZE = 64 << 20; // 64M элементов = 256 MB
    

public:
    OpenCLArrayProcessor() {
        cl_int err;
        cl_platform_id platform;
        cl_device_id device;

        cl_uint numPlatforms;
        CL_CHECK(clGetPlatformIDs(1, &platform, &numPlatforms));
        if (numPlatforms == 0) throw std::runtime_error("No OpenCL platforms");

        cl_uint numDevices;
        CL_CHECK(clGetDeviceIDs(platform, CL_DEVICE_TYPE_GPU, 1, &device, &numDevices));
        if (numDevices == 0) {
            CL_CHECK(clGetDeviceIDs(platform, CL_DEVICE_TYPE_CPU, 1, &device, &numDevices));
            if (numDevices == 0) throw std::runtime_error("No OpenCL devices");
        }

        context = clCreateContext(nullptr, 1, &device, nullptr, nullptr, &err);
        CL_CHECK(err);
        queue = clCreateCommandQueue(context, device, 0, &err);
        CL_CHECK(err);

        const char* kernelSource = 
            "__kernel void swap_even_odd(__global const int* input, __global int* output, int size) {\n"
            "    int gid = get_global_id(0);\n"
            "    if (gid * 2 + 1 < size) {\n"
            "        int even = gid * 2;\n"
            "        int odd = even + 1;\n"
            "        output[even] = input[odd];\n"
            "        output[odd] = input[even];\n"
            "    }\n"
            "}\n";

        program = clCreateProgramWithSource(context, 1, &kernelSource, nullptr, &err);
        CL_CHECK(err);
        CL_CHECK(clBuildProgram(program, 1, &device, nullptr, nullptr, nullptr));

        kernel = clCreateKernel(program, "swap_even_odd", &err);
        CL_CHECK(err);
    }

    ~OpenCLArrayProcessor() {
        if (kernel) clReleaseKernel(kernel);
        if (program) clReleaseProgram(program);
        if (queue) clReleaseCommandQueue(queue);
        if (context) clReleaseContext(context);
    }

    void processFile(const std::string& inputFile, const std::string& outputFile) {
        std::ifstream inFile(inputFile, std::ios::binary);
        if (!inFile) throw std::runtime_error("Cannot open input file");
        
        std::ofstream outFile(outputFile, std::ios::binary);
        if (!outFile) throw std::runtime_error("Cannot open output file");

        inFile.seekg(0, std::ios::end);
        size_t fileSize = inFile.tellg();
        inFile.seekg(0, std::ios::beg);
        size_t elementCount = fileSize / sizeof(int);
        if (elementCount % 2 != 0) elementCount--;

        size_t processed = 0;
        size_t totalElements = elementCount;

        std::cout << "Progress:\t0.0%" << std::flush;

        while (processed < totalElements) {
            size_t blockSize = std::min(MAX_BLOCK_SIZE, totalElements - processed);
            size_t blockElements = blockSize - (blockSize % 2);
            
            std::vector<int> block(blockElements);
            inFile.read(reinterpret_cast<char*>(block.data()), blockElements * sizeof(int));
            processBlock(block);
            outFile.write(reinterpret_cast<const char*>(block.data()), blockElements * sizeof(int));
            processed += blockElements;

            double percent = (static_cast<double>(processed) / totalElements) * 100.0;
            std::cout << "\rProgress:\t" << std::fixed << std::setprecision(1) << percent << "%" << std::flush;
        }

        std::cout << std::endl;
    }

private:
    void processBlock(std::vector<int>& block) {
        cl_int err;
        size_t size = block.size();
        if (size == 0) return;

        cl_mem inputBuf = clCreateBuffer(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR, 
                                        size * sizeof(int), block.data(), &err);
        CL_CHECK(err);
        cl_mem outputBuf = clCreateBuffer(context, CL_MEM_WRITE_ONLY, size * sizeof(int), nullptr, &err);
        CL_CHECK(err);

        CL_CHECK(clSetKernelArg(kernel, 0, sizeof(cl_mem), &inputBuf));
        CL_CHECK(clSetKernelArg(kernel, 1, sizeof(cl_mem), &outputBuf));
        int sizeArg = static_cast<int>(size);
        CL_CHECK(clSetKernelArg(kernel, 2, sizeof(int), &sizeArg));

        size_t globalSize = size / 2;
        CL_CHECK(clEnqueueNDRangeKernel(queue, kernel, 1, nullptr, &globalSize, nullptr, 0, nullptr, nullptr));
        CL_CHECK(clEnqueueReadBuffer(queue, outputBuf, CL_TRUE, 0, size * sizeof(int), block.data(), 0, nullptr, nullptr));

        clReleaseMemObject(inputBuf);
        clReleaseMemObject(outputBuf);
    }
};

int main(int argc, char** argv) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <input> <output>" << std::endl;
        return 1;
    }

    auto start = std::chrono::high_resolution_clock::now();
    try {
        OpenCLArrayProcessor proc;
        proc.processFile(argv[1], argv[2]);
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    auto end = std::chrono::high_resolution_clock::now();
    
    std::cout << "Time: " << std::chrono::duration<double>(end - start).count() << " seconds" << std::endl;
    return 0;
}
