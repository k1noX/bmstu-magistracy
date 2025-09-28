import matplotlib.pyplot as plt

sizes = [300, 530, 710, 1030]

mpi_times = [0.000201, 0.000388, 0.000902, 0.001446]
omp_times = [0.000150001, 0.000259998, 0.000389998, 0.000890000]

plt.figure(figsize=(8, 5))
plt.plot(sizes, mpi_times, 'o-', label='MPI (2 процесса)')
plt.plot(sizes, omp_times, 's-', label='OpenMP (2 потока)')
plt.xlabel('Размер матрицы N (N×N)')
plt.ylabel('Время выполнения (сек)')
plt.title('Сравнение MPI и OpenMP для умножения матрицы на вектор')
plt.legend()
plt.grid(True)
plt.savefig('../../results/task3/matvec_comparison.png', dpi=150)
plt.show()
