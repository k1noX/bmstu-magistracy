import matplotlib.pyplot as plt

sizes = [400, 600, 800, 2000]
ring_times = [0.000388, 0.000801, 0.001813, 0.007421]
graph_times = [0.000320, 0.000600, 0.001366,  0.006407]

plt.figure(figsize=(8, 5))
plt.plot(sizes, ring_times, 'o-', label='Топология "кольцо"')
plt.plot(sizes, graph_times, 's-', label='Топология "полный граф"')
plt.xlabel('Размер матрицы N (N×N)')
plt.ylabel('Время выполнения (сек)')
plt.title('Сравнение топологий для умножения матрицы на вектор')
plt.legend()
plt.grid(True)
plt.savefig('tc.png', dpi=150)
plt.show()
