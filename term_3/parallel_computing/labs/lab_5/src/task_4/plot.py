import matplotlib.pyplot as plt

sizes = [300, 530, 710, 1030]
time_block = [0.014959, 0.076060, 0.192586, 0.583746]
time_cyclic = [0.015854, 0.080210, 0.186305, 0.533575]

plt.figure(figsize=(8, 5))
plt.plot(sizes, time_block, 'o-', label='Блочное распределение')
plt.plot(sizes, time_cyclic, 's-', label='Циклическое распределение')
plt.xlabel('Размер матрицы (n)')
plt.ylabel('Время выполнения (сек)')
plt.title('Сравнение времени решения СЛАУ методом Гаусса (4 процесса)')
plt.legend()
plt.grid(True)
plt.savefig('./results/task_4.png', dpi=150)
plt.show()
