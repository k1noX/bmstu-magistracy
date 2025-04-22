import pandas as pd
import numpy as np
import random

# Установка зерна для воспроизводимости
np.random.seed(42)
random.seed(42)

# Генерация данных
num_students = 100
ids = range(1, num_students + 1)

# Пример списков имен и фамилий
names = ["Алексей", "Мария", "Иван", "Екатерина", "Сергей", "Анна", "Дмитрий", "Ольга", "Андрей", "Наталья"]
surnames = ["Иванов", "Петрова", "Сидоров", "Кузнецова", "Смирнов", "Попова", "Васильев", "Морозова", "Новиков", "Федорова"]

# Случайный выбор имен и фамилий
first_names = random.choices(names, k=num_students)
last_names = random.choices(surnames, k=num_students)

# Средний балл от 2 до 5
average_grades = np.random.uniform(2, 5, num_students).round(2)

# Пропуски от 0 до 30
absences = np.random.randint(0, 31, num_students)

# Группы
groups = random.choices(["ИУК4-21М", "ИУК4-41М"], k=num_students)

# Создание DataFrame
df = pd.DataFrame({
    "ID": ids,
    "Имя": first_names,
    "Фамилия": last_names,
    "Средний балл": average_grades,
    "Пропуски": absences,
    "Группа": groups
})

# Сохранение в CSV
df.to_csv('students_performance.csv', index=False, encoding='utf-8-sig')

print("Файл 'students_performance.csv' успешно создан!")