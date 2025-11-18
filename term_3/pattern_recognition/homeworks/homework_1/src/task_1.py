import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

np.random.seed(42)

x1 = np.random.uniform(0, 10, 50)
x2 = np.random.uniform(5, 15, 70)

mu1, sigma1 = np.mean(x1), np.std(x1)
mu2, sigma2 = np.mean(x2), np.std(x2)

print(f"Класс ω1: μ = {mu1:.3f}, σ = {sigma1:.3f}")
print(f"Класс ω2: μ = {mu2:.3f}, σ = {sigma2:.3f}")

def find_boundary(mu1: float, sigma1: float, mu2: float, sigma2: float, search_range: tuple[float, float]) -> float:
    """
    Нахождение границы разделения классов.
    При равных априорных вероятностях P(ω1)=P(ω2)=0.5:
    Граница - это точка, где P(x|ω1) * P(ω1) = P(x|ω2) * P(ω2), т.е. P(x|ω1) = P(x|ω2)
    """
    x_vals = np.linspace(search_range[0], search_range[1], 1000)
    pdf1 = norm.pdf(x_vals, mu1, sigma1)
    pdf2 = norm.pdf(x_vals, mu2, sigma2)
    diff = pdf1 - pdf2
    idx = np.argmin(np.abs(diff))
    return x_vals[idx]

search_start = min(mu1, mu2) - 2*max(sigma1, sigma2)
search_end = max(mu1, mu2) + 2*max(sigma1, sigma2)
boundary = find_boundary(mu1, sigma1, mu2, sigma2, (search_start, search_end))
print(f"Граница разделения классов: x = {boundary:.3f}")

x_plot = np.linspace(search_start, search_end, 500)
pdf1_plot = norm.pdf(x_plot, mu1, sigma1)
pdf2_plot = norm.pdf(x_plot, mu2, sigma2)

plt.figure(figsize=(10, 6))
plt.plot(x_plot, pdf1_plot, label=f'P(x|ω1), μ={mu1:.2f}, σ={sigma1:.2f}', color='blue')
plt.plot(x_plot, pdf2_plot, label=f'P(x|ω2), μ={mu2:.2f}, σ={sigma2:.2f}', color='red')
plt.axvline(x=boundary, color='green', linestyle='--', label=f'Граница: x={boundary:.2f}')
plt.scatter(x1, [0]*len(x1), alpha=0.6, color='blue', s=20, marker='|')
plt.scatter(x2, [0]*len(x2), alpha=0.6, color='red', s=20, marker='|')
plt.title('Условные плотности вероятностей и граница разделения (1D)')
plt.xlabel('Значение признака x')
plt.ylabel('Плотность вероятности P(x|ω)')
plt.legend()
plt.grid(True, alpha=0.3)
plt.show()

X = 7.5
print(f"\nАнализ точки X = {X}")

P_w1 = 0.5
P_w2 = 0.5

P_X_w1 = norm.pdf(X, mu1, sigma1)
P_X_w2 = norm.pdf(X, mu2, sigma2)

P_X = P_X_w1 * P_w1 + P_X_w2 * P_w2

P_w1_X = (P_X_w1 * P_w1) / P_X
P_w2_X = (P_X_w2 * P_w2) / P_X

print(f"Апостериорная вероятность P(ω1|X) = {P_w1_X:.4f}")
print(f"Апостериорная вероятность P(ω2|X) = {P_w2_X:.4f}")

predicted_class = "ω1" if P_w1_X > P_w2_X else "ω2"
print(f"Точка X = {X} относится к классу {predicted_class}")

P_error_w1 = 1 - norm.cdf(boundary, mu1, sigma1)
P_error_w2 = norm.cdf(boundary, mu2, sigma2)

P_error = P_w1 * P_error_w1 + P_w2 * P_error_w2

print(f"\nВероятность ошибки классификации P(error) = {P_error:.4f}")
