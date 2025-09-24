import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import multivariate_normal

np.random.seed(42)

x1_class1 = np.random.uniform(0, 10, 50)
x2_class1 = np.random.uniform(0, 10, 50)
data_class1 = np.column_stack((x1_class1, x2_class1))

x1_class2 = np.random.uniform(5, 15, 70)
x2_class2 = np.random.uniform(5, 15, 70)
data_class2 = np.column_stack((x1_class2, x2_class2))

mu1_2d = np.mean(data_class1, axis=0)
mu2_2d = np.mean(data_class2, axis=0)

sigma1_2d = np.sqrt(np.mean(np.var(data_class1, axis=0)))
sigma2_2d = np.sqrt(np.mean(np.var(data_class2, axis=0)))

cov1 = np.array([[sigma1_2d**2, 0],
                 [0, sigma1_2d**2]])
cov2 = np.array([[sigma2_2d**2, 0],
                 [0, sigma2_2d**2]])

print(f"Класс ω1: μ = [{mu1_2d[0]:.3f}, {mu1_2d[1]:.3f}], σ = {sigma1_2d:.3f}")
print(f"Класс ω2: μ = [{mu2_2d[0]:.3f}, {mu2_2d[1]:.3f}], σ = {sigma2_2d:.3f}")

x_min, x_max = min(mu1_2d[0]-3*sigma1_2d, mu2_2d[0]-3*sigma2_2d), max(mu1_2d[0]+3*sigma1_2d, mu2_2d[0]+3*sigma2_2d)
y_min, y_max = min(mu1_2d[1]-3*sigma1_2d, mu2_2d[1]-3*sigma2_2d), max(mu1_2d[1]+3*sigma1_2d, mu2_2d[1]+3*sigma2_2d)

xx, yy = np.meshgrid(np.linspace(x_min, x_max, 200),
                     np.linspace(y_min, y_max, 200))
pos = np.dstack((xx, yy))

rv1 = multivariate_normal(mu1_2d, cov1)
rv2 = multivariate_normal(mu2_2d, cov2)

pdf1_2d = rv1.pdf(pos)
pdf2_2d = rv2.pdf(pos)

fig, ax = plt.subplots(1, 1, figsize=(10, 8))

levels = np.linspace(0, max(np.max(pdf1_2d), np.max(pdf2_2d)), 10)
cs1 = ax.contour(xx, yy, pdf1_2d, levels=levels, cmap='Blues', alpha=0.5)
cs2 = ax.contour(xx, yy, pdf2_2d, levels=levels, cmap='Reds', alpha=0.5)

diff = pdf1_2d - pdf2_2d
boundary_contour = ax.contour(xx, yy, diff, levels=[0], colors='green', linewidths=2, linestyles='--')
plt.clabel(boundary_contour, inline=True, fontsize=10, fmt='Граница')

ax.scatter(data_class1[:, 0], data_class1[:, 1], c='blue', s=20, alpha=0.6, label='Класс ω1 (50 точек)')
ax.scatter(data_class2[:, 0], data_class2[:, 1], c='red', s=20, alpha=0.6, label='Класс ω2 (70 точек)')

ax.set_title('Условные плотности вероятностей и граница разделения (2D)')
ax.set_xlabel('Признак x1')
ax.set_ylabel('Признак x2')
ax.legend()
ax.grid(True, alpha=0.3)
ax.set_aspect('equal', 'box')
plt.show()

X_2d = np.array([7.5, 7.5])
print(f"\nАнализ точки X = ({X_2d[0]}, {X_2d[1]})")

P_w1 = 0.5
P_w2 = 0.5

P_X_w1 = rv1.pdf(X_2d)
P_X_w2 = rv2.pdf(X_2d)

P_X = P_X_w1 * P_w1 + P_X_w2 * P_w2

P_w1_X = (P_X_w1 * P_w1) / P_X
P_w2_X = (P_X_w2 * P_w2) / P_X

print(f"Апостериорная вероятность P(ω1|X) = {P_w1_X:.4f}")
print(f"Апостериорная вероятность P(ω2|X) = {P_w2_X:.4f}")

predicted_class_2d = "ω1" if P_w1_X > P_w2_X else "ω2"
print(f"Точка X = ({X_2d[0]}, {X_2d[1]}) относится к классу {predicted_class_2d}")
