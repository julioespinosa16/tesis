import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon

# Configurar la figura
plt.figure(figsize=(10, 8))

# Definir las rectas de restricciones
x = np.linspace(-1, 10, 400)  # Rango para x
y1 = 10 - x  # Recta 1: x + y = 10
y2 = 15 - 2 * x  # Recta 2: 2x + y = 15
y3 = (12 - x) / 2  # Recta 3: x + 2y = 12

# Curvas de nivel (isotermas) para cada iteración
z0 = 0  # Valor en A(0,0)
z1 = 22.5  # Valor en B(7.5,0)
z2 = 21  # Valor en C(3,6)
y_z0 = (z0 - 3 * x) / 2  # Z = 0
y_z1 = (z1 - 3 * x) / 2  # Z = 22.5
y_z2 = (z2 - 3 * x) / 2  # Z = 21

# Graficar rectas de restricciones (líneas punteadas)
plt.plot(x, y1, 'r--', label='x + y = 10', linewidth=2)
plt.plot(x, y2, 'b--', label='2x + y = 15', linewidth=2)
plt.plot(x, y3, 'orange', linestyle='--', label='x + 2y = 12', linewidth=2)

# Región factible (sombreada)
verts = [(0, 0), (0, 5), (3, 6), (5, 5), (7.5, 0)]
poly = Polygon(verts, facecolor='green', edgecolor='green', alpha=0.2, label='Región Factible')
plt.gca().add_patch(poly)

# Camino del simplex: A(0,0) -> B(7.5,0) -> C(3,6)
simplex_path_x = [0, 7.5, 3]
simplex_path_y = [0, 0, 6]
plt.plot(simplex_path_x, simplex_path_y, 'purple', linewidth=3, label='Camino Simplex', marker='o', markersize=8)

# Anotaciones para los vértices con valores de Z
plt.scatter(0, 0, color='red', s=100, label='A (0,0) - Iter 0, Z=0')
plt.text(0.2, 0.2, 'A (0,0)\nZ=0', fontsize=12, color='red')
plt.scatter(7.5, 0, color='orange', s=100, label='B (7.5,0) - Iter 1, Z=22.5')
plt.text(7.7, 0.2, 'B (7.5,0)\nZ=22.5', fontsize=12, color='orange')
plt.scatter(3, 6, color='green', s=150, label='C (3,6) - Óptimo, Z=21')
plt.text(3.2, 6.2, 'C (3,6)\nZ=21', fontsize=12, color='green')

# Flechas para mostrar la dirección del camino
plt.arrow(0, 0, 7.0, 0, head_width=0.2, head_length=0.3, fc='purple', ec='purple')
plt.arrow(7.5, 0, -4.2, 5.8, head_width=0.2, head_length=0.3, fc='purple', ec='purple')

# Graficar curvas de nivel para cada iteración
plt.plot(x, y_z0, 'gray', linestyle='--', label='Z = 0 (Iter 0)', linewidth=1.5)
plt.plot(x, y_z1, 'gray', linestyle='--', label='Z = 22.5 (Iter 1)', linewidth=1.5)
plt.plot(x, y_z2, 'cyan', linestyle='-.', label='Z = 21 (Óptimo)', linewidth=2)

# Configurar ejes y límites
plt.xlim(-1, 8)
plt.ylim(-1, 16)
plt.xlabel('x', fontsize=12)
plt.ylabel('y', fontsize=12)
plt.title('Evolución del Algoritmo Simplex con Curvas de Nivel', fontsize=14)
plt.grid(True, linestyle='--', alpha=0.7)

# Mostrar leyenda
plt.legend(loc='upper right', fontsize=10)

# Guardar la gráfica como archivo
plt.savefig('simplex_with_levels.png', dpi=300, bbox_inches='tight')

# Mostrar la gráfica
plt.show()