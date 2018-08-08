from matplotlib import pyplot as plt

x = [1, 2, 3, 4, 5, 6, 7, 8, 9]
y = [125, 32, 54, 253, 67, 87, 233, 56, 67]

color = [str(item/255.) for item in y]

plt.scatter(x, y, s=500, c=color)

plt.show()