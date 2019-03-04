import mlrose
import numpy as np

# Create list of city coordinates
coords_list = [(1, 1), (4, 2), (5, 2), (6, 4), (4, 4), (3, 6), (1, 5), (2, 3)]

# Initialize fitness function object using coords_list
fitness_coords = mlrose.TravellingSales(coords = coords_list)

# Define optimization problem object
problem_fit = mlrose.TSPOpt(length = 8, fitness_fn = fitness_coords, maximize=False)

# Set random seed
np.random.seed(2)

# Solve problem using the genetic algorithm
best_state, best_fitness = mlrose.genetic_alg(problem_fit)

print(best_state)
# [1 3 4 5 6 7 0 2]

print(best_fitness)
# 18.8958046604



np.linalg.norm(np.array(coords_list[1]) - np.array(coords_list[2]))
