import mlrose
import numpy as np
import csv
import time

with open('../raw-input/city_coords.csv') as f:
    data2 = [line for line in csv.reader(f)]

# Convert the stupid shit to integer. fuck python
for i in data2:
    i[0] = int(i[0])
    i[1] = int(i[1])


results_list = [["algo", "param", "time", "fitness"]]

# Create list of city coordinates
cities = 50
coords_list = data2[1:(cities+1)]

# Initialize fitness function object using coords_list
fitness_coords = mlrose.TravellingSales(coords = coords_list)

# Define optimization problem object
problem_fit = mlrose.TSPOpt(length = len(coords_list), fitness_fn = fitness_coords, maximize=False)

# Set random seed
np.random.seed(2)

for i in range(0,5):
    # RANDOMIZED HILL CLIMBING
    start = time.time()
    restarts = 10
    rando_state, rando_fitness = mlrose.random_hill_climb(problem_fit, max_attempts = 10, restarts = restarts, init_state = None)

    end = time.time()
    rando_time = end - start

    print("RHC time: {}; RHC fitness: {}".format(rando_time, rando_fitness))
    print(rando_state)

    results_list.append(["RHC", restarts, rando_time, rando_fitness])


    # GENETIC ALGO
    start = time.time()
    ga_pop = 200
    genetic_state, genetic_fitness = mlrose.genetic_alg(problem_fit, pop_size = ga_pop, mutation_prob= 0.1, max_attempts=10)
    end = time.time()
    genetic_time = end - start

    print("Genetic time: {}; Genetic fitness: {}".format(genetic_time, genetic_fitness))
    print(genetic_state)
    print(genetic_fitness)

    results_list.append(["GA", ga_pop, genetic_time, genetic_fitness])

    # SIMULATED ANNEALING (fuck python)
    start_temp = 1.0
    decay = mlrose.GeomDecay(init_temp=start_temp)
    start = time.time()
    sa_state, sa_fitness = mlrose.simulated_annealing(problem_fit, max_attempts = 10, schedule = decay)
    end = time.time()
    sa_time = end - start

    print(sa_time)
    print(sa_state)
    print(sa_fitness)

    results_list.append(["SA", start_temp, sa_time, sa_fitness])


    # MIMIC
    start = time.time()
    mimic_pop = 200
    mimic_state, mimic_fitness = mlrose.mimic(problem_fit, pop_size=200, keep_pct=0.2, max_attempts=10)
    end = time.time()
    mimic_time = end - start

    print(mimic_time)
    print(mimic_state)
    print(mimic_fitness)

    results_list.append(["MIMIC", mimic_pop, mimic_time, mimic_fitness])

    results_list


with open("../output/tsp_{}.csv".format(cities), "w") as f:
    writer = csv.writer(f)
    writer.writerows(results_list)
#

# print(best_state)
# # [1 3 4 5 6 7 0 2]
#
# print(best_fitness)
# 18.8958046604
