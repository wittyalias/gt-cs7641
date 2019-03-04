import mlrose
import numpy as np
import csv
import time

# have made graphs with 25, 50 and 100 nodes
nodes = 100

# The random graph is made using the make_graph.R script
with open('../output/graph_{}.csv'.format(nodes)) as f:
    edges = [line for line in csv.reader(f)]

edges = edges[1:len(edges)+1]
edges

# Convert the stupid shit to integer. fuck python
for i in edges:
    i[0] = int(i[0])
    i[1] = int(i[1])

# print(data2)
results_list = [["algo", "param", "time", "fitness"]]

num_edges = nodes +1

# Initialize fitness function object using coords_list
fitness_func = mlrose.MaxKColor(edges = edges)

# Define optimization problem object
problem_fit = mlrose.DiscreteOpt(length = num_edges, fitness_fn = fitness_func, maximize = False)

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

    results_list.append(["GA", ga_pop, genetic_time, genetic_fitness])

    # SIMULATED ANNEALING (fuck python)
    start_temp = 1.0
    decay = mlrose.GeomDecay(init_temp=start_temp)
    start = time.time()
    sa_state, sa_fitness = mlrose.simulated_annealing(problem_fit, max_attempts = 10, schedule = decay)
    end = time.time()
    sa_time = end - start

    print("Simulated Annealing time: {}; Simulated Annealing fitness: {}".format(sa_time, sa_fitness))
    print(sa_state)

    results_list.append(["SA", start_temp, sa_time, sa_fitness])


    # MIMIC
    start = time.time()
    mimic_pop = 200
    mimic_state, mimic_fitness = mlrose.mimic(problem_fit, pop_size=200, keep_pct=0.2, max_attempts=10)
    end = time.time()
    mimic_time = end - start

    print("Mimic time: {}; Mimic fitness: {}".format(mimic_time, mimic_fitness))
    print(mimic_state)

    results_list.append(["MIMIC", mimic_pop, mimic_time, mimic_fitness])

    results_list


with open("../output/k_colors_{}.csv".format(nodes), "w") as f:
    writer = csv.writer(f)
    writer.writerows(results_list)
