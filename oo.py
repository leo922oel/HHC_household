import random as rd
import numpy as np
from scipy.stats import expon
import multiprocessing as mp
import os
import time

beta_hh = 0.5
beta_nhh = 0.4
gamma_rate = 1/ 5
hh_size = 3
hh_total = 100000

def multiproc_binom(sub_Y,type=1, param=beta_hh):
    n_I = np.sum(sub_Y == 1)
    p_ = np.zeros(len(sub_Y))
    if type in [1, 2]:
        for i in range(len(sub_Y)):
            sum = np.sum(sub_Y[i] == 1)
            if type == 1:
                p_[i] = expon.cdf(1, scale=1/(param * sum)) if param * sum != 0 else 0
            else:
                p_[i] = expon.cdf(1, scale=1/(param * (n_I - sum))) if param * (n_I - sum) != 0 else 0
    elif type in [3, 4]:
        for i in range(len(sub_Y)):
            sum = np.sum(sub_Y[i] == 1)
            if type == 3:
                p_[i] = expon.cdf(1/gamma_rate, scale=1/(param * sum)) if param * sum != 0 else 0
            else:
                p_[i] = expon.cdf(1/gamma_rate, scale=1/(param * (n_I - sum))) if param * (n_I - sum) != 0 else 0

    result = np.random.binomial(1, p_, [hh_size, len(sub_Y)])

    return result.T

if __name__ == "__main__":
    Y = np.zeros((hh_total, hh_size))
    Y[0:5, 0] = 1

    tasks = np.split(Y, 8, axis=0)
    pool = mp.Pool(processes=8)
    start = time.time()
    results = pool.starmap(multiproc_binom, [(task, 1, beta_hh) for task in tasks])
    print(time.time()-start)
    inc_hh = np.concatenate(results, axis=0)
    print(len(inc_hh))