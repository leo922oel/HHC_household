import random as rd
import numpy as np
from scipy.stats import expon
import multiprocessing as mp
import matplotlib.pyplot as plt
import pandas as pd
import os
import time
import epyestim

beta_hh = 0.5
beta_nhh = 0.4
gamma_rate = 1/ 5
hh_size = 3
hh_total = 10

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

def get_hh_size_modif():
    total_ = 0
    pop_ = 0
    record = np.zeros(5 + 1)
    while total_ < hh_total:
        times = np.random.randint(1, hh_total//5)
        nums = np.random.randint(1, 5+1)
        if total_ + times > hh_total:
            times = hh_total - total_
        total_ += times
        pop_ += times * nums
        record[nums] += times
    
    return pop_, record

if __name__ == "__main__":
    # Y = np.zeros((hh_total, hh_size))
    # Y[0:5, 0] = 1
# 
    # tasks = np.split(Y, 8, axis=0)
    # pool = mp.Pool(processes=8)
    # start = time.time()
    # results = pool.starmap(multiproc_binom, [(task, 1, beta_hh) for task in tasks])
    # print(time.time()-start)
    # inc_hh = np.concatenate(results, axis=0)
    # print(len(inc_hh))

    a = [1, 2, 3, 4, 5, 6, 7, 8]
    n = np.zeros(8)
    for t in range(8):
        if t < 3 or t >= 8-3:
            n[t] = a[t]
        else:
            print(a[t-3 : t+3])
            n[t] = np.mean(a[t-3 : t+3])
            n[t] = np.mean(a[t-3 : t+3]) 
    print(n)
    """    
    Y = np.zeros(50)
    for i in range(50):
        Y[i] = np.random.randint(0, 10)
    
    g = epyestim.distributions.discretise_gamma(5, (2.2*0.2))
    g = pd.Series(g)
    Y = pd.Series(Y)
    gs = epyestim.smoothen.smoothen_series(g, 5)
    r = epyestim.estimate_r.estimate_r(
        infections_ts = Y,
        gt_distribution = gs,
        a_prior = 0,
        b_prior = .5,
        window_size = 3
    )
    r = epyestim.estimate_r.gamma_quantiles(0.5, r['a_posterior'], r['b_posterior'])
    print(r)
    fig, ax = plt.subplots(figsize=(9, 7))
    ax.plot(g)
    plt.show()
    """