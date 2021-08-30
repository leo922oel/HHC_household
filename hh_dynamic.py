import numpy as np
import numpy
import random as rd
import math
from scipy.stats import expon
import matplotlib.pyplot as plt
import time
import multiprocessing as mp
import sys
import os

hh_total = int(sys.argv[2])
times = int(sys.argv[3])

hh_size = 3
pop = hh_total * hh_size
dur_infect = 5
gamma_rate = 1 / dur_infect
trans_hh = 1 / dur_infect
nhh_ratio = 0.18
trans_nhh = trans_hh * nhh_ratio
mask_eff = 0.8
t_end = 1
a_ = []
# seed = rd.seed()

class Outp:
    def __init__(self) -> None:
        self.Time = np.zeros(t_end + 1)
        self.Time[:] = [i for i in range(t_end + 1)]
        self.I = np.zeros(t_end + 1)
        self.R = np.zeros(t_end + 1)
        self.inc_hh = np.zeros(t_end + 1)
        self.inc_nhh = np.zeros(t_end + 1)
        self.rt_hh = np.zeros(t_end + 1)
        self.rt_nhh = np.zeros(t_end + 1)

def get_hh_size():
    a = rd.sample(range(1, hh_total * hh_size), k=hh_total-1)
    a.append(0)
    a.append(hh_total * hh_size)
    a = sorted(a)
    b = [a[i] - a[i-1] for i in range(1, len(a))]
    b = b.sort()
    return a, b

def get_hh_size_modif():
    size = 0
    total = 0
    record = np.zeros(5 + 1)
    while size < hh_size:
        times = np.random.randint(1, hh_size)
        nums = np.random.randint(1, 5)
        if size + times > hh_size:
            times = hh_size - times
        size += times
        total += times * nums
        record[nums] += times
    
    return total, record


def Init_case(Outp, init_inf):
    Y = []
    # total, record = get_hh_size_modif()
    # for size in range(5):
        # for i in range(record[5 - size]):
            # Y.append(np.zeros(5 - size))
    a, b = get_hh_size()
    for i in range(len(b)):
        Y.append(np.zeros(b[i]))
    for i in range(init_inf):
        Y[i][0] = 1
    Outp.I[0] = np.sum(Y == 1)
    Outp.R[0] = np.sum(Y == 2)

    a.pop(0)
    a.pop()
    return a, Y

def list_split(lst : list, i : int):
    row = len(lst)
    iter = row // i
    result = [lst[i:i + iter] for i in range(0, len(lst), iter)]
    return result

def multiproc_binom(type, sub_Y, param, n_I):
    p_ = np.zeros(len(sub_Y))
    if type in [1, 2]:
        for i in range(len(sub_Y)):
            sum = np.sum(np.array(sub_Y[i]) == 1)
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

    result = []
    for i in range(len(sub_Y)):
        result.append(np.random.binomial(1, p_[i], len(sub_Y[i])))

    return result
    
def list_merge(lst : list):
    result = []
    for i in range(len(lst)):
        for j in range(len(lst[i])):
            result.append(lst[i][j])
    
    return result

def Sim(goal, Outp, Y):
    for t in range(t_end):
        if goal == 1:
            ct_all = 9.14
            ct_hh = 2.26
            ct_nhh = ct_all - ct_hh
            beta_hh = ct_hh * trans_hh
            beta_nhh = ct_nhh * trans_nhh / pop
        
        elif goal == 2:
            ct_all = 3.97
            ct_hh = 2.20
            ct_nhh = ct_all - ct_hh
            beta_hh = ct_hh * trans_hh
            beta_nhh = (ct_nhh * trans_nhh / pop) * mask_eff

        elif goal == 3:
            if t <= t_end/2:
                ct_all = 9.14
                ct_hh = 2.26
                ct_nhh = ct_all - ct_hh
                beta_hh = ct_hh * trans_hh
                beta_nhh = ct_nhh * trans_nhh / pop
            else:
                ct_all = 3.97
                ct_hh = 2.20
                ct_nhh = ct_all - ct_hh
                beta_hh = ct_hh * trans_hh
                beta_nhh = (ct_nhh * trans_nhh / pop) * mask_eff

        # if t == 1:
            # printLog(1, beta_hh, beta_nhh)
        # if t == t_end//2 + 1:
            # printLog(3, beta_hh, beta_nhh)
        
        n_I = 0
        for i in range(len(Y)):
            n_I += np.sum(np.array(Y[i]) == 1)

        # tasks = np.split(Y, cpu, axis=0)
        start = time.time()
        tasks = list_split(Y, cpu)
        pool = mp.Pool(processes=cpu)

        results = pool.starmap(multiproc_binom, [(1, task, beta_hh, n_I) for task in tasks])
        # inc_hh = np.concatenate(results, axis=0)
        inc_hh = list_merge(results)
        results = pool.starmap(multiproc_binom, [(2, task, beta_nhh, n_I) for task in tasks])
        # inc_nhh = np.concatenate(results, axis=0)
        inc_nhh = list_merge(results)
        results = pool.starmap(multiproc_binom, [(3, task, beta_hh, n_I) for task in tasks])
        # rt_hh = np.concatenate(results, axis=0)
        rt_hh = list_merge(results)
        results = pool.starmap(multiproc_binom, [(4, task, beta_nhh, n_I) for task in tasks])
        # rt_nhh = np.concatenate(results, axis=0)
        rt_nhh = list_merge(results)
        print(f'1 : {time.time()-start}')

        # inc_hh = multiproc_binom(1, Y, beta_hh, n_I)
        # inc_nhh = multiproc_binom(2, Y, beta_nhh, n_I)
        # rt_hh = multiproc_binom(3, Y, beta_hh, n_I)
        # rt_nhh = multiproc_binom(4, Y, beta_nhh, n_I)

        start = time.time()
        for i in range(hh_total):
            inc_hh[i] = np.where(Y[i] != 0, 0, inc_hh[i])
            inc_nhh[i] = np.where(Y[i] != 0, 0, inc_nhh[i])
            inc_nhh[i] = np.where(inc_hh[i] == 1, 0, inc_nhh[i])
            rt_hh[i] = np.where(Y[i] != 0, 0, rt_hh[i])
            rt_nhh[i] = np.where(Y[i] != 0, 0, rt_nhh[i])
            rt_nhh[i] = np.where(rt_hh[i] == 1, 0, rt_nhh[i])
        print(f'2 : {time.time()-start}')

        recover = np.random.binomial(1, expon.cdf(1, scale=1/gamma_rate), pop)
        recover = np.hsplit(recover, a_)

        # recover = recover.reshape(hh_total, hh_size)

        start = time.time()
        sum_rt_hh = 0
        sum_rt_nhh = 0
        sum_inc_hh = 0
        sum_inc_nhh = 0
        sum_rt_hh = sum(rt_hh)
        print(sum_inc_hh)
        for i in range(hh_total):
            sum_rt_hh += np.sum(rt_hh[i])
            sum_rt_nhh += np.sum(rt_nhh[i])
            sum_inc_hh += np.sum(inc_hh[i])
            sum_inc_nhh += np.sum(inc_nhh[i])
        print(f'3 : {time.time()-start}')

        Outp.rt_hh[t + 1] = sum_rt_hh / n_I
        Outp.rt_nhh[t + 1] = sum_rt_nhh / n_I
        Outp.inc_hh[t + 1] = sum_inc_hh
        Outp.inc_nhh[t + 1] = sum_inc_nhh

        for i in range(len(Y)):
            for j in range(len(Y[i])):
                if Y[i][j] == 0 and (inc_hh[i][j] == 1 or inc_nhh[i][j] == 1): Y[i][j] = 1
                elif Y[i][j] == 1 and recover[i][j] == 1: Y[i][j] = 2
        
        for i in range(hh_total):
            Outp.I[t + 1] += np.sum(Y[i] == 1)
            Outp.R[t + 1] += np.sum(Y[i] == 2)


def printLog(wave, beta_hh, beta_nhh):
    print(f'===transmission rate, survey wave {wave}===')
    print("household = %.2e" % beta_hh)
    print("non-household = %.2e" % beta_nhh)


def plot_result(type, goal, Outp, info=None):
    fig, ax = plt.subplots(figsize=(9, 6))
    ax.axvline(t_end/2, 0, color='black', linestyle='--', lw=1)
    if type == 'IR':
        I = []
        R = []
        for i in range(len(Outp)):
            I.append(Outp[i].I)
            R.append(Outp[i].R)
            ax.plot(Outp[i].I / pop, color='red', \
                # label='I', \
                    linewidth=2, alpha=.1)
            ax.plot(Outp[i].R / pop, color='green', \
                #label='R', \
                    linewidth=2, alpha=.1)

        mean_I = np.mean(I, axis=0)
        mean_R = np.mean(R, axis=0)
        ax.plot(mean_I / pop, color='blue', label='maen I', linewidth=2, alpha=.6)
        ax.plot(mean_R / pop, color='orange', label='mean R', linewidth=2, alpha=.6)
        ax.legend(loc='upper right')
        #ax.set_xlim([0, t_end])
        ax.set_title(f'IR ({times})')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('pop')
    elif type == 'RT' or type == 'rt':
        hh = []
        nhh = []
        for i in range(len(Outp)):
            hh.append(Outp[i].rt_hh)
            nhh.append(Outp[i].rt_nhh)
            ax.plot(Outp[i].rt_hh + Outp[i].rt_nhh, color='black', \
                # label='total', \
                    linewidth=2, alpha=.1)
            ax.plot(Outp[i].rt_hh, color='red', \
                # label='household', \
                    linewidth=2, alpha=.1)
            ax.plot(Outp[i].rt_nhh, color='green', \
                # label='non-household', \
                    linewidth=2, alpha=.1)

        mean_hh = np.mean(hh, axis=0)
        mean_nhh = np.mean(nhh, axis=0)
        ax.plot(mean_hh, color='blue', label='mean household', linewidth=2, alpha=.6)
        ax.plot(mean_nhh, color='orange', label='mean non-household', linewidth=2, alpha=.6)
        ax.legend(loc='upper right')
        ax.set_title(f'RT ({times})')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('RT rate')
    elif type == 'Inf' or type == 'inf':
        hh = []
        nhh = []
        for i in range(len(Outp)):
            hh.append(Outp[i].inc_hh)
            nhh.append(Outp[i].inc_nhh)
            ax.plot(Outp[i].inc_hh, color='red', \
                # label='household', \
                    linewidth=2, alpha=.1)
            ax.plot(Outp[i].inc_nhh, color='green', \
                # label='non-household', \
                    linewidth=2, alpha=.1)

        mean_hh = np.mean(hh, axis=0)
        mean_nhh = np.mean(nhh, axis=0)
        ax.plot(mean_hh, color='blue', label='mean household', linewidth=2, alpha=.6)
        ax.plot(mean_nhh, color='orange', label='mean non-household', linewidth=2, alpha=.6)
        ax.legend(loc='upper right')
        ax.set_title(f'Inf ({times})')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('# of new infections')
    # plt.show()
    if info == None: plt.savefig(f'{type}_goal_{goal}.png')
    else : plt.savefig(f'{type}_goal_{goal}_{info}.png')


if __name__ == '__main__':
    goal = int(sys.argv[1])
    cpu = int(sys.argv[4])

    container = []

    start = time.time()
    for i in range(times):
        print(f'round: {i+1}')
        tic = time.time()
        Outp_ = Outp()
        a_, Y_ = Init_case(Outp_, 5)
        Sim(goal, Outp_, Y_)
        container.append(Outp_)
        del Outp_
    print(f'time : {time.time() - start}')
    plot_result('IR', goal, container, 'test')
    # plot_result('RT', goal, container)
    # plot_result('Inf', goal, container)