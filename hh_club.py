import numpy as np
import random as rd
import math
import pandas as pd
from scipy.stats import expon
import matplotlib.pyplot as plt
import time
import multiprocessing as mp
import matplotlib.gridspec as gridspec
import sys
import os

hh_total = int(sys.argv[2])
times = int(sys.argv[4])

hh_size = int(sys.argv[3])
pop = hh_total * hh_size
dur_infect = 5
gamma_rate = 1 / dur_infect
# trans_hh = 1 / dur_infect
# nhh_ratio = 0.18
# trans_nhh = trans_hh * nhh_ratio
mask_eff = 0.8
t_end = 50


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
    return b

def Init_case(Outp, init_inf):
    Y = np.zeros((hh_total, hh_size), dtype=np.int64)
    Y[0:init_inf, 0] = 1
    Outp.I[0] = np.sum(Y == 1)
    Outp.R[0] = np.sum(Y == 2)
    return Y

def multiproc_binom(type, sub_Y, param, n_I):
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
    

def Sim(goal, Outp, Y, trans_hh, trans_nhh, ct_hh, ct_all):
    for t in range(t_end):
        if goal == 1:
            # ct_all = 9.14
            # ct_hh = 2.26
            ct_nhh = ct_all - ct_hh
            beta_hh = ct_hh * trans_hh
            beta_nhh = ct_nhh * trans_nhh / pop
        
        elif goal == 2:
            # ct_all = 3.97
            # ct_hh = 2.20
            ct_nhh = ct_all - ct_hh
            beta_hh = ct_hh * trans_hh
            beta_nhh = (ct_nhh * trans_nhh / pop) * mask_eff

        elif goal == 3:
            if t <= t_end/2:
                # ct_all = 9.14
                # ct_hh = 2.26
                ct_nhh = ct_all - ct_hh
                beta_hh = ct_hh * trans_hh
                beta_nhh = ct_nhh * trans_nhh / pop
            else:
                # ct_all = 3.97
                # ct_hh = 2.20
                ct_nhh = ct_all - ct_hh
                beta_hh = ct_hh * trans_hh
                beta_nhh = (ct_nhh * trans_nhh / pop) * mask_eff

        # if t == 1:
            # printLog(1, beta_hh, beta_nhh)
        # if t == t_end//2 + 1:
            # printLog(3, beta_hh, beta_nhh)
        
        n_I = np.sum(Y == 1)
        cpus = int(sys.argv[5])
        if cpus != 1:
            tasks = np.split(Y, cpus, axis=0)
            pool = mp.Pool(processes=cpus)
            results = pool.starmap(multiproc_binom, [(1, task, beta_hh, n_I) for task in tasks])
            inc_hh = np.concatenate(results, axis=0)
            results = pool.starmap(multiproc_binom, [(2, task, beta_nhh, n_I) for task in tasks])
            inc_nhh = np.concatenate(results, axis=0)
            results = pool.starmap(multiproc_binom, [(3, task, beta_hh, n_I) for task in tasks])
            rt_hh = np.concatenate(results, axis=0)
            results = pool.starmap(multiproc_binom, [(4, task, beta_nhh, n_I) for task in tasks])
            rt_nhh = np.concatenate(results, axis=0)
        else:
            inc_hh = multiproc_binom(1, Y, beta_hh, n_I)
            inc_nhh = multiproc_binom(2, Y, beta_nhh, n_I)
            rt_hh = multiproc_binom(3, Y, beta_hh, n_I)
            rt_nhh = multiproc_binom(4, Y, beta_nhh, n_I)

        inc_hh = np.where(Y != 0, 0, inc_hh)
        inc_nhh = np.where(Y != 0, 0, inc_nhh)
        inc_nhh = np.where(inc_hh == 1, 0, inc_nhh)
        rt_hh = np.where(Y != 0, 0, rt_hh)
        rt_nhh = np.where(Y != 0, 0, rt_nhh)
        rt_nhh = np.where(rt_hh == 1, 0, rt_nhh)

        recover = np.random.binomial(1, expon.cdf(1, scale=1/gamma_rate), pop)
        recover = recover.reshape(hh_total, hh_size)
        
        Outp.rt_hh[t + 1] = np.sum(rt_hh) / n_I if n_I > 0 else 0
        Outp.rt_nhh[t + 1] = np.sum(rt_nhh) / n_I if n_I > 0 else 0

        Outp.inc_hh[t + 1] = np.sum(inc_hh)
        Outp.inc_nhh[t + 1] = np.sum(inc_nhh)

        for i in range(len(Y)):
            for j in range(len(Y[i])):
                if Y[i, j] == 0 and (inc_hh[i, j] == 1 or inc_nhh[i, j] == 1): Y[i, j] = 1
                elif Y[i, j] == 1 and recover[i, j] == 1: Y[i, j] = 2
        Outp.I[t + 1] = np.sum(Y == 1)
        Outp.R[t + 1] = np.sum(Y == 2)


def printLog(wave, beta_hh, beta_nhh):
    print(f'===transmission rate, survey wave {wave}===')
    print("household = %.2e" % beta_hh)
    print("non-household = %.2e" % beta_nhh)

def plot_result(type, goal, Outp, info=None):
    fig, ax = plt.subplots(figsize=(9, 6))
    fig.axvline(t_end/2, 0, color='black', linestyle='--', lw=1)
    if type == 'IR':
        I = []
        R = []
        for i in range(len(Outp)):
            I.append(Outp[i].I)
            R.append(Outp[i].R)
            if i == 0:
                ax.plot(Outp[i].I / pop, color='red', \
                    label='I', \
                        linewidth=1, alpha=.3)
                ax.plot(Outp[i].R / pop, color='green', \
                    label='R', \
                        linewidth=1, alpha=.3)
            else:
                ax.plot(Outp[i].I / pop, color='red', \
                    # label='I', \
                        linewidth=1, alpha=.3)
                ax.plot(Outp[i].R / pop, color='green', \
                    #label='R', \
                        linewidth=1, alpha=.3)

        mean_I = np.mean(I, axis=0)
        mean_R = np.mean(R, axis=0)
        ax.plot(mean_I / pop, color='blue', label='maen I', linewidth=2, alpha=.6)
        ax.plot(mean_R / pop, color='orange', label='mean R', linewidth=2, alpha=.6)
        
        ax.legend(loc='upper right')
        #ax.set_xlim([0, t_end])
        ax.set_title('IR')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('pop')
    elif type == 'RT' or type == 'rt':
        hh = []
        nhh = []
        for i in range(len(Outp)):
            hh.append(Outp[i].rt_hh)
            nhh.append(Outp[i].rt_nhh)
            smooth_hh = np.zeros(t_end+1)
            smooth_nhh = np.zeros(t_end+1)
            for t in range(t_end+1):
                if t <= 3 or t >= t_end-3:
                    smooth_hh[t] = hh[i][t]
                    smooth_nhh[t] = nhh[i][t]
                else:
                    smooth_hh[t] = np.mean(hh[i][t-3 : t+3])
                    smooth_nhh[t] = np.mean(hh[i][t-3 : t+3]) 
            # ax.plot(Outp[i].rt_hh + Outp[i].rt_nhh, color='black', \
                # label='total', \
                    # linewidth=1, alpha=.1)
            if i == 0:
                ax.plot(smooth_hh, color='red', \
                    label='household', \
                        linewidth=1, alpha=.3)
                ax.plot(smooth_nhh, color='green', \
                    label='non-household', \
                        linewidth=1, alpha=.3)
            else:
                ax.plot(smooth_hh, color='red', \
                    # label='household', \
                        linewidth=1, alpha=.3)
                ax.plot(smooth_nhh, color='green', \
                    # label='non-household', \
                        linewidth=1, alpha=.3)

        mean_hh = np.mean(hh, axis=0)
        mean_nhh = np.mean(nhh, axis=0)
        ax.plot(mean_hh, color='blue', label='mean household', linewidth=2, alpha=.6)
        ax.plot(mean_nhh, color='orange', label='mean non-household', linewidth=2, alpha=.6)
        ax.legend(loc='upper right')
        ax.set_title('RT')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('RT rate')
    elif type == 'Inf' or type == 'inf':
        hh = []
        nhh = []
        for i in range(len(Outp)):
            hh.append(Outp[i].inc_hh)
            nhh.append(Outp[i].inc_nhh)
            if i == 0:
                ax.plot(Outp[i].inc_hh, color='red', \
                    label='household', \
                        linewidth=1, alpha=.3)
                ax.plot(Outp[i].inc_nhh, color='green', \
                    label='non-household', \
                        linewidth=1, alpha=.3)
            else:
                ax.plot(Outp[i].inc_hh, color='red', \
                    # label='household', \
                        linewidth=1, alpha=.3)
                ax.plot(Outp[i].inc_nhh, color='green', \
                    # label='non-household', \
                        linewidth=1, alpha=.3)

        mean_hh = np.mean(hh, axis=0)
        mean_nhh = np.mean(nhh, axis=0)
        ax.plot(mean_hh, color='blue', label='mean household', linewidth=2, alpha=.6)
        ax.plot(mean_nhh, color='orange', label='mean non-household', linewidth=2, alpha=.6)
        ax.legend(loc='upper right')
        ax.set_title('Inf')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('# of new infections')
    elif type == 'R':
        gs = gridspec.GridSpec(nrows=1, ncols=2,)

        ax0 = fig.add_subplot(gs[0, 0])
        ax1 = fig.add_subplot(gs[0, 1])
        rt_hh = []
        rt_nhh = []
        r0_hh = []
        r0_nhh = []
        for i in range(len(Outp)):
            rt_hh.append(Outp[i].rt_hh)
            rt_nhh.append(Outp[i].rt_nhh)
            r0_hh.append(Outp[i].r0_hh)
            r0_nhh.append(Outp[i].r0_nhh)
            if i == 0:
                ax0.plot(Outp[i].rt_hh, color='red',\
                    label='household',\
                        linewidth=1, alpha=.3)
                ax0.plot(Outp[i].rt_nhh, color='green',\
                    label='non-household',\
                        linewidth=1, alpha=.3)
                ax1.plot(Outp[i].r0_hh, color='red',\
                    label='household',\
                        linewidth=1, alpha=.3)
                ax1.plot(Outp[i].r0_nhh, color='green',\
                    label='non-household',\
                        linewidth=1, alpha=.3)
            else:
                ax0.plot(Outp[i].rt_hh, color='red',\
                    # label='household',\
                        linewidth=1, alpha=.3)
                ax0.plot(Outp[i].rt_nhh, color='green',\
                    # label='non-household',\
                        linewidth=1, alpha=.3)
                ax1.plot(Outp[i].r0_hh, color='red',\
                    # label='household',\
                        linewidth=1, alpha=.3)
                ax1.plot(Outp[i].r0_nhh, color='green',\
                    # label='non-household',\
                        linewidth=1, alpha=.3)
        mean_hh = np.mean(rt_hh, axis=0)
        mean_nhh = np.mean(rt_nhh, axis=0)
        ax0.plot(mean_hh, color='blue', label='mean household', linewidth=2, alpha=.6)
        ax0.plot(mean_nhh, color='orange', label='mean non-household', linewidth=2, alpha=.6)
        ax0.plot(mean_hh + mean_nhh, color='black', label='mean total', linewidth=2, alpha=.6)
        mean_hh = np.mean(r0_hh, axis=0)
        mean_nhh = np.mean(r0_nhh, axis=0)
        ax1.plot(mean_hh, color='blue', label='mean household', linewidth=2, alpha=.6)
        ax1.plot(mean_nhh, color='orange', label='mean non-household', linewidth=2, alpha=.6)
        ax1.plot(mean_hh + mean_nhh, color='black', label='mean total', linewidth=2, alpha=.6)

        ax0.legend(loc='upper right')
        ax0.set_title('ref rt')
        ax0.set_xlabel('Time(days)')
        ax0.set_ylabel('# of new infections')
        ax1.legend(loc='upper right')
        ax1.set_title('Rhh & Rnhh')
        ax1.set_xlabel('Time(days)')
        ax1.set_ylabel('')
        plt.suptitle(f"{hh_total} * {hh_size}\n{info}")

    # plt.show()
    save = f'./{hh_total}x{hh_size}'
    if not os.path.exists(save): os.mkdir(save)
    if info == None: plt.savefig(f'{save}/{type}_goal_{goal}.png')
    else : plt.savefig(f'{save}/{type}_goal_{goal}_{info}.png')
    plt.close()


def merge_plot(Outp, ct_all, ct_hh, trans_hh, nhh_ratio):
    hh, nhh = [], []
    inc_hh, inc_nhh = [], []
    for i in range(len(Outp)):
        hh.append(Outp[i].rt_hh)
        nhh.append(Outp[i].rt_nhh)
        inc_hh.append(Outp[i].inc_hh)
        inc_nhh.append(Outp[i].inc_nhh)
        # smooth_hh = np.zeros(t_end+1)
        # smooth_nhh = np.zeros(t_end+1)
        # for t in range(t_end+1):
            # if t <= 3 or t >= t_end-3:
                # smooth_hh[t] = hh[i][t]
                # smooth_nhh[t] = nhh[i][t]
            # else:
                # smooth_hh[t] = np.mean(hh[i][t-3 : t+3])
                # smooth_nhh[t] = np.mean(hh[i][t-3 : t+3]) 
        # ax.plot(Outp[i].rt_hh + Outp[i].rt_nhh, color='black', \
            # label='total', \
                # linewidth=1, alpha=.1)

        # if i == 0:
            # ax.plot(Outp[i].rt_hh, color='red', \
                # label='household', \
                    # linewidth=1, alpha=.3)
            # ax.plot(Outp[i].rt_nhh, color='green', \
                # label='non-household', \
                    # linewidth=1, alpha=.3)
        # else:
            # ax.plot(Outp[i].rt_hh, color='red', \
                # label='household', \
                    # linewidth=1, alpha=.3)
            # ax.plot(Outp[i].rt_nhh, color='green', \
                # label='non-household', \
                    # linewidth=1, alpha=.3)

    mean_hh = np.mean(hh, axis=0)
    mean_nhh = np.mean(nhh, axis=0)
    mean_inc_hh = np.mean(inc_hh, axis=0)
    mean_inc_nhh = np.mean(inc_nhh, axis=0)
    maxRT = np.max(mean_hh + mean_nhh)
    Rhh = 0
    Rnhh = 0
    for t in range(t_end+1):
       if mean_hh[t] + mean_nhh[t] == maxRT:
           Rhh = mean_hh[t]
           Rnhh = mean_nhh[t]
           break

    fig, ax1 = plt.subplots(figsize=(9, 6))
    # ax.axvline(t_end/2, 0, color='black', linestyle='--', lw=1)
    plt.set_title(f'{hh_total} households with {hh_size} members')
    plt.set_xlabel('Time(days)')
    ax2 = ax1.twinx()
    ax1.set_ylim([0, 20])
    ax1.set_ylabel('RT rate')
    ax1.plot(mean_hh+mean_nhh, color='black', label='mean total', linewidth=2, )
    ax1.plot(mean_hh, color='green', label='mean household', linewidth=2, )
    ax1.plot(mean_nhh, color='red', label='mean non-household', linewidth=2, )
    ax1.legend(loc='center right')

    ax2.set_ylabel('# of infection per day')
    ax2.plot(mean_inc_hh+mean_inc_nhh, color='black', marker='--', linewidth=5, alpha=.4)
    ax2.plot(mean_inc_hh, color='green', marker='--', linewidth=5, alpha=.4)
    ax2.plot(mean_inc_nhh, color='red', marker='--', linewidth=5, alpha=.4)

    t = f"ct_hh: {ct_hh}, ct_nhh: {ct_all-ct_hh}\n"\
            f"trans_hh: {trans_hh}, trans_nhh: {trans_hh*nhh_ratio} (nhh_tatio: {nhh_ratio})\n"\
            f"Rt of hh = {round(Rhh, 2)}, Rt of nhh = {round(Rnhh, 2)}"
    ax1.text(30, 15, t, fontsize=12, ha='left', wrap=True)

    save = f'./{hh_total}x{hh_size}_RT_2'
    if not os.path.exists(save): os.mkdir(save)
    plt.savefig(f'{save}/{ct_all}_{ct_hh}_{trans_hh}_{nhh_ratio}.png')
    plt.close()

def plot_table(container):
    fig = plt.subplots(figure=(9, 8))
    plt.table()

if __name__ == '__main__':
    # python hh_club.py "goal" "hh_total" "hh_size" "times" "cpus" 
    # run goal 2
    goal = int(sys.argv[1])

    if hh_total == 4500:
        dataset = {'ct_all' : [3, 4, 5, 9], 
                    'ct_hh' : [1.5, 1.8, 2],
                    'trans_hh' : [.2, .3],
                    'nhh_rato' : [.25, .5]}
    else:
        dataset = {'ct_all' : [3, 4, 5, 9], 
                    'ct_hh' : [2.2, 2.5, 3],
                    'trans_hh' : [.2, .3],
                    'nhh_rato' : [.25, .5]}


    for trans_hh in dataset['trans_hh']:
        for nhh_ratio in dataset['nhh_rato']:
            trans_nhh = trans_hh * nhh_ratio
            print(f"trans_hh : {trans_hh} ; nhh_ratio : {nhh_ratio}")
            # can12 = []
            for ct_hh in dataset['ct_hh']:
                for ct_all in dataset['ct_all']:
                    container = []
                    print(f"ct_all : {ct_all} ; ct_hh : {ct_hh}")
                    start = time.time()
                    for i in range(times):
                        Outp_ = Outp()
                        Y_ = Init_case(Outp_, 5)
                        Sim(goal, Outp_, Y_, trans_hh, trans_nhh, ct_hh, ct_all)
                        """
                        thh = pd.Series(Outp_.inc_hh)
                        tnhh = pd.Series(Outp_.inc_nhh)
                        rhh = epyestim.estimate_r.estimate_r(
                            infections_ts = thh,
                            gt_distribution = g,
                            a_prior = 1,
                            b_prior = .5,
                            window_size = 1
                        )
                        rhh = epyestim.estimate_r.gamma_quantiles(0.5, rhh['a_posterior'], rhh['b_posterior'])
                        rnhh = epyestim.estimate_r.estimate_r(
                            infections_ts = tnhh,
                            gt_distribution = g,
                            a_prior = 1,
                            b_prior = .5,
                            window_size = 1
                        )
                        rnhh = epyestim.estimate_r.gamma_quantiles(0.5, rnhh['a_posterior'], rnhh['b_posterior'])
                        Outp_.r0_hh = rhh
                        Outp_.r0_nhh = rnhh
                        """
                        container.append(Outp_)
                        del Outp_
                    t = time.time() - start
                    print("%dh %dm %ds" % (t//3600, t//60, t%60))
                    # plot_result('RT', goal, container, f'{trans_hh}_{nhh_ratio}_{ct_hh}_{ct_all}')
                    merge_plot(container, ct_all, ct_hh, trans_hh, nhh_ratio)
                    # can12.append(container)

    # plot_result('IR', goal, container, 'test')
    # plot_result('RT', goal, container, 'test')
    # plot_result('Inf', goal, container, 'test')
