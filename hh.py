from os import stat
import numpy as np
import numpy
import random as rd
import math
from scipy.stats import expon
import matplotlib.pyplot as plt
from numba import cuda
import time
import cupy as cp
import sys

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
t_end = 25



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
    # Y = []
    # b = get_hh_size()
    # for i in range(len(b)):
        # Y.append(np.zeros(b[i]))
    # for i in range(init_inf):
        # Y[i][0] = 1
    Y[0:5, 0] = 1
    Outp.I[0] = np.sum(Y == 1)
    Outp.R[0] = np.sum(Y == 2)
    return Y

@cuda.jit
def gpu_rbinom(type, p, container):
    row = cuda.threadIdx.x + cuda.blockDim.x * cuda.blockIdx.x
    # col = cuda.threadIdx.y + cuda.blockDim.x * cuda.blockIdx.y

    if row < container.shape[0]:
        # for i in range(hh_total):
        container[row] = np.random.binomial(1, p=p, size=3)

@cuda.jit
def gpu_where(Y, tmp1, tmp2, t):
    row = cuda.threadIdx.x + cuda.blockDim.x * cuda.blockIdx.x
    col = cuda.threadIdx.y + cuda.blockDim.y * cuda.blockIdx.y

    # result[row][col] = np.where(Y[row][col] != 0, 0, tmp1[row][col])
    if row < Y.shape[0] and col < Y.shape[1]:
        tmp1[row][col] = 0 if (Y[row][col] != 0) else tmp1[row][col]
        if t == 2:
            # result[row][col] = np.where(tmp2[row][col] == 1, 0, result[row][col])
            tmp1[row][col] = 0 if (tmp2[row][col] == 1) else tmp1[row][col]

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

        if t == 1:
            printLog(1, beta_hh, beta_nhh)
        if t == t_end//2 + 1:
            printLog(3, beta_hh, beta_nhh)
        
        inc_hh = []
        inc_nhh = []
        rt_hh = []
        rt_nhh = []
        n_I = np.sum(Y == 1)

        for i in range(hh_total):
            P = expon.cdf(1, scale=1/(beta_hh*np.sum(Y[i] == 1))) if beta_hh*np.sum(Y[i] == 1) != 0 else 0
            inc_hh.append(np.random.binomial(1, p=P, size=len(Y[i])))
            P = expon.cdf(1, scale=1/(beta_nhh*(n_I - np.sum(Y[i] == 1)))) if beta_nhh*(n_I - np.sum(Y[i] == 1)) != 0 else 0
            inc_nhh.append(np.random.binomial(1, p=P, size=len(Y[i])))
            P = expon.cdf(1/gamma_rate, scale=1/(beta_hh*np.sum(Y[i] == 1))) if beta_hh*np.sum(Y[i] == 1) != 0 else 0
            rt_hh.append(np.random.binomial(1, p=P, size=len(Y[i])))
            P = expon.cdf(1/gamma_rate, scale=1/(beta_nhh*(n_I - np.sum(Y[i] == 1)))) if beta_nhh*(n_I - np.sum(Y[i] == 1)) != 0 else 0
            rt_nhh.append(np.random.binomial(1, p=P, size=len(Y[i])))

        Y_device = cuda.to_device(Y)
        inc_hh_device = cuda.to_device(inc_hh)
        inc_nhh_device = cuda.to_device(inc_nhh)
        rt_hh_device = cuda.to_device(rt_hh)
        rt_nhh_device = cuda.to_device(rt_nhh)

        threads_per_block = (16, 16)
        blocks_per_grid_x = int(math.ceil(Y_device.shape[0] / threads_per_block[0]))
        blocks_per_grid_y = int(math.ceil(Y_device.shape[1] / threads_per_block[1]))
        blocks_per_grid = (blocks_per_grid_x, blocks_per_grid_y)
        #result = cuda.device_array((hh_total, hh_size))
        gpu_where[blocks_per_grid, threads_per_block] (Y_device, inc_hh_device, inc_hh_device, 1)
        inc_hh = inc_hh_device.copy_to_host()
        #result = cuda.device_array((hh_total, hh_size))
        gpu_where[blocks_per_grid, threads_per_block] (Y_device, inc_nhh_device, inc_hh_device, 2)
        inc_nhh = inc_nhh_device.copy_to_host()
        #result = cuda.device_array((hh_total, hh_size))
        gpu_where[blocks_per_grid, threads_per_block] (Y_device, rt_hh_device, rt_hh_device, 1)
        rt_hh =rt_hh_device.copy_to_host()
        #result = cuda.device_array((hh_total, hh_size))
        gpu_where[blocks_per_grid, threads_per_block] (Y_device, rt_nhh_device, rt_hh_device, 2)
        rt_nhh = rt_nhh_device.copy_to_host()
        cuda.synchronize()

        # inc_hh = np.where(Y != 0, 0, inc_hh)
        # inc_nhh = np.where(Y != 0, 0, inc_nhh)
        # inc_nhh = np.where(inc_hh == 1, 0, inc_nhh)
        # rt_hh = np.where(Y != 0, 0, rt_hh)
        # rt_nhh = np.where(Y != 0, 0, rt_nhh)
        # rt_nhh = np.where(rt_hh == 1, 0, rt_nhh)

        recover = np.random.binomial(1, expon.cdf(1, scale=1/gamma_rate), pop)
        recover = recover.reshape(hh_total, hh_size)
        # recover_shap = Y
        # k=0
        # for i in range(len(Y)):
            # for j in range(len(Y[i])):
                # recover_shap[i][j] = recover[k]
                # k += 1
        Outp.rt_hh[t + 1] = np.sum(rt_hh) / np.sum(Y == 1)
        Outp.rt_nhh[t + 1] = np.sum(rt_nhh) / np.sum(Y == 1)

        Outp.inc_hh[t + 1] = np.sum(inc_hh)
        Outp.inc_nhh[t + 1] = np.sum(inc_nhh)

        for i in range(len(Y)):
            for j in range(len(Y[i])):
                if Y[i, j] == 0 and (inc_hh[i, j] == 1 or inc_nhh[i, j] == 1): Y[i, j] = 1
                elif Y[i, j] == 1 and recover[i, j] == 1: Y[i, j] = 2
        Outp.I[t + 1] = np.sum(Y == 1)
        Outp.R[t + 1] = np.sum(Y == 2)

        # Outp.inf_hh_0[t + 1] = np.sum(Y == 1, axis=0)[0]
        # Outp.inf_hh_0[t + 1] += np.sum(Y == 2, axis=0)[0]
        # Outp.inf_hh_1[t + 1] = np.sum(Y == 1, axis=0)[1]
        # Outp.inf_hh_1[t + 1] += np.sum(Y == 2, axis=0)[1]
        # Outp.inf_hh_2[t + 1] = np.sum(Y == 1, axis=0)[2]
        # Outp.inf_hh_2[t + 1] += np.sum(Y == 2, axis=0)[2]

def printLog(wave, beta_hh, beta_nhh):
    print(f'===transmission rate, survey wave {wave}===')
    print("household = %.2e" % beta_hh)
    print("non-household = %.2e" % beta_nhh)

def plot_result(type, goal, Outp):
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
        ax.set_title('IR')
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
        ax.set_title('RT')
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
        ax.set_title('Inf')
        ax.set_xlabel('Time(days)')
        ax.set_ylabel('# of new infections')
    # plt.show()
    plt.savefig(f'{type}_goal_{goal}.png')


if __name__ == '__main__':
    goal = int(sys.argv[1])

    container = []

    # start = time.time()
    for i in range(times):
        print(f'round: {i+1}')
        tic = time.time()
        Outp_ = Outp()
        Y_ = Init_case(Outp_, 5)
        Sim(goal, Outp_, Y_)
        container.append(Outp_)
        del Outp_
    # print(f'time : {time.time() - start}')
    # plot_result('IR', goal, container)
    # plot_result('RT', goal, container)
    # plot_result('Inf', goal, container)