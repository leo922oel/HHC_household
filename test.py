from numba import cuda
import numpy as np
import time
import math

mode = 'GPU' # 'GPU' or 'CPU'

population_size = 10000                                 
total_round = 1
beta = np.float32(0.5)

# 從小到大：Thread -> Block -> Grid
# 將整個程式分成一個個batch給GPU跑
batch_size = population_size//10
threads_per_block = 256
blocks_per_grid = batch_size // threads_per_block + 1
loop = population_size // batch_size # 在GPU中每一個round總共需要跑這麼多loop

# Status: 0->Healthy / 1->Infected / 2->Immune
status = np.zeros(population_size, dtype=np.int8) # 用來記錄檢康狀態
status[0] = 1 # Patient Zero
position = np.random.rand(population_size, 2).astype(np.float32) # 用來隨機每個人的位置

# 在GPU中使用function（像這裡要用的random）的限制還蠻多的，比較方便的做法是在外面寫好後直接把資料傳進去
# 這裡直接把random的結果放在array中傳進去
random = np.random.rand(population_size).astype(np.float32) 


@cuda.jit # 加這行表示以下是GPU的code
def sir_gpu(Num, total_round, batch_size, batch_idx, round_, status, position, random, beta):
    idx = cuda.blockDim.x * cuda.blockIdx.x + cuda.threadIdx.x # 計算現在在的thread位置
    if idx < batch_size: # 必須加這行確定idx沒有超過範圍
        # 下面用兩個迴圈計算人跟人之間的距離是否有可能發生傳染
        # 外圈分成一個個batch來計算，內圈是完整（=人數）的迴圈
        if status[idx + batch_idx*batch_size] == 1: 
            for i in range(Num):
                if status[i] == 0:
                    new_X = position[idx + batch_idx*batch_size, 0] - position[i, 0]
                    new_Y = position[idx + batch_idx*batch_size, 1] - position[i, 1]
                    distance = math.sqrt(new_X ** 2 + new_Y ** 2)
                    p = random[i]
                    if(p<beta and distance<0.1):
                        status[i] = 1
        # 更新位置
        position[idx + batch_idx*batch_size, 0] += random[idx + batch_idx*batch_size]
        position[idx + batch_idx*batch_size, 1] += random[idx + batch_idx*batch_size]

def sir_cpu(Num, round_, status, position, random, beta):
    # 和GPU的差不多，除了這裡沒有分成一個個batch去計算
    for person1 in range(Num):
        if status[person1] == 1:
            for person2 in range(Num):
                if status[person2] == 0:
                    new_X = position[person1, 0] - position[person2, 0]
                    new_Y = position[person1, 1] - position[person2, 1]
                    distance = math.sqrt(new_X ** 2 + new_Y ** 2)
                    p = random[person2]
                    if(p<beta and distance<0.1):
                        status[person2] = 1
        position[person1, 0] += random[person1]
        position[person1, 1] += random[person1]


if __name__ == '__main__': # main function得寫在這裡面

    start = time.time()
    if mode == 'GPU':
        for i in range(total_round):
            for gpu_loop in range(loop): # loop＝每一個round要分成幾個batch跑完
                # call function時會需要在後面加 [blocks_per_grid, threads_per_block] 告訴GPU blocks/thread數量
                sir_gpu[blocks_per_grid, threads_per_block](population_size, total_round, batch_size, gpu_loop, i, status, position, random, beta)
    if mode == 'CPU':
        for i in range(total_round):
            sir_cpu(population_size, i, status, position, random, beta)
    
    healthy = 0
    infected = 0
    for i in range(population_size):
        if status[i] == 0:
            healthy+=1
        if status[i] == 1:
            infected+=1

    end = time.time()
    print(f'Healthy Cases: {healthy}, Infected Cases:{infected}')
    print(f'Mode: {mode}, Executed Time: {end-start}')