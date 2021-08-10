
from numba import cuda
import numpy as np
import math
from time import time

#print(cuda.gpus) # No.0 GPU by default
# use "CUDA_VISIBLE_DEVICES='{No.}' python a.py" to use specify
# borrow the mimic GPU from Numba by "SET NUMBA_ENABLE_CUDASIM=1"

@cuda.jit
def matmul(A, B, C):
    """
    """
    row = cuda.threadIdx.x + cuda.blockDim.x * cuda.blockIdx.x
    col = cuda.threadIdx.y + cuda.blockDim.y * cuda.blockIdx.y


    if row < C.shape[0] and col < C.shape[1]:
        tmp = 0.
        for k in range(A.shape[1]):
            tmp += A[row, k] * B[k, col]
        C[row, col] = tmp

def main():
    M = 6000
    N = 4800
    P = 4000
    A = np.random.random((M, N)) 
    B = np.random.random((N, P)) 

    start = time()
    A = cuda.to_device(A)
    B = cuda.to_device(B)
    C_gpu = cuda.device_array((M, P))


    threads_per_block = (2, 4)
    blocks_per_grid_x = int(math.ceil(A.shape[0] / threads_per_block[0]))
    blocks_per_grid_y = int(math.ceil(B.shape[1] / threads_per_block[1]))
    blocksPerGrid = (blocks_per_grid_x, blocks_per_grid_y)


    matmul[blocksPerGrid, threads_per_block](A, B, C_gpu)


    C = C_gpu.copy_to_host()
    cuda.synchronize()

    print("gpu matmul time :" + str(time() - start))


    start = time()
    C_cpu = np.empty((M, P), np.float)
    np.matmul(A, B, C_cpu)
    print("cpu matmul time :" + str(time() - start))

    if np.allclose(C_cpu, C):
        print("gpu result correct")

if __name__ == "__main__":
    main()