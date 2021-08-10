# Household sim Model

---------------------------------------------
## 2021/08/10 progress
    I change 'list' to 'array' to store data, and it make me faster sim time which no longer use 'append' to allocate storge. that really be faster so much. Then now I stop trying gpu coding and multiprocessing because those method will actually slow down the compile time by moving the data in and out. Maybe I will keep studing those tech in future, but now I keep it out. Next work is to implement the dynamics array to allocate household size. Maybe it will need list again and I will back to learn multiprocessing.    
