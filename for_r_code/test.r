hh_total <- 3000 # total households
hh_size <- 3 # average number of household
pop <- hh_total * hh_size # total population
dur_infect <- 5 # infectiousness period = 5 days
gamma_rate <- 1 / dur_infect # recovery rate per day
trans_hh <- 1 / dur_infect # transmission probability, household
nhh_ratio <- 0.18 # relative risk of transmission, non-household vs household
trans_nhh <- trans_hh*nhh_ratio # transmission probability, non-household
mask_eff <- 0.8 # relative risk of mask wearing policy

t_end <- 50 # timeframe for simulation (days)
Outp <- data.frame(
Time = 0:t_end,
I = rep(0, t_end+1),
R = rep(0, t_end+1),
inc_hh = rep(0, t_end+1),
inc_nhh = rep(0, t_end+1),
rt_hh = rep(0, t_end+1),
rt_nhh = rep(0, t_end+1),
inf_hh_0 = rep(0, t_end+1),
inf_hh_1 = rep(0, t_end+1),
inf_hh_2 = rep(0, t_end+1)
)

t <- 0
Y <- matrix(0, hh_total, hh_size)
Y[1:5, 1] <- 1
Outp$I[1] <- sum(Y == 1)
Outp$R[1] <- sum(Y == 2)
Outp$inf_hh_0[1] <- table(rowSums((Y == 1)|(Y == 2)))[1]
Outp$inf_hh_1[1] <- table(rowSums((Y == 1)|(Y == 2)))[2]

for (t in 1:t_end){
    if (t <= t_end/2){
        ct_all = 9.14 # number of daily contacts, total
        ct_hh = 2.26 # number of daily contacts, household
        ct_nhh = ct_all - ct_hh # number of daily contacts, non-household
        beta_hh = ct_hh*trans_hh # transmission rate per day, household (density-dependent)
        beta_nhh = ct_nhh*trans_nhh/pop # transmission rate per day, non-household (frequency-dependent)
        if (t == 1) {
            print("=== transmission rates, survey wave 1 ===")
            print(paste0("household = ", sprintf("%1.2e", beta_hh)))
            print(paste0("non-household = ", sprintf("%1.2e", beta_nhh)))
        }
    } else {
        ct_all = 3.97
        ct_hh = 2.20
        ct_nhh = ct_all - ct_hh
        beta_hh = ct_hh*trans_hh
        beta_nhh = (ct_nhh*trans_nhh/pop)*mask_eff
        if (t == (t_end/2+1)) {
            print("=== transmission rates, survey wave 3 ===")
            print(paste0("household = ", sprintf("%1.2e", beta_hh)))
            print(paste0("non-household = ", sprintf("%1.2e", beta_nhh)))
        }
    }
    # Event (A): household transmission
    # -- occurs when there are both susceptible and infectious in a household
    # -- depends on (household transmission rate) * (cases in the same household)
    inc_hh <- t(apply(Y, 1, function(y) {
        rbinom(hh_size, size = 1, prob = pexp(1, beta_hh * sum(y == 1)))
    }))
    inc_hh[Y != 0] <- 0
    # Event (B): non-household transmission
    # -- depends on (non-household transmission rate) *
    # (total cases in the population excluding those in the same household)
    n_I <- sum(Y == 1)
    inc_nhh <- t(apply(Y, 1, function(y) {
        rbinom(hh_size, size = 1, prob = pexp(1, beta_nhh * (n_I - sum(y == 1))))
    }))
    inc_nhh[Y != 0] <- 0
    inc_nhh[inc_hh == 1] <- 0
    # -- alternative assumption for non-household transmission:
    # includes contacts with own family outside household
    # inc_nhh <- matrix(rbinom(hh_total * hh_size, size = 1,
    # prob = pexp(1, beta_nhh * sum(Y == 1))),
    # hh_total, hh_size)
    # inc_nhh[Y != 0] <- 0
    # inc_nhh[inc_hh == 1] <- 0
    # ------------------------------------------------------------------------------------------------------
    # Event (C): recovery
    recov <- matrix(rbinom(hh_total, size = 1, prob = pexp(1, gamma_rate)),
                        hh_total, hh_size)
    # calculate Rt
    rt_hh <- t(apply(Y, 1, function(y) {
        rbinom(hh_size, size = 1, prob = pexp(1 / gamma_rate, beta_hh * sum(y == 1)))
    }))
    rt_hh[Y != 0] <- 0
    rt_nhh <- t(apply(Y, 1, function(y) {
        rbinom(hh_size, size = 1, prob = pexp(1 / gamma_rate, beta_nhh * (n_I - sum(y == 1))))
    }))
    rt_nhh[Y != 0] <- 0
    rt_nhh[rt_hh == 1] <- 0
    Outp$rt_hh[t+1] = sum(rt_hh)/sum(Y == 1)
    Outp$rt_nhh[t+1] = sum(rt_nhh)/sum(Y == 1)

    # number of new infections
    Outp$inc_hh[t+1] = sum(inc_hh) # from household transmission
    Outp$inc_nhh[t+1] = sum(inc_nhh) # from non-household transmission
    # update output
    Y[Y == 0 & (inc_hh == 1 | inc_nhh)] <- 1
    Y[Y == 1 & recov == 1] <- 2
    Outp$I[t+1] <- sum(Y == 1)
    Outp$R[t+1] <- sum(Y == 2)
    # number of households with 0, 1, 2 infected populations
    Outp$inf_hh_0[t+1] <- table(rowSums((Y == 1)|(Y == 2)))[1]
    Outp$inf_hh_1[t+1] <- table(rowSums((Y == 1)|(Y == 2)))[2]
    Outp$inf_hh_2[t+1] <- table(rowSums((Y == 1)|(Y == 2)))[3]
}
t<-0
Outp$I[1] <- Outp$I[1] / pop
for (t in 1:t_end) {
    Outp$I[t+1] <- Outp$I[t+1] / pop
    Outp$R[t+1] <- Outp$R[t+1] / pop
}

png("IR.png", width = 640, height = 400)
plot(Outp$R, 
        main='IR',
        col='green',
        type='l',
        xlab='Time(Day)',
        ylab='Proportion among total pop',
        lwd=2,
        #ylim=c(0,0.5)
        )
lines(Outp$I,
        col='red',
        lwd=2)
legend("topright", c("I", "R"),
        fill=c("red", "green"))
abline(v = t_end / 2, col='black', lty=2)


png("RT.png", width = 640, height = 400)

plot((Outp$rt_hh + Outp$rt_nhh), 
        main='RT',
        col='black',
        type='l',
        xlab='Time(Day)',
        ylab='RT',
        lwd=2,
        #ylim=c(0,0.5)
        )
lines(Outp$rt_hh,

        col='red',
        lwd=2)
lines(Outp$rt_nhh,
        col='green',
        lwd=2)
legend("topright", c("total", "hh", "nhh"),
        fill=c("black", "red", "green"))
abline(v = t_end / 2, col='black', lty=2)


png("Inf.png", width = 640, height = 400)
plot(Outp$inc_hh, 
        main='Inf of hh & nhh',
        col='red',
        type='l',
        xlab='Time(Day)',
        ylab='nuber of new infections',
        lwd=2,
        #ylim=c(0,0.5)
        )
lines(Outp$inc_nhh,
        col='green',
        lwd=2)

legend("topright", c("hh", "nhh"),
        fill=c("red", "green"))
abline(v = t_end / 2, col='black', lty=2)