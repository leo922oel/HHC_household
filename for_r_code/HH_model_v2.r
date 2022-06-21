"
hh_total: total households
hh_size: average number of household
ct_all: contact rate for all
ct_hh: contact rate for household
ct_nhh: number of daily contacts for non household
pop: population
dur_infect: infectiousness period (days)
gamma_rate: recovery rate per day
trans_hh: transmission probability for household
nhh_ratio: relative risk of transmission for non-household vs household
trans_nhh: transmission probability for non-household
mask_eff: relative risk of mask wearing policy
t_end: timeframe for simulation -- days
times: # of simulation

beta_hh: transmission rate per day for household (density-dependent)
beta_nhh: transmission rate per day for non-household (frequency-dependent)
"
########### server args ###############
args <- commandArgs(trailingOnly = TRUE)
hh_total <- as.integer(args[1])
hh_size <- as.integer(args[2])
ct_all <- as.integer(args[3])
# ct_hh <- as.numeric(args[4])
savepath <- "nas/"

########### given params ###############
# hh_total <- 3000 # total households
# hh_size <- 3 # average number of household
# savepath <- "nas/"

if (!dir.exists(paste0(savepath, hh_total))) dir.create(paste0(savepath, hh_total))
pop <- hh_total * hh_size 
dur_infect <- 5
gamma_rate <- 1 / dur_infect
# trans_hh <- 1 / dur_infect
# nhh_ratio <- 0.18
# trans_nhh <- trans_hh * nhh_ratio
mask_eff <- 0.8
t_end <- 90
times <- 50

### plot cumulative Infection and Recover cases for each epoch
plot_I_and_R <- function(I, R, filename) {
    png(paste0(filename, "_IR.png"), width = 840, height = 600, type="cairo")
    for (i in 1:times) {
        t <- 0
        I[i][1] <- I[i][1] / pop
        for (t in 1:t_end) {
            I[i, t +1] <- I[i, t + 1] / pop
            R[i, t + 1] <- R[i, t + 1] / pop
        }
        if (i == 1) {
            plot(R[i, ],
                    main = "IR",
                    col = rgb(0, 255, 0, 60, maxColorValue = 255),
                    type = "l",
                    xlab = "Time(Day)",
                    ylab = "Proportion among total pop",
                    lwd = 2,
                    ylim = c(0, 1)
                    )
        }
        else {
            lines(R[i, ],
                    col = rgb(0, 255, 0, 60, maxColorValue = 255),
                    lwd = 2)
        }

        lines(I[i, ],
                col = rgb(255, 0, 0, 60, maxColorValue = 255),
                lwd = 2)
    }
    legend("topright", c("I", "R"),
            fill = c("red", "green"))
    # abline(v = t_end / 2, col = "black", lty = 2)
    dev.off()
    return()
}

### store mean statistic as .csv file
write_meanRT <- function(I, R, rt_hh, rt_nhh, filename) {
    mean_I <- rep(0, t_end + 1)
    mean_R <- rep(0, t_end + 1)
    mean_hh <- rep(0, t_end + 1)
    mean_nhh <- rep(0, t_end + 1)
    for (i in 1:t_end+1) {
        mean_I[i] <- mean(I[, i], na.rm=T)
        mean_R[i] <- mean(R[, i], na.rm=T)
        mean_hh[i] <- mean(rt_hh[, i], na.rm=T)
        mean_nhh[i] <- mean(rt_nhh[, i], na.rm=T)
    }

    save.df <- data.frame(
        mean_I = mean_I,
        mean_R = mean_R,
        mean_hh = mean_hh,
        mean_nhh = mean_nhh
    )
    write.csv(save.df, file=paste0(filename, "_mean.csv"))
    return()
}

### plot daily Rt for each epoch
plot_RT <- function(rt_hh, rt_nhh, defR, filename) {
    png(paste0(filename, "_RT.png"), width = 840, height = 600, type="cairo")
    for (i in 1:times) {
        if (i == 1) {
            plot((rt_hh[i, ] + rt_nhh[i, ]), 
                    main = "RT",
                    col = rgb(0, 0, 0, 60, maxColorValue = 255),
                    type = "l",
                    xlab = "Time(Day)",
                    ylab = "RT",
                    lwd = 2,
                    # ylim = c(0,0.5)
            )
        }
        else {
            lines((rt_hh[i, ] + rt_nhh[i, ]),
                    col = rgb(0, 0, 0, 60, maxColorValue = 255),
                    lwd = 2)
        }
        lines(rt_hh[i, ],
                col = rgb(255, 0, 0, 60, maxColorValue = 255),
                lwd = 2)
        lines(rt_nhh[i, ],
                col = rgb(0, 255, 0, 60, maxColorValue = 255),
                lwd = 2)
    }
 
    legend("topright", c("total", "hh", "nhh"),
            fill=c("black", "red", "green"))
    # abline(v = t_end / 2, col = 'black', lty = 2)

    dev.off()
    return()
}

### plot daily Infection for each epoch
plot_Inf <- function(inc_hh, inc_nhh, filename) {
    png(paste0(filename, "_Inf.png"), width = 840, height = 600, type="cairo")
    for (i in 1:times) {
        if (i == 1) {
            plot(inc_hh[i, ], 
                    main = "Inf of hh & nhh",
                    col = rgb(255, 0, 0, 60, maxColorValue = 255),
                    type = "l",
                    xlab = "Time(Day)",
                    ylab = "nuber of new infections",
                    lwd = 2,
                    #ylim = c(0, 1)
            )
        }
        else {
            lines(inc_hh[i, ],
                    col = rgb(255, 0, 0, 60, maxColorValue = 255),
                    lwd = 2)
        }
        lines(inc_nhh[i, ],
                col = rgb(0, 255, 0, 60, maxColorValue = 255),
                lwd = 2)
    }


    legend("topright", c("hh", "nhh"),
            fill=c("red", "green"))
    # abline(v = t_end / 2, col = "black", lty = 2)

    dev.off()
    return()
}

### container to store data of one epoch
Outp <- data.frame(
    Time = 0 : t_end,
    I = rep(0, t_end + 1),
    R = rep(0, t_end + 1),
    inc_hh = rep(0, t_end + 1),
    inc_nhh = rep(0, t_end + 1),
    rt_hh = rep(0, t_end + 1),
    rt_nhh = rep(0, t_end + 1)
)

### matrix for statistic analysis
mI = matrix(0, times, t_end + 1)
mR = matrix(0, times, t_end + 1)
minc_hh = matrix(0, times, t_end + 1)
minc_nhh = matrix(0, times, t_end + 1)
mrt_hh = matrix(0, times, t_end + 1)
mrt_nhh = matrix(0, times, t_end + 1)

# beta_hh <- ct_hh * trans_hh
# beta_nhh <- ct_nhh * trans_nhh / pop

###　household structure set up
if (hh_size == 3) {
    # ct_all <- c(3, 4, 5, 9)
    ct_hh.set <- c(1, 1.5, 2, 2.5, 3)
} else if (hh_size == 2) {
    # ct_all <- c(2, 3, 4, 5, 9)
    ct_hh.set <- c(1, 1.5, 1.8, 2)
} else if (hh_size == 5){
    # ct_all <- c(6, 8, 9, 11)
    ct_hh.set <- c(2, 3, 4, 5)
} else {
    # ct_all <- c(10, 12, 13, 15)
    ct_hh.set <- c(3, 5, 7, 9)
}

trans_hh.set <- c(0.2, 0.3)
nhh_ratio.set <- c(0.25, 0.5)

### Simulation
### state >> 0: susceptible, 1: infectious, 2: recovery
for (ct_all in ct_all.set) {
    for (ct_hh in ct_hh.set) {
        print(paste0("ct_hh: ", ct_hh, "   ct_all: ", ct_all))
        print("-----------------------------")

        ct_nhh <- ct_all - ct_hh

        # filename <- paste0("nas/",hh_total, "/", hh_total, "_", row, "_", col)
        # png(paste0(filename, ".png"), width = 840, height = 600, type="cairo")
        # par(mfrow = c(length(trans_hh), length(nhh_ratio)))

        for (trans_hh in trans_hh.set) {
            for(nhh_ratio in nhh_ratio.set) {
                set.seed(1000)
        	    filename <- paste0("nas/",hh_total, "/", ct_all, "_", ct_hh, "_", trans_hh, "_", nhh_ratio)
        	    #png(paste0(filename, ".png"), width = 840, height = 600, type="cairo")
                trans_nhh <- trans_hh * nhh_ratio
                beta_hh <- ct_hh * trans_hh
                beta_nhh <- (ct_nhh * trans_nhh / pop) * mask_eff

                defR_nhh <- ct_nhh * trans_nhh * mask_eff * dur_infect
                defR_hh <- ct_hh * trans_hh * dur_infect
                defR <- defR_nhh + defR_hh / ( 1 + defR_hh)

                print(paste0("trans_hh: ", trans_hh, "   nhh_ratio: ", nhh_ratio))

                for (i in 1:times) { ### epoch
                    print(paste0("round = ", sprintf("%d", i)))

                    ### Initialize the container
                    Outp$I <- rep(0, t_end + 1)
                    Outp$R <- rep(0, t_end + 1)
                    Outp$inc_hh <- rep(0, t_end + 1)
                    Outp$inc_nhh <- rep(0, t_end + 1)
                    Outp$rt_hh <- rep(0, t_end + 1)
                    Outp$rt_nhh <- rep(0, t_end + 1)

                    t <- 0
                    Y <- matrix(0, hh_total, hh_size) ### store daily healthy state
                    Y[1:5, 1] <- 1
                    Outp$I[0] <- sum(Y == 1)
                    Outp$R[0] <- sum(Y == 2)
                    Outp$inf_hh_0[0] <- table(rowSums((Y == 1) | (Y == 2)))[1]
                    Outp$inf_hh_1[0] <- table(rowSums((Y == 1) | (Y == 2)))[2]

                    for (t in 0:t_end) {
                        # case1 : household transmission
                        # -- occurs when there are both susceptible & infectious in a household
                        # -- depends on (household transmission rate) * (case in the same household)
                        inc_hh <- t(apply(Y, 1, function(y) {
                            rbinom(hh_size, size = 1, prob = pexp(1, beta_hh * sum(y == 1)))
                        }))
                        inc_hh[Y != 0] <- 0

                        # case2 : non-household transmission
                        # -- depends on (non-household transimission rate) *
                        # (total cases in the pop excluding those in the same household)
                        n_I <- sum(Y == 1)
                        inc_nhh <- t(apply(Y, 1, function(y) {
                            rbinom(hh_size, size = 1, prob = pexp(1, beta_nhh * (n_I - sum(y == 1))))
                        }))
                        inc_nhh[Y != 0] <- 0
                        inc_nhh[inc_hh == 1] <- 0

                        # case3 : recovery
                        recover <- matrix(rbinom(hh_total, size = 1, prob = pexp(1, gamma_rate)),
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
                        Outp$rt_hh[t + 1] <- sum(rt_hh) / sum(Y == 1)
                        Outp$rt_nhh[t + 1] <- sum(rt_nhh) / sum(Y == 1)

                        # update the number of new infectious
                        Outp$inc_hh[t + 1] <- sum(inc_hh)
                        Outp$inc_nhh[t + 1] <- sum(inc_nhh)

                        # update output
                        Y[Y == 0 & (inc_hh == 1 | inc_nhh)] <- 1
                        Y[Y == 1 & recover == 1] <- 2
                        Outp$I[t + 1] <- sum(Y == 1)
                        Outp$R[t + 1] <- sum(Y == 2)

                        # number of households with 0, 1, 2 infected pop
                    }

                    mI[i, ] <- Outp$I
                    mR[i, ] <- Outp$R
                    minc_hh[i, ] <- Outp$inc_hh
                    minc_nhh[i, ] <- Outp$inc_nhh
                    mrt_hh[i, ] <- Outp$rt_hh
                    mrt_nhh[i, ] <- Outp$rt_nhh
                }

                write_meanRT(mI, mR, mrt_hh, mrt_nhh, filename)
                # plot_RT(mrt_hh, mrt_nhh, defR, filename)
                # plot_I_and_R(mI, mR, filename)
                #plot_Inf(minc_hh, minc_nhh, filename)
            }
            
        }
        # legend("topright", c("total", "hh", "nhh"),
                    # fill=c("black", "red", "green"))
        # dev.off()
    }
}
