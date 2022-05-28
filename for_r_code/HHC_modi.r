args <- commandArgs(trailingOnly = TRUE)
hh_total <- as.integer(args[1])
# hh_total <- 3000 # total households
hh_size <- as.integer(args[2])
if (!dir.exists(paste0("nas/", hh_total))) dir.create(paste0("nas/", hh_total))
# hh_size <- 3 # average number of household
pop <- hh_total * hh_size # total population
dur_infect <- 5 # infectiousness period = 5 days
gamma_rate <- 1 / dur_infect # recovery rate per day
# trans_hh <- 1 / dur_infect # transmission probability for household
# nhh_ratio <- 0.18 # relative risk of transmission for non-household vs household
# trans_nhh <- trans_hh * nhh_ratio # transmission probability for non-household
mask_eff <- 0.8 # relative risk of mask wearing policy
t_end <- 60 # timeframe for simulation -- days
times <- 50


plot_I_and_R <- function(I, R, filename) {
    i <- 1
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

write_meanRT <- function(rt_hh, rt_nhh, filename) {
    mean_hh <- rep(0, t_end + 1)
    mean_nhh <- rep(0, t_end + 1)
    for (i in 1:t_end+1) {
        mean_hh[i] <- mean(rt_hh[, i], na.rm=T)
        mean_nhh[i] <- mean(rt_nhh[, i], na.rm=T)
    }

    save.df <- data.frame(
        mean_hh = mean_hh,
        mean_nhh = mean_nhh
    )
    write.csv(save.df, file=paste0(filename, "_mean.csv"))

    return()
}

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

plot_Inf <- function(inc_hh, inc_nhh, filename) {
    i <- 1
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

i <- 1

Outp <- data.frame(
    Time = 0 : t_end,
    I = rep(0, t_end + 1),
    R = rep(0, t_end + 1),
    inc_hh = rep(0, t_end + 1),
    inc_nhh = rep(0, t_end + 1),
    rt_hh = rep(0, t_end + 1),
    rt_nhh = rep(0, t_end + 1)
)

mI = matrix(0, times, t_end + 1)
mR = matrix(0, times, t_end + 1)
minc_hh = matrix(0, times, t_end + 1)
minc_nhh = matrix(0, times, t_end + 1)
mrt_hh = matrix(0, times, t_end + 1)
mrt_nhh = matrix(0, times, t_end + 1)


# ct_all <- 9.14 # number of daily contacts for total
# ct_hh <- 2.26 # number of daily contacts for household
# ct_nhh <- ct_all - ct_hh # number of daily contacts for household
# beta_hh <- ct_hh * trans_hh
# transmission rate per day
# for household (density-dependent)
# beta_nhh <- ct_nhh * trans_nhh / pop
# transmission rate per day
# for non-household (frequency-dependent)

# if (hh_total == 3000) {
#     ct_all <- c(3, 4, 5, 9)
#     ct_hh <- c(1, 1.5, 2, 2.5, 3)
#     trans_hh <- c(0.2, 0.3)
#     nhh_ratio <- c(0.25, 0.5)
# } else if (hh_total == 4500) {
#     ct_all <- c(2, 3, 4, 5, 9)
#     ct_hh <- c(1, 1.5, 1.8, 2)
#     trans_hh <- c(0.2, 0.3)
#     nhh_ratio <- c(0.25, 0.5)
# } else {
#     ct_all <- c(6, 8, 9, 11)
#     ct_hh <- c(2, 3, 4, 5)
#     trans_hh <- c(0.2, 0.3)
#     nhh_ratio <- c(0.25, 0.5)
# }
ct_all <- as.integer(args[3])
ct_hh <- as.integer(args[4])
trans_hh <- c(0.2, 0.3)
nhh_ratio <- c(0.25, 0.5)


row <- 1
for (ct_all_ in ct_all) {
    col <- 1
    for (ct_hh_ in ct_hh) {
        print(paste0("ct_hh: ", ct_hh_, "   ct_all: ", ct_all_))
        print("-----------------------------")

        ct_nhh <- ct_all_ - ct_hh_ # number of daily contacts for household

        # filename <- paste0("nas/",hh_total, "/", hh_total, "_", row, "_", col)
        # png(paste0(filename, ".png"), width = 840, height = 600, type="cairo")
        # par(mfrow = c(length(trans_hh), length(nhh_ratio)))

        for (hh_tr in trans_hh) {
            for(ratio_nhh in nhh_ratio) {
        	filename <- paste0("nas/",hh_total, "/", ct_all_, "_", ct_hh_, "_", hh_tr, "_", ratio_nhh)
        	#png(paste0(filename, ".png"), width = 840, height = 600, type="cairo")
                trans_nhh <- hh_tr * ratio_nhh
                beta_hh <- ct_hh_ * hh_tr
                beta_nhh <- (ct_nhh * trans_nhh / pop) * mask_eff

                defR_nhh <- ct_nhh * trans_nhh * mask_eff * dur_infect
                defR_hh <- ct_hh_ * hh_tr * dur_infect
                defR <- defR_nhh + defR_hh/(1+defR_hh)

                print(paste0("trans_hh: ", hh_tr, "   nhh_ratio: ", ratio_nhh))

                system.time(
                for (i in 1:times) {
                    print(paste0("round = ", sprintf("%d", i)))
                    Outp$I <- rep(0, t_end + 1)
                    Outp$R <- rep(0, t_end + 1)
                    Outp$inc_hh <- rep(0, t_end + 1)
                    Outp$inc_nhh <- rep(0, t_end + 1)
                    Outp$rt_hh <- rep(0, t_end + 1)
                    Outp$rt_nhh <- rep(0, t_end + 1)

                    t <- 0
                    Y <- matrix(0, hh_total, hh_size)
                    Y[1:5, 1] <- 1
                    Outp$I[1] <- sum(Y == 1)
                    Outp$R[1] <- sum(Y == 2)
                    Outp$inf_hh_0[1] <- table(rowSums((Y == 1) | (Y == 2)))[1]
                    Outp$inf_hh_1[1] <- table(rowSums((Y == 1) | (Y == 2)))[2]

                    for (t in 1:t_end) {
                        # case1 : huosehold transmission
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
                )

                write_meanRT(mrt_hh, mrt_nhh, filename)
                # plot_RT(mrt_hh, mrt_nhh, defR, filename)
                plot_I_and_R(mI, mR, filename)
                #plot_Inf(minc_hh, minc_nhh, filename)
            }
            
        }
        # legend("topright", c("total", "hh", "nhh"),
                    # fill=c("black", "red", "green"))
        # dev.off()
        col <- col+1
    }
    row <- row + 1
}
