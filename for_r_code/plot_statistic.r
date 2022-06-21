require("stringr")
type <- 7
### use binary code for (plot_IR, plot_Rt, plot_box)
### ex. 7 = 111 = plot_IR, plot_Rt, plot_box
### ex. 4 = 100 = plot_IR

########### server args ###############
# args <- commandArgs(trailingOnly = TRUE)
# hhstr <- as.integer(args[1])
# ori_path <- args[2]

########### laptop given path ###############
hhstr <- 45000
ori_path <- "D:/user/Desktop/"

########### HHC-DT1 given path ###############
# hhstr <- 18000
# ori_path <- "D:/Leo Chien/Desktop/"

dur_infect <- 5 # infectiousness period = 5 days
mask_eff <- 0.8 # relative risk of mask wearing policy

plot_IR <- function(hhstr, ori_path) {
    path <- paste0(ori_path, hhstr)
    setwd(path)
    files <- list.files(pattern = ".csv")

    save_path <- paste0(ori_path, hhstr, "_IR")
    if (!dir.exists(save_path)) dir.create(save_path)

    for (i in 1:length(files)) {
        a <- str_split(files[i], pattern = "_", simplify = T)
        ct_all <- as.numeric(a[1])
        ct_hh <- as.numeric(a[2])
        trans_hh <- as.numeric(a[3])
        nhh_ratio <- as.numeric(a[4])

        ct_nhh <- ct_all - ct_hh
        trans_nhh <- trans_hh * nhh_ratio

        setwd(path)
        file <- read.csv(files[i], header = T, sep=",")
        setwd(save_path)
        pngname <- str_replace(files[i], pattern = "csv", replacement = "png")
        png(pngname, width = 840, height = 600, type="cairo")

        mean.I <- file$mean_I / 90000
        mean.R <- file$mean_R / 90000
        peak <- max(mean.I)

        plot(mean.I, main = paste0("avg I/R of size ", hhstr), xlab = "Time(Day)", ylab = "Population", ylim = c(0, 1), type = "n", las = 1)
        lines(mean.I, col = "red", lwd = 2)
        lines(mean.R, col = "green", lwd = 2)
        abline(h = peak, lty = 2, col = "blue", lwd = 2)

        text(45, 5, paste0("hh contact = ", ct_hh, " | nhh contact = ", ct_nhh,
                "\nhh trans = ", trans_hh, " | nhh trans = ", trans_nhh), cex = 1.5, col = "blue")
        legend("right", c("Infection", "Recovery"),
               fill = c("red", "green"), cex = 1.8)

        dev.off()
    }
}

plot_Rt <- function(hhstr, ori_path) {
    path <- paste0(ori_path, hhstr)
    setwd(path)
    files <- list.files(pattern = ".csv")

    save_path <- paste0(ori_path, hhstr, "_Rt")
    if (!dir.exists(save_path)) dir.create(save_path)

    for (i in 1:length(files)) {
        a <- str_split(files[i], pattern = "_", simplify = T)
        ct_all <- as.numeric(a[1])
        ct_hh <- as.numeric(a[2])
        trans_hh <- as.numeric(a[3])
        nhh_ratio <- as.numeric(a[4])

        ct_nhh <- ct_all - ct_hh
        trans_nhh <- trans_hh * nhh_ratio
        defR_nhh <- ct_nhh * trans_nhh * mask_eff * dur_infect
        defR_hh <- ct_hh * trans_hh * dur_infect
        defR_hh.2 <- defR_hh / (1 + defR_hh)
        defR <- defR_nhh + defR_hh / (1 + defR_hh)

        setwd(path)
        file <- read.csv(files[i], header = T, sep = ",")
        setwd(save_path)
        pngname <- str_replace(files[i], pattern = "csv", replacement = "png")
        png(pngname, width = 840, height = 600, type = "cairo", res = 84)

        mean.hh <- file$mean_hh[-1]
        mean.nhh <- file$mean_nhh[-1]
        mean.total <- mean.hh + mean.nhh

        plot(mean.total, main = paste0("avg Rt of size ", hhstr), xlab = "Time(Day)", ylab = "Value of Rt", ylim = c(0, 8), type = "n", las = 1, cex = 1.5, xlim = c(0, 40))
        lines(mean.total, col = "black", lwd = 2)
        lines(mean.hh, col = "red", lwd = 2)
        lines(mean.nhh, col = "green", lwd = 2)

        abline(h = defR, lty = 2, col = "black", lwd = 2)
        abline(h = defR_hh.2, lty = 2, col = "red", lwd = 2)
        abline(h = defR_nhh, lty = 2, col = "green", lwd = 2)

        text(20, 3, paste0("hh contact = ", ct_hh, " | nhh contact = ", ct_nhh,
                "\nhh trans = ", trans_hh, " | nhh trans = ", trans_nhh), cex = 1.3, col = "blue")
        legend("topright", c("total", "hh", "nhh"),
               fill = c("black", "red", "green"), cex = 1.8)
    
        dev.off()
    }
}

sizes <- c(45000, 30000, 18000, 10000)
plot_box <- function(ori_path) {

    save_path <- paste0(ori_path, "box", length(sizes))
    if (!dir.exists(save_path)) dir.create(save_path)

    class <- NA
    duration <- NA
    idx.peak <- NA
    Rt <- NA
    for (size in sizes) {
        path <- paste0(ori_path, size)
        setwd(path)
        files <- list.files(pattern = ".csv")
        class <- c(class, rep(as.integer(90000 / size), length(files)))

        dura.temp <- rep(0, length(files))
        peak.temp <- rep(0, length(files))
        Rt.temp <- rep(0, length(files))

        for (i in 1:length(files)) {
            file <- read.csv(files[i], header = T, sep = ",")
            mean.I <- file$mean_I
            S <- sum(mean.I)
            if (S < 90000 * 0.3) {
                class[i] <- NA
                dura.temp[i] <- NA
                peak.temp[i] <- NA
                Rt.temp[i] <- NA
                next
            }
            prep <- 0
            start <- 0
            end <- 0
            for (d in 1:length(mean.I)) {
                p <- sum(mean.I[1:d])
                if (start == 0 && prep / S < 0.025 && 0.025 <= p / S) start <- d
                if (end == 0 && mean.I[d] <= mean.I[start] && 0.025 <= (1 - prep / S) && (1 - p / S) < 0.025) end <- d
                prep <- p
            }
            plot(mean.I)
            abline(v = start)
            abline(v = end)

            if (end != 0) {
                dura.temp[i] <- end - start
                peak.temp[i] <- which.max(mean.I)
                mean.hh <- file$mean_hh[-1]
                mean.nhh <- file$mean_nhh[-1]
                mean.total <- mean.hh + mean.nhh
                Rt.temp[i] <- mean(mean.total[5:11])
            } else {
                class[i] <- NA
                dura.temp[i] <- NA
                peak.temp[i] <- NA
                Rt.temp[i] <- NA
            }
        }

        duration <- c(duration, dura.temp)
        idx.peak <- c(idx.peak, peak.temp)
        Rt <- c(Rt, Rt.temp)
    }
    class <- class[-1]
    duration <- duration[-1]
    idx.peak <- idx.peak[-1]
    Rt <- Rt[-1]

    setwd(save_path)
    pngname <- "box_peak.png"
    png(pngname, width = 840, height = 600, type="cairo", res=84)
    plot(idx.peak~as.factor(class), main = "box plot of transmission peak ", #col = c("red", "green", "blue"), 
            xlab = "household size", ylab = "Time (Day)", ylim = c(0, 60), las = 1, cex.main=2, cex.lab=1.3, cex.axis=1, na.rm=T)
    # stripchart(idx.peak~as.factor(class), main = "box plot of transmission peak ", xlab = "household size", ylab = "Time (Day)", ylim = c(0, 60), las = 1)
            # abline(h=defR, lty=2, col="black", lwd = 2)
            # abline(h=defR_hh.2, lty=2, col="red", lwd = 2)
            # abline(h=defR_nhh, lty=2, col="green", lwd = 2)

            # text(45, 5, paste0("hh contact = ", ct_hh, " | nhh contact = ", ct_nhh,
                    # "\nhh trans = ", trans_hh, " | nhh trans = ", trans_nhh), cex = 1.5, col = "blue")
            # legend("topright", c("total", "hh", "nhh"),
                #    fill=c("black", "red", "green"), cex=1.8)
    dev.off()

    pngname <- "box_Rt.png"
    png(pngname, width = 840, height = 600, type="cairo", res=84)
    plot(Rt~as.factor(class), main = "box plot of Rt", #col = c("red", "green", "blue"), 
            xlab = "household size", ylab = "Rt", ylim = c(0, 6), las = 1, cex.main=2, cex.lab=1.3, cex.axis=1, na.rm=T)
    # jitter_class <- jitter(as.factor(class))
    # points(Rt~jitter_class)
    dev.off()

    # pngname <- "box_outbreak.png"
    # png(pngname, width = 840, height = 600, type="cairo")
    # plot(outbreak~as.factor(class), main = "box plot of outbeak", #col = c("red", "green", "blue"), 
            # xlab = "household size", ylab = "Time (Day)", ylim = c(0, 40), las = 1, cex.main=2, na.rm=T)
    # jitter_class <- jitter(as.factor(class))
    # points(Rt~jitter_class)
    # dev.off()

    pngname <- "box_duration.png"
    png(pngname, width = 840, height = 600, type="cairo", res=84)
    plot(duration~as.factor(class), main = "box plot of duration", #col = c("red", "green", "blue"), 
            xlab = "household size", ylab = "Duration (Days)", ylim = c(0, 60), las = 1, cex.main=2, cex.lab=1.3, cex.axis=1, na.rm=T)
    # jitter_class <- jitter(as.factor(class))
    # points(Rt~jitter_class)
    dev.off()
}

box <- type %% 2
Rt <- (type %/% 2) %% 2
IR <- ((type %/% 2) %/% 2) %% 2
if (IR) plot_IR(hhstr, ori_path)
if (Rt) plot_Rt(hhstr, ori_path)
if (box) plot_box(ori_path)