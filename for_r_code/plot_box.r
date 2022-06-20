require("stringr")
########### server args ###############
# args <- commandArgs(trailingOnly = TRUE)
# hhstr <- as.integer(args[1])
# ori_path <- args[2]

########### laptop given path ###############
# hhstr <- 18000
ori_path <- "D:/user/Desktop/"

########### HHC-DT1 given path ###############
# hhstr <- 18000
# ori_path <- "D:/Leo Chien/Desktop/"

dur_infect <- 5 # infectiousness period = 5 days
gamma_rate <- 1 / dur_infect # recovery rate per day
mask_eff <- 0.8 # relative risk of mask wearing policy

sizes <- c(45000, 30000, 18000, 10000)
# sizes <- c(45000, 30000)
save_path <- paste0(ori_path,"box", length(sizes))
if (!dir.exists(save_path)) dir.create(save_path)

class <- NA
# outbreak <- NA
duration <- NA
idx.peak <- NA
Rt <- NA
for (size in sizes) {
    path <- paste0(ori_path, size)
    setwd(path)
    files <- list.files(pattern = ".csv")
    class <- c(class, rep(as.integer(90000/size), length(files)))
    # outbreak.temp <- rep(0, length(files))
    dura.temp <- rep(0, length(files))
    peak.temp <- rep(0, length(files))
    Rt.temp <- rep(0, length(files))

    for (i in 1:length(files)) {
        file <- read.csv(files[i], header = T, sep=",")
        mean.I <- file$mean_I
        S <- sum(mean.I)
        if (S < 90000*0.3) {
            class[i] <- NA
            # outbreak.temp[i] <- NA
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
            if (start == 0 && prep/S < 0.025 && 0.025 <= p/S) start <- d
            if (end == 0 && mean.I[d] <= mean.I[start] && 0.025 <= (1 - prep/S) && (1 - p/S) < 0.025) end <- d
            prep <- p
        }
        plot(mean.I)
        abline(v=start)
        abline(v=end)
        # outbreak.temp[i] <- start
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

    # outbreak <- c(outbreak, outbreak.temp)
    duration <- c(duration, dura.temp)
    idx.peak <- c(idx.peak, peak.temp)
    Rt <- c(Rt, Rt.temp)
}
class <- class[-1]
# outbreak <- outbreak[-1]
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
