require("stringr")
########### server args ###############
# args <- commandArgs(trailingOnly = TRUE)
# hhstr <- as.integer(args[1])
# path <- args[2]
# save_path <- paste0("/home/leo922oel/nas/", hhstr,"_box")

########### laptop given path ###############
# hhstr <- 18000
# path <- paste0("D:/user/Desktop/", hhstr)
# save_path <- paste0("D:/user/Desktop/", hhstr,"_box")

########### HHC-DT1 given path ###############
# hhstr <- 18000
ori_path <- "D:/Leo Chien/Desktop/"

dur_infect <- 5 # infectiousness period = 5 days
gamma_rate <- 1 / dur_infect # recovery rate per day
mask_eff <- 0.8 # relative risk of mask wearing policy

save_path <- paste0(ori_path,"box")
if (!dir.exists(save_path)) dir.create(save_path)

class <- NA
outbreak <- NA
duration <- NA
idx.peak <- NA
Rt <- NA
sizes <- c(45000, 30000, 18000)
for (size in sizes) {
    path <- paste0(ori_path, size)
    setwd(path)
    files <- list.files(pattern = ".csv")
    class <- c(class, rep(as.integer(90000/45000), length(files)))
    outbreak.temp <- rep(0, length(files))
    dura.temp <- rep(0, length(files))
    peak.temp <- rep(0, length(files))
    Rt.temp <- rep(0, length(files))

    for (i in 1:length(files)) {
        file <- read.csv(files[i], header = T, sep=",")
        mean.I <- file$mean_I
        start <- 0
        end <- 0
        for (d in 1:length(mean.I)) {
            t <- (mean.I[d+1] - mean.I[d]) / 
            if ( <= mean.I[d+1]) start <- mean.I[d]
            if (mean.I[d] >= 2*mean.I[d+1]) end <- mean.I[d+1]
        }
        outbreak.temp[i] <- start
        dura.temp[i] <- end - start
        peak.temp[i] <- which.max(mean.I)
        mean.hh <- file$mean_hh[-1]
        mean.nhh <- file$mean_nhh[-1]
        mean.total <- mean.hh + mean.nhh
        Rt.temp[i] <- mean(mean.total[5:11])
    }

    outbreak <- c(outbreak, outbreak.temp)
    duration <- c(duration, dura.temp)
    idx.peak <- c(idx.peak, peak.temp)
    Rt <- c(Rt, Rt.temp)
}
setwd(save_path)
pngname <- "box_I.png"
png(pngname, width = 840, height = 600, type="cairo")
boxplot(idx.peak~class, main = "box plot of transmission peak ", xlab = "household size", ylab = "Time (Day)", ylim = c(0, 60), las = 1)
        # abline(h=defR, lty=2, col="black", lwd = 2)
        # abline(h=defR_hh.2, lty=2, col="red", lwd = 2)
        # abline(h=defR_nhh, lty=2, col="green", lwd = 2)

        # text(45, 5, paste0("hh contact = ", ct_hh, " | nhh contact = ", ct_nhh,
                # "\nhh trans = ", trans_hh, " | nhh trans = ", trans_nhh), cex = 1.5, col = "blue")
        # legend("topright", c("total", "hh", "nhh"),
            #    fill=c("black", "red", "green"), cex=1.8)
dev.off()

pngname <- "box_Rt.png"
png(pngname, width = 840, height = 600, type="cairo")
boxplot(Rt~class, main = "box plot of Rt", xlab = "household size", ylab = "Rt", ylim = c(0, 8), las = 1)