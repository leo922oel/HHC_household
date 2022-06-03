require("stringr")
########### server args ###############
# args <- commandArgs(trailingOnly = TRUE)
# hhstr <- as.integer(args[1])
# path <- args[2]
# save_path <- paste0("/home/leo922oel/nas/", hhstr,"_IR")

########### laptop given path ###############
hhstr <- 45000
ori_path <- "D:/user/Desktop/"
path <- paste0(ori_path, hhstr)
save_path <- paste0(ori_path, hhstr,"_IR")

########### HHC-DT1 given path ###############
#hhstr <- 18000
#ori_path <- "D:/Leo Chien/Desktop/"
#path <- paste0(ori_path, hhstr)
#save_path <- paste0(ori_path, hhstr,"_IR")

setwd(path)
files <- list.files(pattern = ".csv")

dur_infect <- 5 # infectiousness period = 5 days
gamma_rate <- 1 / dur_infect # recovery rate per day
mask_eff <- 0.8 # relative risk of mask wearing policy

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
    abline(h=peak, lty=2, col="blue", lwd=2)

    text(45, 5, paste0("hh contact = ", ct_hh, " | nhh contact = ", ct_nhh,
            "\nhh trans = ", trans_hh, " | nhh trans = ", trans_nhh), cex = 1.5, col = "blue")
    legend("right", c("Infection", "Recovery"),
           fill=c("red", "green"), cex=1.8)
    
    dev.off()
 
}