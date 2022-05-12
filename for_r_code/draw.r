require("stringr")
args <- commandArgs(trailingOnly = TRUE)
hhstr <- as.integer(args[1])
# hhstr <- 3000
path <- args[2]
# path <- "D:/Dataset/household_data/3000_nmask"
setwd(path)
files <- list.files(pattern = ".csv")

dur_infect <- 5 # infectiousness period = 5 days
gamma_rate <- 1 / dur_infect # recovery rate per day
mask_eff <- 0.8 # relative risk of mask wearing policy

save_path <- paste0("/home/leo922oel/nas/", hhstr,"_new")
if (!dir.exists(save_path)) dir.create(save_path)

for (i in 1:length(files)) {
# for (i in 1:1) {
    a <- str_split(files[i], pattern = "_", simplify = T)
    ct_all <- as.numeric(a[2])
    ct_hh <- as.numeric(a[3])
    trans_hh <- as.numeric(a[4])
    nhh_ratio <- as.numeric(a[5])

    ct_nhh <- ct_all - ct_hh
    trans_nhh <- trans_hh * nhh_ratio
    defR_nhh <- ct_nhh * trans_nhh * mask_eff * dur_infect
    defR_hh <- ct_hh * trans_hh * dur_infect
    defR <- defR_nhh + defR_hh/(1+defR_hh)

    setwd(path)
    file <- read.csv(files[i], header = T, sep=",")
    setwd(save_path)
    pngname <- str_replace(files[i], pattern = "csv", replacement = "png")
    png(pngname, width = 840, height = 600, type="cairo")

    mean.hh <- file$mean_hh[-1]
    mean.nhh <- file$mean_nhh[-1]
    mean.total <- mean.hh + mean.nhh

    plot(mean.total, main = paste0("avg Rt of size ", hhstr), xlab = "Time(Day)", ylab = "Value of Rt", ylim = c(0, 5), type = "n", las = 2)
    lines(mean.total, col = "black", lwd = 2)
    lines(mean.hh, col = "red", lwd = 2)
    lines(mean.nhh, col = "green", lwd = 2)

    abline(h=defR, lty=2, col="black", lwd = 2)
    abline(h=defR_hh, lty=2, col="red", lwd = 2)
    abline(h=defR_nhh, lty=2, col="green", lwd = 2)

    text(75, 3, paste0("hh contact = ", ct_hh, " | nhh contact = ", ct_nhh,
            "\nhh trans = ", trans_hh, " | nhh trans = ", trans_nhh), cex = 1.5, col = "blue")
    legend("topright", c("total", "hh", "nhh"),
           fill=c("black", "red", "green"), cex=1.8)
    
    dev.off()
 
}



