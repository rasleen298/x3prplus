## Kernel

library(x3pr)
library(x3prplus)
library(ggplot2)

bullet <- get_bullet("~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-5.x3p")

smoothfactor <- 35

weight_ind <- seq(-1, 1, length.out = smoothfactor)
kernel_weights <- 3/4 * (1 - weight_ind^2)

bulletvals <- bullet$value
for (i in 2:length(bulletvals)) {
    if (is.na(bulletvals[i])) bulletvals[i] <- bulletvals[i - 1]
}

qplot(bullet$y, bulletvals)

smoothed <- NULL
for (i in 1:(length(bulletvals) - smoothfactor)) {
    smoothed <- c(smoothed, sum(bulletvals[i:(i + smoothfactor - 1)] * kernel_weights))
}
