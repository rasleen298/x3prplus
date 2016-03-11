library(x3pr)
library(x3prplus)
library(scatterplot3d)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(zoo)
library(gridExtra)
library(plotly)
library(pastecs)

Sys.setenv("plotly_username" = "erichare")
Sys.setenv("plotly_api_key" = "xd0oxpeept")

list_of_plots <- lapply(dir("images/Hamby252_3DX3P1of2"), function(file) {
    br111 <- get_bullet(file.path("images", "Hamby252_3DX3P1of2", file))
    br111.groove <- get_grooves(br111)
    br111.groove$plot
    fit_loess(br111, br111.groove)
})

grid.arrange(list_of_plots[[1]], list_of_plots[[2]], list_of_plots[[3]],
             list_of_plots[[4]], list_of_plots[[5]], list_of_plots[[6]],
             list_of_plots[[7]], list_of_plots[[8]], list_of_plots[[9]],
             list_of_plots[[10]], list_of_plots[[11]], list_of_plots[[12]], ncol = 3)

br111 <- get_bullet(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-1.x3p"))
br111.groove <- get_grooves(br111)
plot_3d_land(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-1.x3p"), br111, br111.groove)

br112 <- get_bullet(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-2.x3p"))
br113 <- get_bullet(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-3.x3p"))
br114 <- get_bullet(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-4.x3p"))
br115 <- get_bullet(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-5.x3p"))
br116 <- get_bullet(file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", "Br1 Bullet 1-6.x3p"))

my.fit <- fit_loess(br111, br111.groove)

path <- file.path("GitHub", "x3prproto", "images", "Hamby252_3DX3P1of2", dir("GitHub/x3prproto/images/Hamby252_3DX3P1of2")[5])
br111 <- read.x3p(path)
crosscuts <- unique(fortify_x3p(br111)$x)
crosscuts <- crosscuts[crosscuts > 50]
crosscuts <- crosscuts[crosscuts < 150]

list_of_fits <- lapply(crosscuts, function(x) {
  br111 <- get_bullet(path, x = x)
  br111.groove <- get_grooves(br111)
  br111.groove$plot
  fit_loess(br111, br111.groove)
})

lof <- lapply(list_of_fits, function(x) x$resid$data) %>% bind_rows
qplot(x, y, fill = resid, data=lof, geom="tile") + 
  scale_fill_gradient2() + theme_bw()

path <- file.path("images", "Hamby252_3DX3P1of2", dir("images/Hamby252_3DX3P1of2")[7])
br111 <- read.x3p(path)
crosscuts <- unique(fortify_x3p(br111)$x)
crosscuts <- crosscuts[crosscuts > 50]
crosscuts <- crosscuts[crosscuts < 150]

list_of_fits2 <- lapply(crosscuts, function(x) {
  br111 <- get_bullet(path, x)
  br111.groove <- get_grooves(br111)
  br111.groove$plot
  fit_loess(br111, br111.groove)
})

lof2 <- lapply(list_of_fits2, function(x) x$resid$data) %>% bind_rows
lof$bullet <- 1
lof2$bullet <- 2

LOF <- rbind(lof, lof2)
qplot(x, y, fill = resid, data=LOF, geom="tile", facets=~bullet) + 
  scale_fill_gradient2(limits=c(-5,5)) + theme_bw()

qplot(x, y, fill = resid, data=LOF, geom="tile", facets=~bullet) + 
  scale_fill_gradient(limits=c(-5,5), low = "grey5", high="grey95") + theme_bw()

qplot(x, y, fill = resid, data=LOF, geom="tile", facets=~bullet) + 
  scale_fill_gradient2(limits=c(-5,5), low = "grey5", mid = "gold4", #mid="darkgoldenrod1", 
                       high = "lightgoldenrod1") + theme_bw()


qplot(x, y, fill = resid, data=LOF, geom="tile", facets=~bullet) + 
  scale_fill_gradientn(limits=c(-5,5), 
                       colors=c("grey5", "gold4","darkgoldenrod1","lightgoldenrod1", "lemonchiffon"),
                       values = c(0,0.5, 0.75, 0.9, 1)) + theme_bw()


subLOF1 <- LOF %>% filter(bullet == 1, x <= 100)
subLOF2 <- LOF %>% filter(bullet == 2, x > 100)

subLOF <- rbind(data.frame(subLOF1), data.frame(subLOF2))

qplot(x, y, fill = resid, colour=I(NA), data=subLOF, geom="tile") + 
  scale_fill_gradientn(limits=c(-5,5), 
                       colors=c("grey5", "gold4","darkgoldenrod1","lightgoldenrod1", "lemonchiffon"),
                       values = c(0,0.5, 0.75, 0.9, 1)) +
  theme_bw()

#subLOF$y <- subLOF$y %/% .64
subLOF1$y <- subLOF1$y + 19*1.5625 # working now!!!
subLOF <- rbind(data.frame(subLOF1), data.frame(subLOF2))

qplot(x, y, fill = resid, colour=I(NA), data=subLOF, geom="tile") + 
  scale_fill_gradientn(limits=c(-5,5), 
                       colors=c("grey5", "gold4","darkgoldenrod1","lightgoldenrod1", "lemonchiffon"),
                       values = c(0,0.5, 0.75, 0.9, 1)) +
  theme_bw()

ggsave("../code/images/side-by-side-manual-match.png", width=5, height=5)

library(reshape2)
z <- acast(data=subLOF, x~y, value.var="resid")
nbcol = 256
color = rev(rainbow(256, start = 0/6, end = 2/6))
zcol = cut(z, nbcol)
persp3d(z, type="surface", col=color[zcol], aspect=c(1,1,0.2))

qplot(x, y, fill = 5*sign(resid)*sqrt(abs(resid/5)), colour=I(NA), data=subLOF, geom="tile") + 
  scale_fill_gradientn(limits=c(-5,5), 
                       colors=c("grey5", "gold4","darkgoldenrod1","lightgoldenrod1", "lemonchiffon"),
                       values = c(0,0.5, 0.75, 0.9, 1)) +
  theme_bw()



qplot(y, resid, colour=resid, data=subset(LOF, x == 99.84), facets=~bullet) + 
  scale_colour_gradient2(limits=c(-5,5)) + theme_bw()

qplot(y, resid, geom="line", data=subset(LOF, x == 99.84), group = bullet, colour= factor(bullet)) +
  scale_colour_brewer(palette="Set1") + theme_bw()

###########################
# 
fitloess <- function(x, y, span, sub = 2) {
  dat <- data.frame(x, y)
    indx <- sub *(1: (nrow(dat) %/% sub))
  subdat <- dat[indx, ]
  lwp <- with(subdat, loess(y~x,span=span))
  predict(lwp, newdata = dat)
}

LOFx <- subset(LOF, x == 100)
LOFx = LOFx %>% group_by(bullet) %>% mutate(
  l30 = fitloess(y, resid, span = 0.03),
  l40 = fitloess(y, resid, span = 0.04),
  l50 = fitloess(y, resid, span = 0.05),
  l60 = fitloess(y, resid, span = 0.06),
  l70 = fitloess(y, resid, span = 0.07),
  l80 = fitloess(y, resid, span = 0.08),
  l90 = fitloess(y, resid, span = 0.09),
  l100 = fitloess(y, resid, span = 0.1),
  l110 = fitloess(y, resid, span = 0.11)
)
  

ggplot(aes(x=y, y=resid, group=bullet), data=LOFx) +
  geom_line(colour="grey60") +
  geom_line(aes(y=l30), colour="black", size=0.5) + 
  geom_line(aes(y=l40), colour="black", size=0.5) + 
  geom_line(aes(y=l50), colour="black", size=0.5) + 
  geom_line(aes(y=l60), colour="black", size=0.5) + 
  geom_line(aes(y=l70), colour="black", size=0.5) + 
  geom_line(aes(y=l80), colour="black", size=0.5) + 
  geom_line(aes(y=l90), colour="black", size=0.5) + 
  geom_line(aes(y=l100), colour="black", size=0.5) + 
  geom_line(aes(y=l110), colour="black", size=0.5) + 
  scale_color_brewer(palette="Set1") +
  facet_grid(bullet~.) + 
  theme_bw()

# a span of 0.03 looks decent

ggplot(aes(x=y, y=resid, colour=factor(bullet), group=bullet), data=LOFx) +
#  geom_point() +
  geom_line(aes(y=l50), size=0.5) + 
  scale_color_brewer(palette="Set1") +
#  facet_grid(bullet~.) + 
  theme_bw()

subLOFx1 <- subset(LOFx, bullet==1)
subLOFx2 <- subset(LOFx, bullet==2)
subLOFx1$y <- subLOFx1$y + 19*1.5625 # working now!!!
subLOFx <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))

ggplot(aes(x=y, y=resid, colour=factor(bullet), group=bullet), data=subLOFx) +
  #  geom_point() +
  geom_line(aes(y=l50), size=0.5) + 
  scale_color_brewer(palette="Set1") +
  #  facet_grid(bullet~.) + 
  theme_bw()

# create a signature by using a threshold value - 
subLOFx$r05 <- sign(subLOFx$l30) * as.numeric(abs(subLOFx$l30) > .75)
qplot(data=subLOFx, x=y, y=l30, colour=factor(r05), geom="line", group=bullet) + facet_grid(bullet~.) +
  geom_point(aes(y=r05))

qplot(data=subLOFx, x=y, y=bullet, fill=factor(r05), geom="tile") + ylim(c(0,3))

