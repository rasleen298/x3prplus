library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(gridExtra)

datadir <- "app/images/Hamby252_3DX3P1of2/"
images <- file.path(datadir, dir(datadir))



processBullets <- function(paths, x = 100) {
  br111 <- read.x3p(paths[1])
  crosscuts <- unique(fortify_x3p(br111)$x)
  crosscuts <- crosscuts[crosscuts >= min(x)]
  crosscuts <- crosscuts[crosscuts <= max(x)]
  
  LOF <- lapply(paths, function(path) {
    list_of_fits <- lapply(crosscuts, function(x) {
      br111 <- get_bullet(path, x = x)
      br111.groove <- get_grooves(br111)
      br111.groove$plot
      fit_loess(br111, br111.groove)
    })
    lof <- lapply(list_of_fits, function(x) x$resid$data) %>% bind_rows
    lof$path <- path
    path <- gsub("app.*//", "", as.character(path))
    lof$bullet <- gsub(".x3p", "", path)
    
    lof
  })
  LOF %>% bind_rows()
}

smoothloess <- function(x, y, span, sub = 2) {
  dat <- data.frame(x, y)
  indx <- sub *(1: (nrow(dat) %/% sub))
  subdat <- dat[indx, ]
  lwp <- with(subdat, loess(y~x,span=span))
  predict(lwp, newdata = dat)
}


lof <- processBullets(paths = images[c(5,7)], x = 100)

subLOFx1 <- subset(lof, bullet=="Br1 Bullet 1-5")
subLOFx2 <- subset(lof, bullet=="Br1 Bullet 2-1")
subLOFx1$y <- subLOFx1$y + 23*1.5625 # working now!!!
lof <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))



# couple of side by side comparisons
qplot(bullet, y, fill = resid, data=lof, geom="tile") + 
  scale_fill_gradient2() + theme_bw()

qplot(y, resid, group=bullet, colour=bullet, data=lof, geom="line") + 
  scale_colour_brewer(palette="Set1") + theme_bw() +
  theme(legend.position="bottom")


lof <- lof %>% group_by(bullet) %>% mutate(
  l30 = smoothloess(y, resid, span = 0.03)
)
lof <- subset(lof, !is.na(l30))

lof$r05 <- 0.75* sign(lof$l30) * as.numeric(abs(lof$l30) > .75)
lof$type <- factor(lof$r05)
levels(lof$type) <- c("groove", NA, "peak")

qplot(data=lof, x=y, y=l30, colour=type, geom="line", group=bullet) + 
  facet_grid(bullet~.) +
  geom_point(aes(y=r05))

qplot(data=lof, x=y, y=bullet, fill=type, geom="tile") +
  scale_fill_brewer(palette="Set1") + 
  theme_bw() +
  ylab("") +
  theme(legend.position="bottom")

lof <- lof %>% group_by(bullet) %>% mutate(
  diff = c(0, diff(r05)),
  mins = list(c(1, which(diff != 0))),
  maxs = list(c(which(diff != 0)-1, n())),
  n = n(),
  minMarks = list(mins[[1]][!is.na(type[mins[[1]]])]),
  maxMarks = list(maxs[[1]][!is.na(type[maxs[[1]]])])
)

marks <- lof %>% group_by(bullet) %>% summarise(
  mins = mins[1],
  maxs = maxs[1],
  n = n[1],
  minMarks = minMarks[1],
  maxMarks = maxMarks[1]
)

mins <- marks$minMarks[1][[1]]
maxs <- marks$maxMarks[1][[1]]
for (i in 2:nrow(marks)) {
  mins <- c(mins, marks$n[i-1] + marks$minMarks[i][[1]])
  maxs <- c(maxs, marks$n[i-1] + marks$maxMarks[i][[1]])
}

# start and end of the marks
qplot(data=lof, x=y, y=l30, colour=type, geom="line", group=bullet) + 
  facet_grid(bullet~.) +
  geom_point(aes(y=r05), data=lof[mins,]) +
  geom_point(aes(y=r05), data=lof[maxs,])

matches <- lof[mins, c("y", "bullet", "type")]
matches$yend <- unlist(lof[maxs, c("y")])

qplot(y = bullet, yend=bullet, x=y, xend = yend, data=matches, 
      geom="segment", colour=type, size=I(10))

##################
# different attempt


matches <- lof %>% group_by(y) %>% summarise(
  potential = (length(unique(type)) == 1),
  allnas = sum(is.na(type))/n(),
  type1 = na.omit(type)[1],
  type = paste(type, sep="|", collapse="|"),
  n = n()
)




qplot(x = y, y = potential, colour=type1, data=matches)

qplot(data=lof, x=y, y=bullet, fill=type, geom="tile") +
  geom_point(aes(x = y, fill=type1), y=1.5, data=subset(matches, potential & allnas < 1)) +
#  geom_point(aes(x = y), y=1.5, pch = "x", data=subset(matches, !potential)) +
  scale_fill_brewer(palette="Set1") + 
  theme_bw() +
  ylab("") +
  theme(legend.position="bottom")


# lines are defined by white space in between 
matches$id <- cumsum(matches$allnas == 1) + 1
matches$lineid <- as.numeric(matches$allnas != 1) * matches$id

isMatch <- function(id, type) {
  if (id[1] == 0) return(FALSE)
#  browser()
  types <- strsplit(type, split = "|", fixed=TRUE) 
  t1 <- sapply(types, function(x) x[1])
  t2 <- sapply(types, function(x) x[2])
  if (all(t1 == "NA")) return(FALSE)
  if (all(is.na(t2))) return(FALSE)
  if (all(t2 == "NA")) return(FALSE)
  
  peak <- length(grep("peak", c(t1, t2))) > 0
  groove <- length(grep("groove", c(t1, t2))) > 0
  if (peak & groove) return(FALSE)

  return(TRUE)
}

lines <- matches %>% group_by(lineid) %>% summarise(
  meany = mean(y, na.rm=T),
  miny = min(y, na.rm=T),
  maxy = max(y, na.rm=T) + 1.5625,
  match = isMatch(lineid, type),
  type = type1[1]
)


qplot(data=lof, x=y, y=bullet, fill=type, geom="tile") +
  geom_text(aes(x = meany, label=lineid), colour="black", y=1, data=subset(lines, match)) +
  scale_fill_brewer(palette="Set1") + 
  theme_bw() +
  ylab("") +
  theme(legend.position="bottom") 
  
ggplot() +
  geom_rect(aes(xmin = miny, xmax = maxy), ymin = 0.25, ymax=2.75, fill="grey80", data = subset(lines, lineid != 0)) +
  geom_tile(data=lof, aes(x=y, y=bullet, fill=type)) + 
  scale_y_discrete(expand = c(0.2,0)) +
  scale_fill_brewer(palette="Set1") + 
  theme_bw() +
  ylab("") +
  theme(legend.position="bottom") +
  geom_text(aes(x = meany), y= .4, label= "x", data = subset(lines, !match & lineid !=0)) +
  geom_text(aes(x = meany), y= .4, label= "o", data = subset(lines, match & lineid !=0))


thresholds <- seq(0.3, 1.5, by = 0.05)
CMS <- thresholds %>% lapply(function(threshold) {
 lines <- striation_identify(lofX, threshold = threshold)
 cms <- CMS(lines$match)
 data.frame(threshold=threshold, maxCMS = as.numeric(rev(names(cms)))[1])
}) %>% bind_rows()

qplot(threshold, maxCMS, data=CMS) + theme_bw() +
  ylab("Number of maximal CMS") +
  ylim(c(0,15))

###############
# how do things look like between non-matches?

# biggest problem right now: horizontal alignment. 
list_of_matches <- lapply(19:24, function(i) {
  cat(i)
  lof <- processBullets(paths = images[c(15,i)], x = 100)
  lof <- bulletSmooth(lof)
  
  subLOFx1 <- subset(lof, bullet==unique(lof$bullet)[1])
  subLOFx2 <- subset(lof, bullet==unique(lof$bullet)[2]) 
  
  # ccf assumes that both time series start at the same y
  subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
  subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
  ccf <- ccf(subLOFx1$l30, subLOFx2$l30, plot = FALSE, lag.max=150, na.action = na.omit)
  lag <- ccf$lag[which.max(ccf$acf)]
  
  subLOFx1$y <- subLOFx1$y -  lag * 1.5625 # amount of shifting should just be lag * y.inc
  lofX <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))

#  browser()  
  CMS <- thresholds %>% lapply(function(threshold) {
    lines <- striation_identify(lofX, threshold = threshold)
    cms <- CMS(lines$match)
    data.frame(threshold=threshold, maxCMS = as.numeric(rev(names(cms)))[1])
  }) %>% bind_rows()
  
  threshold = CMS$threshold[which.max(CMS$maxCMS)]
  lines <- striation_identify(lofX, threshold = threshold)
  title <- gsub("app.*//","", images[i])
  title <- gsub(".x3p","", title)
  ggplot() +
    theme_bw() + 
    geom_rect(aes(xmin=miny, xmax=maxy), ymin=-6, ymax=5, fill="grey90", data=subset(lines, lineid!=0)) +
    geom_line(aes(x = y, y = l30, colour = bullet),  data = lofX) +
    geom_hline(yintercept = threshold) +
    geom_hline(yintercept = - threshold) +
    scale_colour_brewer(palette="Set1") +
    theme(legend.position = "none") + 
    ylim(c(-6,6)) +
    geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match & lineid !=0)) +
    geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match & lineid !=0)) +
    ggtitle(title)
})

grid.arrange(list_of_matches[[1]], list_of_matches[[2]], list_of_matches[[3]], 
             list_of_matches[[4]], list_of_matches[[5]], list_of_matches[[6]],
             ncol = 2)
