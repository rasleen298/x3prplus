span <- 25
dataStr <- sprintf("data-new-all-%d-25", span)
datas <- file.path(dataStr, dir(dataStr, pattern="RData"))

CCFs <- plyr::ldply(datas, function(x) {
  load(x)
  cmsdist <- sapply(reslist, function(x) x$maxCMS)
  ccf <- plyr::ldply(reslist, function(res) {
    lofX <- res$bullets
    b12 <- unique(lofX$bullet)
    
    subLOFx1 <- subset(lofX, bullet==b12[1])
    subLOFx2 <- subset(lofX, bullet==b12[2]) 
    
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)

    ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max=200, na.action = na.omit)
    lag <- ccf$lag[which.max(ccf$acf)]
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    subLOFx1$y <- subLOFx1$y -  lag * incr # amount of shifting should just be lag * y.inc
    ys <- intersect(subLOFx1$y, subLOFx2$y)
    idx1 <- which(subLOFx1$y %in% ys)
    idx2 <- which(subLOFx2$y %in% ys)
    distr.dist <- mean((subLOFx1$val[idx1] - subLOFx2$val[idx2])^2, na.rm=TRUE)
    distr.sd <- sd(subLOFx1$val, na.rm=TRUE) + sd(subLOFx2$val, na.rm=TRUE)
    km <- which(res$lines$match)
    knm <- which(!res$lines$match)
    if (length(km) == 0) km <- c(length(knm)+1,0)
    if (length(knm) == 0) knm <- c(length(km)+1,0)
# browser()    
    # feature extraction
    data.frame(ccf=max(ccf$acf), lag=which.max(ccf$acf), 
               D=distr.dist, 
               sd.D = distr.sd,
               b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
               num.matches = sum(res$lines$match), 
               num.mismatches = sum(!res$lines$match), 
               non_cms = x3prplus::maxCMS(!res$lines$match),
               left_cms = max(knm[1] - km[1], 0),
               right_cms = max(km[length(km)] - knm[length(knm)],0),
               left_noncms = max(km[1] - knm[1], 0),
               right_noncms = max(knm[length(knm)]-km[length(km)],0),
               sumpeaks = sum(abs(res$lines$heights[res$lines$match]))
               )
  })
  ccf$cms <- cmsdist
  ccf$data <- x
  ccf  
})

CCFs$resID <- rep(1:210, length=nrow(CCFs))
CCFs <- CCFs[order(as.character(CCFs$b2)),]
CCFs$b2 <- factor(as.character(CCFs$b2))

splits <- strsplit(as.character(CCFs$b1), split="/")
CCFs$b1 <- sapply(splits, function(x) x[length(x)])
CCFs$b1 <- gsub(".x3p","", CCFs$b1)
  
splits <- strsplit(as.character(CCFs$b2), split="/")
CCFs$b2 <- sapply(splits, function(x) x[length(x)])
CCFs$b2 <- gsub(".x3p","", CCFs$b2)


matches <- read.csv("csvs/matches.csv", header=FALSE, stringsAsFactors = FALSE)
matches$V3 <- paste("Ukn Bullet",matches$V3)
matches$V4 <- paste("Ukn Bullet",matches$V4)
matches$V5 <- paste("Ukn Bullet",matches$V5)
matches$id <- 1:nrow(matches)

library(reshape2)
mm <- melt(matches, id.var="id")
mm <- subset(mm, value != "Ukn Bullet ")

CCFs <- merge(CCFs, mm[,c("id","value")], by.x="b1", by.y="value")
CCFs <- merge(CCFs, mm[,c("id","value")], by.x="b2", by.y="value")
CCFs$match <- CCFs$id.x == CCFs$id.y
CCFs$span <- span

flagged <- c("Br6 Bullet 2-1", "Br9 Bullet 2-4", "Ukn Bullet B-2", "Ukn Bullet Q-4")
CCFs$flagged <- CCFs$b1 %in% flagged

CCFs$b1 <- as.character(CCFs$b1)
CCFs$b2 <- as.character(CCFs$b2)
CCFs$id <- with(CCFs, paste(pmin(b1,b2), pmax(b1,b2), sep="|"))
# just checking - should all be 0
stats <- CCFs %>% group_by(id) %>% summarize(
#  tree = diff(pred),
#  forest = diff(forest),
  flagged = mean(flagged),
  sumpeaks = diff(sumpeaks),
  cms = diff(cms),
  cnms = diff(non_cms),
  matches=diff(num.matches),
  mismatches=diff(num.mismatches),
  D=diff(D)
)
summary(stats)
as.data.frame(stats)[which(stats$sumpeaks != 0),] # and of course, it's not zero. darn
# it doesn't seem to make a practical difference, though.
# XXX we should check closer into where these differences come from.

dpls <- duplicated(CCFs$id)
CCFs <- CCFs[!dpls,]

library(rpart)
library(rpart.plot)

includes <- setdiff(names(CCFs), c("id","b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))
rp1 <- rpart(match~., data=subset(CCFs, !flagged)[,includes])  # doesn't include cms at all !!!!
prp(rp1, extra = 101)
CCFs$pred <- predict(rp1, newdata=CCFs)
xtabs(~(pred>0.5)+match, data=CCFs[!CCFs$flagged,])


library(randomForest)
set.seed(20151202)
rtrees <- randomForest(factor(match)~., data=subset(CCFs, !flagged)[,includes], ntree=300)
CCFs$forest <- predict(rtrees, newdata=CCFs, type="prob")[,2]
xtabs(~(forest>0.5)+match, data=CCFs[!CCFs$flagged,])

write.csv(CCFs, file=file.path(dataStr, "bullet-stats-single.csv"), row.names=FALSE)


##################################################



# diagnostics


idx <- which(CCFs$match & CCFs$cms <= 6)
idx <- which(!CCFs$match & CCFs$ccf > .7)
idx <- which(!CCFs$match & CCFs$cms == 12)
idx <- which(CCFs$cms == 11)

for ( i in idx) {
  
  load( CCFs$data[i])
  res <- reslist[[CCFs$resID[i]]]  
  
  ch <- scan()
  print(ggplot() +
          theme_bw() + 
          geom_rect(aes(xmin=xmin, xmax=xmax, fill=factor(type)), show.legend=FALSE, ymin=-6, ymax=5, data=res$lines, alpha=0.2) +
          geom_line(aes(x = y, y = l30, colour = bullet),  data = res$bullets) +
    #      geom_hline(yintercept = res$threshold) +
    #      geom_hline(yintercept = - res$threshold) +
          scale_colour_brewer(palette="Set1", na.value=alpha("grey50", .5)) +
          theme(legend.position = c(1,1), legend.justification=c(1,1)) + 
          ylim(c(-6,6)) +
          geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(res$lines, !match)) +
          geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(res$lines, match)))
  
}


library(ggplot2)
CCFs <- read.csv(file.path(dataStr, "bullet-stats.csv"))

qplot(factor(cms), data=CCFs)
ggplot(data=CCFs) + geom_bar(aes(x=factor(cms), fill=match), position="fill")
sum(CCFs$cms >= 13)
ggplot(data=CCFs) + geom_jitter(aes(x=factor(cms), y=D, colour=match))
ggplot(data=CCFs) + geom_jitter(aes(x=factor(cms), y=ccf, colour=match)) + facet_wrap(~match)
ggplot(data=CCFs) + geom_bar(aes(x=factor(non_cms), fill=match), position="fill")
qplot(ccf, num.matches, geom="jitter", data=CCFs, colour=match, alpha=I(0.5)) + facet_wrap(~match)
qplot(cms, sumpeaks, geom="jitter", data=CCFs, colour=match, alpha=I(0.5)) + facet_wrap(~match)
qplot(cms, distr.sd, geom="jitter", data=CCFs, colour=match, alpha=I(0.5)) 

qplot(factor(cms), fill=match, data=CCFs) + facet_wrap(~b2)


qplot(ccf, num.matches, geom="jitter", data=CCFs, colour=match, alpha=0.1) + facet_wrap(~match)


# the ccf/num.matches distribution of non-matches looks like a normal distribution - 
# let's find a mean and compute (a standardized) distance from that

means <- CCFs %>% group_by(match) %>% summarize(
  meanx = mean(ccf), 
  sdx = sd(ccf),
  meany = mean(num.matches),
  sdy = sd(num.matches))

CCFs$dist <- sqrt(with(CCFs, (num.matches-means$meany[1])^2/means$sdy[1]^2 + (ccf-means$meanx[1])^2/means$sdx[1]^2 ))
subCCFs <- subset(CCFs, match==FALSE)
qplot(ccf, num.matches, geom="jitter", data=subCCFs, colour=dist) 

includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred35"))
rp2 <- rpart(match~., CCFs[,includes])  
prp(rp2)
CCFs$pred352 <- predict(rp2)
xtabs(~pred352+match, data=CCFs)

# not actually an improvement - now there's two false positives in the mix

# different idea: look at number of matches and percentage correct/incorrect ones
CCFs$total <- with(CCFs, num.matches+num.mismatches)
CCFs$percMatch <- CCFs$num.matches/CCFs$total

includes <- setdiff(names(CCFs), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred35", "bullet"))
rp3 <- rpart(match~., CCFs[,includes])
library(rpart.plot)
prp(rp3)

CCFs$pred353 <- predict(rp3)
xtabs(~pred353+match, data=CCFs)

CCFs$bullet <- gsub("-[0-9]$", "", CCFs$b2)
qplot(bullet, data=CCFs, fill=match) + coord_flip()
qplot(bullet, data=CCFs, fill=pred35 > 0.5) + coord_flip()

prp(rp1, extra = 101, fallen.leaves=TRUE)

#####################
# throw in all three evaluations into the mix:

bstats <- NULL
for (i in c(5, 10, 15, 20, 25, 30, 35, 40)) {
  dataStr <- sprintf("data-%d-25", i)
  temp <- read.csv(file.path(dataStr, "bullet-stats.csv"))
  includes <- setdiff(names(temp), c("b1", "b2", "data", "resID", "id.x", "id.y"))
  temp$diffx <- with(temp, abs(x1-x2))
  temp$perc_matches <- with(temp, num.matches/(num.matches+num.mismatches))
  rp <- rpart(match~., data=temp[,includes])
  
  prp(rp, extra = 101)
  #ch <- scan()
  temp$pred <- predict(rp)
  temp$span <- i
  temp$bullet <- NULL
  temp$crosscutdist <- NULL
  bstats <- rbind(bstats, temp)
}



xtabs(~(pred>0.5)+match+span, data=bstats)

bstats$bullet <- gsub("-[0-9]$", "", bstats$b2)
bullets <- bstats %>% group_by(bullet, span) %>% summarize(
  n = n(),
  pred = sum(pred > 0.5)
)

qplot(data=bullets, pred, reorder(bullet, pred/n),  colour=factor(span), shape = factor(span)) + 
  theme_bw()  + ylab("") + 
  scale_x_continuous("Number of correctly predicted land-to-land matches", 
                     breaks = 3*0:4, limits=c(0,12)) 

write.csv(bstats, "csvs/bullet-stats.csv", row.names=TRUE)


