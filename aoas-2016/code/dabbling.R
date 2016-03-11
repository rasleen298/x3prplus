# just trying to piece these bullet pieces together ... 
library(x3pr)
library(ggplot2)
library(dplyr)

getCircle <- function(x, y) {
  nas <- which(is.na(y))
  x <- x[-nas]
  y <- y[-nas]
  
  mx <- mean(x)
  my <- mean(y)
  
  x <- x - mx
  y <- y - my
  
  c1 <- sum(x^3) + sum(x*y^2)
  c2 <- sum(x^2*y) + sum(y^3)
  
  syy <- sum(y^2)
  sxy <- sum(x*y)
  sxx <- sum(x^2)
  
  D <- sxx*syy - sxy^2
  
  a <- (c1*syy - c2*sxy)/(2*D)
  b <- (c2*sxx - c1*sxy)/(2*D)
  r <- mean((x-a)^2 + (y-b)^2)
  cbind(x0=a+mx, y0=b+my, radius=sqrt(r))
}

fortify <- function(x3d) {
  info <- x3d[[1]]
  
  df <- data.frame(expand.grid(x=1:info$num.pts.line, y=1:info$num.lines), 
             value=as.vector(t(x3d[[2]])))
  df$x <- (df$x-1) * info$x.inc
  df$y <- (df$y-1) * info$y.inc
  df
}

flatten <- function(br11) {
  dbr11 <- fortify(br11)

  require(dplyr)
  
  dbr11 <- dbr11 %>% group_by(x) %>% do(
    data.frame(., getCircle(.$y, .$value))
  )
  dbr11$theta <- with(dbr11, acos((y-x0)/radius)/pi*180)

  dbr11$xpred <- with(dbr11, x0+radius*cos(theta/180*pi))
  dbr11$ypred <- with(dbr11, y0+radius*sin(theta/180*pi))
  
#  qplot(y, value, data=subset(dbr11, x <= 10), colour=factor(x)) + 
#    geom_point(aes(x=xpred, y=ypred), colour="black", size=.25)

  # if the scanner is not just moving horizontally, 
  # we might want to switch to orthogonal distance instead of residuals  
  with(dbr11, matrix(value-ypred, ncol=br11[[1]]$num.pts.line))
}



br11 <- read.x3p("Br1 Bullet 1-1.x3p")
dbr11 <- fortify(br11)

# for the same x, we have circle segments
qplot(y, value, data=subset(dbr11, x <= 5*1.65), colour=factor(x))  

# raw image
plot3D.x3p.file(read.x3p("Br1 Bullet 1-1.x3p"), plot.type="surface")
plot3D.x3p.file(read.x3p("Br1 Bullet 1-3.x3p"), plot.type="surface")

# processed image
pbr11 <- br11
pbr11[[2]] <- flatten(br11)
plot3D.x3p.file(pbr11, plot.type="surface")

br13 <- read.x3p("Br1 Bullet 1-3.x3p")
# raw image
plot3D.x3p.file(br13, plot.type="surface")

# processed image
pbr11 <- br13
pbr11[[2]] <- flatten(br13)
plot3D.x3p.file(pbr11, plot.type="surface")


br13 <- read.x3p("Br1 Bullet 1-3.x3p")
br14 <- read.x3p("Br1 Bullet 1-4.x3p")
br15 <- read.x3p("Br1 Bullet 1-5.x3p")
br16 <- read.x3p("Br1 Bullet 1-6.x3p")


###########
flat <- function(fname) {
br11 <- read.x3p(fname)
fbr11 <- br11
fbr11[[2]] <- flatten(br11)
fortify(fbr11)
}


qplot(y, value, data=subset(dbr11), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 1-1", size=15)

qplot(y, value, data=subset(flat("Br1 Bullet 1-1.x3p")), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 1-1", size=15)


qplot(y, value, data=subset(flat("Br1 Bullet 2-1.x3p"), x < 10), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 2-1", size=15)
qplot(y, value, data=subset(flat("Br1 Bullet 2-2.x3p"), x < 10), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 2-2", size=15)
qplot(y, value, data=subset(flat("Br1 Bullet 2-3.x3p"), x < 10), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 2-3", size=15)
qplot(y, value, data=subset(flat("Br1 Bullet 2-4.x3p"), x < 10), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 2-4", size=15)
qplot(y, value, data=subset(flat("Br1 Bullet 2-5.x3p"), x < 10), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 2-5", size=15)
qplot(y, value, data=subset(flat("Br1 Bullet 2-6.x3p"), x < 10), geom="line", size=I(0.5), group=x) + 
  annotate("text", colour="grey80", x=1250, y=20, label="Br1 2-6", size=15)
