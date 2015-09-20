#' Convert a list of x3d file into a data frame
#' 
#' x3d format consists of a list with header info and a 2d matrix of scan depths. 
#' fortify_x3p turn the matrix into a variable within a data frame, using the parameters of the header as necessary.
#' @param x3d a file in x3d format as return by function read.x3d
#' @return data frame with variables x, y, and value
#' @export
fortify_x3p <- function(x3d) {
  require(x3pr)
  info <- x3d[[1]]
  
  df <- data.frame(expand.grid(x=1:info$num.pts.line, y=1:info$num.lines), 
                   value=as.vector(t(x3d[[2]])))
  df$x <- (df$x-1) / info$x.inc
  df$y <- (df$y-1) / info$y.inc
  df
}

#' Sample every X element of a data frame
#' 
#' Sample every X element of a data frame in x and y direction
#' @param dframe data frame with x and y variable
#' @param byxy (vector) of numeric value indicating the sapling resolution. If a single number, the same resolution is used for x and y.
#' @return subset of the input variable
#' @export
sample.x3d <- function(dframe, byxy=c(2,2)) {
  # use fortified data set
  # use only every byxy sample in x and y direction 
  if(length(byxy)==1) byxy <- rep(byxy, length=2)
  xn <- sort(as.numeric(unique(dframe$x)))
  yn <- sort(as.numeric(unique(dframe$y)))
  
  xseq <- xn[seq(1, length(xn), by=byxy[1])] 
  yseq <- yn[seq(1, length(yn), by=byxy[2])] 
  subset(dframe, (x %in% xseq) & (y %in% yseq))
}

#' Estimate center and radius
#' 
#' Assuming the variables x and y are describing points located on a circle, the function uses a likelihood approach to estimate center and radius of the circle.
#' @param x numeric vector of values
#' @param y numeric vector of values
#' @return three dimensional vector of the circle center (x0, y0) and the radius
#' @export
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

#' Estimate predictions and residuals for a circle fit of x and y
#' 
#' estimate a circle, find predictive values and resiudals. depending on specification, vertical (regular) residuals or orthogonal residuals are computed.
#' @param x vector of numeric values
#' @param y vector of numeric values
#' @param resid.method character, one of "response" or "ortho"(gonal)
#' @return data frame with predictions and residuals
#' @export
predCircle <- function(x, y, resid.method="response") {
  pars <- data.frame(getCircle(x, y))
  theta <- acos((x-pars$x0)/pars$radius)/pi*180
  ypred <- pars$y0+pars$radius*sin(theta/180*pi)
  
  dframe <- data.frame(ciry=ypred)
  if ("response" %in% resid.method) {
    dframe$resid = y-ypred
  }  
  if ("ortho" %in% resid.method) {
    dframe$oresid = sqrt( (y-pars$y0)^2 + (x - pars$x0)^2) - pars$radius
  }

  dframe
}