library(x3pr)
library(x3prplus)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(zoo)

knowndatadir <- "app/images/Hamby252_3DX3P1of2/"
unknowndatadir <- "app/images/Hamby252_3DX3P2of2/"


###############
# can we identify the barrels the unknown bullets came from?

# new_get_bullet <- function(path, x = 243.75, bullet = NULL) {
#     if (is.null(bullet)) bullet <- read.x3p(path)
#     dbr111 <- fortify_x3p(bullet)
#     
#     pickx <- dbr111$x[which.min(abs(x - unique(dbr111$x)))]
#     
#     dbr111.fixx <- dbr111[dbr111$x == pickx,]
#     
#     return(dbr111.fixx)
# }

# new_processBullets <- function(bullet, x = 100) {
#     crosscuts <- unique(fortify_x3p(bullet)$x)
#     crosscuts <- crosscuts[crosscuts >= min(x)]
#     crosscuts <- crosscuts[crosscuts <= max(x)]
#     if (length(x) > 2) crosscuts <- crosscuts[crosscuts %in% x]
#     
#     list_of_fits <- lapply(crosscuts, function(x) {
#         br111 <- get_crosscut(path = NULL, x = x, bullet = bullet)
#         br111.groove <- get_grooves(br111)
#         fit_loess(br111, br111.groove)$resid$data
#     })
#     lof <- list_of_fits %>% bind_rows
#     
#     data.frame(lof, bullet = bullet$path, stringsAsFactors = FALSE)
# }

# new_bulletGetMaxCMSXXX <- function(lof1, lof2, span=35) {
#     lof <- rbind(lof1, lof2)
#     bAlign = bulletAlign(lof)
#     lofX <- bAlign$bullet  
#     
#     b12 <- unique(lof$bullet)
#     peaks1 <- get_peaks(subset(lofX, bullet==b12[1]), smoothfactor = span)
#     peaks2 <- get_peaks(subset(lofX, bullet == b12[2]), smoothfactor = span)
#     
#     #qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
#     #    theme_bw() +
#     #    geom_rect(data=peaks1$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25)) +
#     #    geom_rect(data=peaks2$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25))
#     
#     peaks1$lines$bullet <- b12[1]
#     peaks2$lines$bullet <- b12[2]
#     
#     lines <- striation_identifyXXX(peaks1$lines, peaks2$lines)
#     
#     #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
#     #     theme_bw() +
#     #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
#     #     ylim(c(-6,6)) +
#     #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
#     #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE) 
#     
#     maxCMS <- maxCMS(lines$match==TRUE)
#     list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=lofX)
# }  

# match unknown land using crosscuts
ccs <- read.csv("csvs/crosscuts-25.csv")
all_bullets <- lapply(as.character(ccs$path), function(x) {
    result <- read.x3p(x)
    result[[3]] <- x
    names(result)[3] <- "path"
        
    return(result)
})

knowns <- all_bullets[1:120]
unknowns <- all_bullets[121:210]
bullets_processed <- lapply(all_bullets, function(bul) {
    cat("Computing processed bullet", basename(bul$path), "\n")
    processBullets(bullet = bul, name = bul$path, x = ccs$cc[which(ccs$path == bul$path)])
})
names(bullets_processed) <- as.character(ccs$path)

bullets_smoothed <- bullets_processed %>% bind_rows %>% bulletSmooth

for (span in c(10, 20, 25, 30, 40)) {
    dataStr <- sprintf("data-new-%d-25", span) # using crosscuts-25.csv
    
    if (!file.exists(dataStr)) dir.create(dataStr)
    for (j in 1:90) {
        reslist <- lapply(knowns, function(x) {
            cat("Processing", j, "vs", basename(x$path), "with span", span, "\n")
            
            br1 <- filter(bullets_smoothed, bullet == x$path)
            br2 <- filter(bullets_smoothed, bullet == unknowns[[j]]$path)
            
            bulletGetMaxCMSXXX(br1, br2, span=span)
        })
        save(reslist, file=file.path(dataStr, sprintf("unkn%d.RData", j)))
    }
}
