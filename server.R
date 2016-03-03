library(shiny)
library(dplyr)
library(XML)
library(x3prplus)
library(plotly)
library(gridExtra)
library(zoo)
library(reshape2)
library(randomForest)

options(shiny.maxRequestSize=30*1024^2) 

source(system.file("gui/view", "helpers.R", package = "x3pr"))

Sys.setenv("plotly_username" = "erichare")
Sys.setenv("plotly_api_key" = "xd0oxpeept")

matches <- read.csv("data/matches.csv", header=FALSE, stringsAsFactors = FALSE)
matches$V3 <- paste("Ukn Bullet",matches$V3)
matches$V4 <- paste("Ukn Bullet",matches$V4)
matches$V5 <- paste("Ukn Bullet",matches$V5)
matches$id <- 1:nrow(matches)

mm <- melt(matches, id.var="id")
mm <- subset(mm, value != "Ukn Bullet ")

shinyServer(function(input, output, session) {
    
    values <- reactiveValues(path1 = "images/Hamby252_3DX3P1of2/Br1 Bullet 2-5.x3p", 
                             path2 = "images/Hamby252_3DX3P1of2/Br1 Bullet 1-3.x3p",
                             fort1_fixed = NULL, 
                             fort2_fixed = NULL,
                             xcoord = NULL)
    
    bullet1 <- reactive({
        if (!is.null(input$file1)) values$path1 <- input$file1$datapath
        
        return(x3pr::read.x3p(values$path1))
    })
    
    bullet2 <- reactive({
        if (!is.null(input$file2)) values$path2 <- input$file2$datapath
        
        return(x3pr::read.x3p(values$path2))
    })
    
    theSurface <- reactive({
        if (is.null(bullet1()) || is.null(bullet2())) return(NULL)
        
        b1 <- bullet1()
        b2 <- bullet2()
        
        surf.b1 <- b1[[2]]
        surf.b2 <- b2[[2]]
        if (input$transpose) {
            surf.b1 <- t(surf.b1)
            surf.b2 <- t(surf.b2)
        }
        
        minrows <- min(nrow(surf.b1), nrow(surf.b2))
        
        surf.mat <- cbind(surf.b1[1:minrows,], surf.b2[1:minrows,])
        
        x_idx <- seq(1, nrow(surf.mat), by = input$subsample)
        y_idx <- seq(1, ncol(surf.mat), by = input$subsample)
        
        return(surf.mat[x_idx, y_idx])
    })
    
    observe({
         updateSliderInput(session, "xcoord", max = ncol(theSurface()) / 2 * input$subsample)
    })
    
    output$trendPlot <- renderPlotly({
        if (is.null(theSurface())) return(NULL)
        
        p <- plot_ly(z = theSurface(), type = "surface", showscale = FALSE, lighting = list(ambient = input$ambient_lighting,
                                                                                             diffuse = input$diffuse_lighting,
                                                                                             specular = input$specular_lighting,
                                                                                             roughness = input$roughness_lighting,
                                                                                             fresnel = input$fresnel_lighting))
        p
    })
    
    observeEvent(input$compute, {
        values$xcoord <- input$xcoord
    })
    
    observe({
        values$fort1_fixed <- processBullets(bullet1())
        values$fort2_fixed <- processBullets(bullet2())
    })
    
    output$residuals <- renderPlot({
        if (is.null(values$fort1_fixed) || is.null(values$fort2_fixed) || is.null(values$xcoord)) return(NULL)
        
        smoothed <- bulletSmooth(values$fort1_fixed, span = input$span)
        smoothed2 <- bulletSmooth(values$fort2_fixed, span = input$span)
        
        mydat <- rbind(smoothed, smoothed2)
        mydat$bullet <- c(rep("b1", nrow(smoothed)), rep("b2", nrow(smoothed2)))
        
        aligned <- x3prplus:::bulletAlign_new(mydat)

        lofX <- aligned$bullet
        b12 <- unique(lofX$bullet)
        peaks1 <- get_peaks(subset(lofX, bullet == b12[1]), smoothfactor = input$smoothfactor)
        peaks2 <- get_peaks(subset(lofX, bullet == b12[2]), smoothfactor = input$smoothfactor)
        
        dframe1 <- peaks1$dframe
        dframe2 <- peaks2$dframe
        dframe <- rbind(dframe1, dframe2)
        dframe$bullet <- c(rep("b1", nrow(dframe1)), rep("b2", nrow(dframe2)))
       
        qplot(y, smoothed, data = dframe, colour = bullet, geom = "line", size = I(2)) +
            theme_bw()
    })
    
    processed1 <- reactive({
        if (is.null(values$fort1_fixed) || is.null(values$xcoord)) return(NULL)
        
        bul <- bullet1()
        bul[[3]] <- values$path1
        names(bul)[3] <- "path"
        
        myx <- unique(fortify_x3p(bul)$x)
        xval <- myx[which.min(abs(myx - values$xcoord))]
        
        processBullets(bullet = bul, name = bul$path, x = xval)
    })
    
    processed2 <- reactive({
        if (is.null(values$fort2_fixed) || is.null(values$xcoord)) return(NULL)
        
        bul <- bullet2()
        bul[[3]] <- values$path2
        names(bul)[3] <- "path"
        
        myx <- unique(fortify_x3p(bul)$x)
        xval <- myx[which.min(abs(myx - values$xcoord))]
        
        processBullets(bullet = bul, name = bul$path, x = xval)
    })
    
    smoothed <- reactive({
        if (is.null(processed1()) || is.null(processed2())) return(NULL)

        bulletSmooth(rbind(processed1(), processed2()))
    })
    
    CMS <- reactive({
        if (is.null(smoothed())) return(NULL)
        
        br1 <- filter(smoothed(), bullet == values$path1)
        br2 <- filter(smoothed(), bullet == values$path2)
        
        bulletGetMaxCMS(br1, br2, span = 25)
    })
    
    features <- reactive({
        if (is.null(CMS())) return(NULL)
        
        res <- CMS()

        lofX <- res$bullets
        
        aligned <- x3prplus:::bulletAlign_new(lofX)
        b12 <- unique(lofX$bullet)
        
        subLOFx1 <- subset(aligned$bullets, bullet==b12[1])
        subLOFx2 <- subset(aligned$bullets, bullet==b12[2]) 
        
        subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
        subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
        
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
        data.frame(ccf=aligned$ccf, lag=aligned$lag, 
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
    
    output$features <- renderDataTable({
        if (is.null(features())) return(NULL)
        
        result <- as.data.frame(t(features()))
        result <- cbind(feature = rownames(result), result)
        names(result)[2] <- "value"
        
        return(result)
    })
    
    output$rfpred <- renderText({
        if (is.null(features())) return(NULL)
        
        features <- features()
        features$b1 <- gsub(".x3p", "", basename(as.character(features$b1)))
        features$b2 <- gsub(".x3p", "", basename(as.character(features$b2)))
        features$span <- span
        
        includes <- setdiff(names(features), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))
        
       # CCFs <- read.csv("data/bullet-stats.csv")
        load("data/rf.RData")
        
        matchprob <- round(predict(rtrees, newdata = features[,includes], type = "prob")[,2], digits = 4)
        if (matchprob == 0) matchprob <- "< .0001" else if (matchprob == 1) matchprob <- "> .9999"
        
        #rtrees <- randomForest(factor(match)~., data=CCFs[,includes], ntree=300)
        return(paste0("The probability of a match is ", matchprob))
    })
    
})
