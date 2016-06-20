library(shiny)
library(dplyr)
library(tidyr)
library(XML)
library(x3prplus)
library(ggplot2)
library(plotly)
library(gridExtra)
library(zoo)
library(reshape2)
library(randomForest)

options(shiny.maxRequestSize=30*1024^2) 

Sys.setenv("plotly_username" = "erichare")
Sys.setenv("plotly_api_key" = "xd0oxpeept")

# input <- list(file1 = list(datapath = "~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 1-5.x3p"), file2 = list(datapath = "~/GitHub/imaging-paper/app/images/Hamby252_3DX3P1of2/Br1 Bullet 2-1.x3p"), span = 0.03)
# bullet1 <- function() {x3prplus::read.x3pplus(input$file1$datapath)}
# bullet2 <- function() {x3prplus::read.x3pplus(input$file2$datapath)}
# values <- list(xcoord1 = 50, xcoord2 = 100, bounds1 = c(300, 2200), bounds2 = c(300, 2200))
# crosscut1 <- function() {get_crosscut(bullet = bullet1(), x = values$xcoord1)}
# crosscut2 <- function() {get_crosscut(bullet = bullet2(), x = values$xcoord2)}
# loess1 <- function() {fit_loess(bullet = crosscut1(), groove = list(groove = values$bounds1), span = 0.03)}
# loess2 <- function() {fit_loess(bullet = crosscut2(), groove = list(groove = values$bounds2), span = 0.03)}
# processed1 <- function() {processBullets(bullet = bullet1(), name = input$file1$datapath, x = values$xcoord1, grooves = values$bounds1)}
# processed2 <- function() {processBullets(bullet = bullet2(), name = input$file2$datapath, x = values$xcoord2, grooves = values$bounds2)}
# bullets_processed <- list(b1 = processed1(), b2 = processed2())
# smoothed <- function(){bullets_processed %>% bind_rows %>% bulletSmooth(span = input$span)}
# myalign <- function(){x3prplus:::bulletAlign_test(data = smoothed())}

shinyServer(function(input, output, session) {
    
    values <- reactiveValues(xcoord1 = NULL,
                             xcoord2 = NULL,
                             bounds1 = NULL,
                             bounds2 = NULL)
    
    bullet1 <- reactive({
        if (is.null(input$file1)) return(NULL)

        return(x3prplus::read.x3pplus(input$file1$datapath, transpose = input$transpose1))
    })
    
    bullet2 <- reactive({
        if (is.null(input$file2)) return(NULL)

        return(x3prplus::read.x3pplus(input$file2$datapath, transpose = input$transpose2))
    })
    
    observe({
        if (!is.null(bullet1()) && !is.null(bullet2())) updateCheckboxInput(session, "stage0", value = TRUE)
    })
    

    theSurface <- reactive({
        if (!input$stage0) return(NULL)

        b1 <- bullet1()
        b2 <- bullet2()

        surf.b1 <- b1[[2]]
        surf.b2 <- b2[[2]]

        minrows <- min(nrow(surf.b1), nrow(surf.b2))

        surf.mat <- cbind(surf.b1[1:minrows,], surf.b2[1:minrows,])

        x_idx <- seq(1, nrow(surf.mat), by = 2)
        y_idx <- seq(1, ncol(surf.mat), by = 2)

        return(surf.mat[x_idx, y_idx])
    })

    observe({
        updateSliderInput(session, "xcoord1", max = ncol(theSurface()) / 2 )
        updateSliderInput(session, "xcoord2", max = ncol(theSurface()), min = 1 + ncol(theSurface()) / 2)
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
    
    observeEvent(input$suggest, {
        withProgress(message = "Calculating CCF...", expr = {
            crosscut1 <- bulletCheckCrossCut(input$file1$datapath, 
                                             xlimits = seq(25, 500, by = 25), 
                                             transpose = input$transpose1)
            
            crosscut2 <- bulletCheckCrossCut(input$file2$datapath, 
                                             xlimits = seq(25, 500, by = 25), 
                                             transpose = input$transpose2)
            
            updateSliderInput(session, "xcoord1", value = crosscut1)
            updateSliderInput(session, "xcoord2", value = crosscut2 + ncol(theSurface()) / 2)
        })
    })

    observeEvent(input$confirm, {
        values$xcoord1 <- input$xcoord1
        values$xcoord2 <- input$xcoord2 - ncol(theSurface()) / 2
        
        updateCheckboxInput(session, "stage1", value = TRUE)
    })
    
    fortified1 <- reactive({
        if (is.null(values$xcoord1)) return(NULL)
        
        bul <- bullet1()
        bul[[3]] <- "b1"
        names(bul)[3] <- "path"
        
        return(fortify_x3p(bul))
    })
    
    fortified2 <- reactive({
        if (is.null(values$xcoord2)) return(NULL)
        
        bul <- bullet2()
        bul[[3]] <- "b2"
        names(bul)[3] <- "path"
        
        return(fortify_x3p(bul))
    })
    
    crosscut1 <- reactive({
        if (is.null(values$xcoord1)) return(NULL)
        
        return(get_crosscut(bullet = bullet1(), x = values$xcoord1))
    })
    
    crosscut2 <- reactive({
        if (is.null(values$xcoord2)) return(NULL)
        
        return(get_crosscut(bullet = bullet2(), x = values$xcoord2))
    })
    
    observe({
        updateSliderInput(session, "bounds1", max = floor(max(fortified1()$y)), value = c(0, floor(max(fortified1()$y))))
        updateSliderInput(session, "bounds2", max = floor(max(fortified2()$y)), value = c(0, floor(max(fortified2()$y))))
    })
    
    observeEvent(input$suggestgrooves, {
        withProgress(message = "Locating grooves...", expr = {
            groove1 <- get_grooves(crosscut1())
            groove2 <- get_grooves(crosscut2())
            
            updateSliderInput(session, "bounds1", value = groove1$groove)
            updateSliderInput(session, "bounds2", value = groove2$groove)
        })
    })
    
    output$crosssection <- renderPlot({
        if (is.null(fortified1()) || is.null(fortified2())) return(NULL)
        
        fortified <- fortified1()
        fortified2 <- fortified2()
        
        myx <- unique(fortified$x)
        xval <- myx[which.min(abs(myx - values$xcoord1))]
        myx2 <- unique(fortified2$x)
        xval2 <- myx2[which.min(abs(myx2 - values$xcoord2))]
        
        plotdat <- fortified %>%
            filter(x == xval, y >= input$bounds1[1], y <= input$bounds1[2]) %>%
            select(-x) %>%
            full_join(
                fortified2 %>%
                    filter(x == xval2, y >= input$bounds2[1], y <= input$bounds2[2]) %>%
                    select(-x)
            , by = c("y" = "y")) %>%
            rename(bullet1 = value.x, bullet2 = value.y) %>%
            gather(key = bullet, value = value, bullet1:bullet2)
        
        ggplot(data = plotdat, aes(x = y, y = value)) +
            facet_wrap(~bullet, nrow = 2) +
            geom_line(size = 1) +
            xlim(c(0, max(plotdat$y))) +
            theme_bw()
    })
    
    observeEvent(input$confirm2, {
        values$bounds1 <- input$bounds1
        values$bounds2 <- input$bounds2
        
        updateCheckboxInput(session, "stage2", value = TRUE)
    })
    
    loess1 <- reactive({
        if (!input$stage2) return(NULL)
        
        return(fit_loess(bullet = crosscut1(), groove = list(groove = values$bounds1), span = input$span))
    })
    
    loess2 <- reactive({
        if (!input$stage2) return(NULL)
        
        return(fit_loess(bullet = crosscut2(), groove = list(groove = values$bounds2), span = input$span))
    })
    
    processed1 <- reactive({
        if (!input$stage2) return(NULL)
        
        myx <- unique(fortified1()$x)
        xval <- myx[which.min(abs(myx - values$xcoord1))]
        
        processBullets(bullet = bullet1(), name = "b1", x = xval, grooves = values$bounds1)
    })
    
    processed2 <- reactive({
        if (!input$stage2) return(NULL)
        
        myx <- unique(fortified2()$x)
        xval <- myx[which.min(abs(myx - values$xcoord2))]
        
        processBullets(bullet = bullet2(), name = "b2", x = xval, grooves = values$bounds2)
    })
    
    smoothed <- reactive({
        if (is.null(processed1()) || is.null(processed2())) return(NULL)
        
        bullets_processed <- list(b1 = processed1(), b2 = processed2())
        
        result <- bullets_processed %>% bind_rows %>% bulletSmooth(span = input$span)
        result$bullet <- c(rep("b1", nrow(processed1())), rep("b2", nrow(processed2())))
        
        return(result)
    })
    
    output$loess1 <- renderPlot({
        if (is.null(loess1()) || is.null(smoothed())) return(NULL)
        p1 <- qplot(y, l30, data = filter(smoothed(), bullet == "b1"), geom = "line") +
            theme_bw()
        grid.arrange(loess1()$fitted, p1, ncol = 2)
    })
    
    output$loess2 <- renderPlot({
        if (is.null(loess2()) || is.null(smoothed())) return(NULL)
        p2 <- qplot(y, l30, data = filter(smoothed(), bullet == "b2"), geom = "line") +
            theme_bw()
        grid.arrange(loess2()$fitted, p2, ncol = 2)
    })
    
    observeEvent(input$confirm3, {
        updateCheckboxInput(session, "stage3", value = TRUE)
    })
    
    myalign <- reactive({
        if (is.null(smoothed())) return(NULL)

        x3prplus:::bulletAlign_test(data = smoothed())
    })
    
    observeEvent(input$suggestalign, {
        withProgress(message = "Determining alignment...", expr = {
            updateSliderInput(session, "alignment", value = myalign()$lag)
        })
    })
    
    chosenalign <- reactive({
        if (is.null(myalign())) return(NULL)
        
        chosen <- myalign()
        chosen$lag <- input$alignment
        chosen$bullets$y[chosen$bullets$bullet == unique(chosen$bullets$bullet[2])] <- chosen$bullets$y[chosen$bullets$bullet == unique(chosen$bullets$bullet[2])] - min(chosen$bullets$y[chosen$bullets$bullet == unique(chosen$bullets$bullet[2])]) + chosen$lag
        
        return(chosen)
    })
    
    output$alignment <- renderPlot({
        if (is.null(chosenalign())) return(NULL)
            
        mydat <- chosenalign()$bullets
        
        qplot(y, l30, data = mydat, geom = "line", colour = bullet, alpha = I(0.8)) +
            theme(legend.position = "bottom") +
            theme_bw()
    })
    
    observeEvent(input$confirm4, {
        updateCheckboxInput(session, "stage4", value = TRUE)
    })

    CMS <- reactive({
        if (!input$stage4) return(NULL)
        
        bAlign <- chosenalign()

        lofX <- bAlign$bullet
        
        peaks1 <- get_peaks(subset(lofX, bullet == "b1"), smoothfactor = 25)
        peaks2 <- get_peaks(subset(lofX, bullet == "b2"), smoothfactor = 25)
        
        peaks1$lines$bullet <- "b1"
        peaks2$lines$bullet <- "b2"
        
        lines <- striation_identify(peaks1$lines, peaks2$lines)
        maxCMS <- maxCMS(lines$match == TRUE)
        list(maxCMS = maxCMS, ccf = bAlign$ccf, lag = bAlign$lag, 
             lines = lines, bullets = lofX)
    })

    features <- reactive({
        if (is.null(CMS())) return(NULL)

        res <- CMS()

        lofX <- res$bullets
        aligned <- chosenalign()
        b12 <- unique(lofX$bullet)

        subLOFx1 <- subset(aligned$bullets, bullet==b12[1])
        subLOFx2 <- subset(aligned$bullets, bullet==b12[2])

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

       signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))

       data.frame(ccf=res$ccf, lag=res$lag,
                  D=distr.dist,
                  sd.D = distr.sd,
                  b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
                  #num.matches = sum(res$lines$match),
                  signature.length = signature.length,
                  matches.per.y = sum(res$lines$match) / signature.length,
                  #num.mismatches = sum(!res$lines$match),
                  mismatches.per.y = sum(!res$lines$match) / signature.length,
                  #cms = res$maxCMS,
                  cms.per.y = res$maxCMS / signature.length,
                  #cms2 = x3prplus::maxCMS(subset(res$lines, type==1 | is.na(type))$match),
                  cms2.per.y = x3prplus::maxCMS(subset(res$lines, type==1 | is.na(type))$match) / signature.length,
                  #non_cms = x3prplus::maxCMS(!res$lines$match),
                  non_cms.per.y = x3prplus::maxCMS(!res$lines$match) / signature.length,
                  #left_cms = max(knm[1] - km[1], 0),
                  left_cms.per.y = max(knm[1] - km[1], 0) / signature.length,
                  #right_cms = max(km[length(km)] - knm[length(knm)],0),
                  right_cms.per.y = max(km[length(km)] - knm[length(knm)],0) / signature.length,
                  #left_noncms = max(km[1] - knm[1], 0),
                  left_noncms.per.y = max(km[1] - knm[1], 0) / signature.length,
                  #right_noncms = max(knm[length(knm)]-km[length(km)],0),
                  right_noncms.per.y = max(knm[length(knm)]-km[length(km)],0) / signature.length,
                  #sumpeaks = sum(abs(res$lines$heights[res$lines$match])),
                  sumpeaks.per.y = sum(abs(res$lines$heights[res$lines$match])) / signature.length
        )
    })

    output$features <- renderDataTable({
        if (is.null(features())) return(NULL)

        result <- as.data.frame(t(features()))
        result <- cbind(feature = rownames(result), result)
        names(result)[2] <- "value"

        return(result)
    })
    
    observeEvent(input$confirm5, {
        updateCheckboxInput(session, "stage5", value = TRUE)
    })

    output$rfpred <- renderText({
        if (is.null(features())) return(NULL)

        features <- features()
        features$b1 <- gsub(".x3p", "", basename(as.character(features$b1)))
        features$b2 <- gsub(".x3p", "", basename(as.character(features$b2)))
        features$span <- span

        includes <- setdiff(names(features), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))

        load("data/rf.RData")

        matchprob <- round(predict(rtrees, newdata = features[,includes], type = "prob")[,2], digits = 4)
        if (matchprob == 0) matchprob <- "< .0001" else if (matchprob == 1) matchprob <- "> .9999"

        #rtrees <- randomForest(factor(match)~., data=CCFs[,includes], ntree=300)
        return(paste0("The probability of a match is ", matchprob))
    })

})
