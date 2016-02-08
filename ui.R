library(shiny)
library(shinythemes)
library(plotly)

source(system.file("gui/view", "helpers.R", package="x3pr"))

shinyUI(fluidPage(theme = shinytheme("cerulean"),
    headerPanel("Bullet Rotation Prototype"),
    
    sidebarLayout(
        sidebarPanel(width = 3,
            h4("Bullet Options"),
            fileInput("file1", "First Bullet"),
            fileInput("file2", "Second Bullet"),
            
            hr(),
            
            h4("Matching"),
            sliderInput("xcoord", "X Coordinate", min = 1, max = 1000, value = 136, step = 1),
            
            actionButton("compute", "Compute Match Probability"),
            
            hr(),
            
            h4("Plot Options"),
            sliderInput("subsample", "Subsample Factor", min = 1, max = 20, value = 2),
            sliderInput("ambient_lighting", "Ambient Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
            sliderInput("diffuse_lighting", "Diffuse Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
            sliderInput("specular_lighting", "Specular Lighting", min = 0, max = 2, step = 0.05, value = 0.05),
            sliderInput("roughness_lighting", "Roughness Lighting", min = 0, max = 1, step = 0.1, value = 0.5),
            sliderInput("fresnel_lighting", "Fresnel Lighting", min = 0, max = 5, step = 0.1, value = 0.2)
        ),
        
        mainPanel(width = 9,
            plotlyOutput("trendPlot", height = "700px"),
            hr(),
            h3(textOutput("rfpred")),
            hr(),
            plotOutput("residuals"),
            hr(),
            dataTableOutput("features")
        )
    )
))
