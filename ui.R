library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
    headerPanel("Bullet Rotation Prototype"),
    
    sidebarLayout(
        sidebarPanel(width = 3,
            useShinyjs(),
            tags$head(tags$style("#info{font-size: 18px;}")),
            
            conditionalPanel(condition = "!input.stage0",
                 h4("Stage 0 Options"),
                 fileInput("file1", "First Bullet Land"),
                 checkboxInput("transpose1", "Transpose Bullet 1"),
                 
                 fileInput("file2", "Second Bullet Land"),
                 checkboxInput("transpose2", "Transpose Bullet 2"),
                 
                 hidden(checkboxInput("stage0", "Stage 0")),
                 hidden(checkboxInput("stage1", "Stage 1")),
                 hidden(checkboxInput("stage2", "Stage 2"))
            ),
            
            conditionalPanel(condition = "input.stage0 && !input.stage1",
                h4("Stage 1 Options"),
                
                actionButton("suggest", "Automatically Suggest"),
                
                hr(),
                
                sliderInput("xcoord1", "X Coordinate (First Land)", min = 1, max = 251, value = 136, step = 1),
                sliderInput("xcoord2", "X Coordinate (Second Land)", min = 252, max = 502, value = 386, step = 1),
                
                hr(),
                
                actionButton("confirm", "Confirm Coordinates")
            ),
            
            conditionalPanel(condition = "input.stage1 && !input.stage2",
                h4("Stage 2 Options"),
                
                actionButton("suggestgrooves", "Automatically Suggest"),
                
                hr(),
                
                sliderInput("bounds1", "Coordinate Bounds 1", min = 0, max = 2400, value = c(0, 2400)),
                sliderInput("bounds2", "Coordinate Bounds 2", min = 0, max = 2400, value = c(0, 2400)),
                
                hr(),
                
                actionButton("confirm2", "Confirm Bounds")
            ),
            
            conditionalPanel(condition = "input.stage2 && !input.stage3",
                h4("Stage 3 Options")
            ),

            #sliderInput("span", "Loess Span", min = 0.01, max = 0.2, value = 0.03, step = 0.01),
            #sliderInput("smoothfactor", "Smoothing Factor", min = 0, max = 50, step = 5, value = 25),
            #sliderInput("groove_cutoff", "Groove Cutoff", min = 1, max = 1000, value = 500),

            #hr(),
            
            hidden(
                h4("Lighting Options"),
                sliderInput("subsample", "Subsample Factor", min = 1, max = 20, value = 2),
                sliderInput("ambient_lighting", "Ambient Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
                sliderInput("diffuse_lighting", "Diffuse Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
                sliderInput("specular_lighting", "Specular Lighting", min = 0, max = 2, step = 0.05, value = 0.05),
                sliderInput("roughness_lighting", "Roughness Lighting", min = 0, max = 1, step = 0.1, value = 0.5),
                sliderInput("fresnel_lighting", "Fresnel Lighting", min = 0, max = 5, step = 0.1, value = 0.2)
            )
        ),
        
        mainPanel(width = 9,
            conditionalPanel(condition = "!input.stage0",
                 h2("Stage 0: Preliminary Information"),
                 hr(),
                 div(id = "info", HTML("This app will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun barrel. We compare at the bullet land level.<br><br>To begin, upload the two .x3p files representing the two bullet lands you wish to compare."))
            ),
            conditionalPanel(condition = "input.stage0 && !input.stage1",
                 h2("Stage 1: Finding a Stable Region"),
                 hr(),
                 div(id = "info", HTML("Below you will find surface topologies of the two bullet lands you have uploaded. You can rotate, pan, zoom, and perform a number of other functions to examine the surfaces.<br><br>Our goal is to find a <b>stable region</b>. We want an area of the bullet where there is minimal noise or tank rash, but plenty of prounounced striation markings.<br><br>Our algorithm steps through cross-sections of each land at a fixed step size, and uses the CCF (cross-correlation function) to determine stability (a high CCF means that subsequent cross-sections are similar to each other). We begin this procedure near the area where striation markings are typically most pronounced.<br><br>You may choose the location to take a cross-section, or allow our algorithm to do so for you."))           
            ),
            conditionalPanel(condition = "input.stage1 && !input.stage2",
                 h2("Stage 2: Removing Grooves"),
                 hr(),
                 div(id = "info", HTML("The cross-sections you have taken are shown below. Our next goal will be to remove the grooves, which contain no relevant information for matching, and greatly exceed the size of a typical striation mark.<br><br>Our algorithm uses a double-pass smoothing method to determine the location of the grooves. You may once again use our algorithm to suggest groove locations, or define them yourself. As you adjust the sliders, the plot will automatically update.")),
                 hr(),
                 
                 plotOutput("crosssection")
            ),
            conditionalPanel(condition = "input.stage2 && !input.stage3",
                 h2("Stage 3: Removing Global Structure"),
                 hr(),
                 div(id = "info", HTML("We have removed the grooves, but the global structure of cross-section dominates the overall appearance, making striae more difficult to locate.<br><br>We are going to fit a loess regression to model this structure. The loess regression includes a span parameter which adjusts the amount of smoothing used. Different values will yield different output. We default to a span of 0.75, but this may be adjusted as desired.")),
                 hr(),
                 
                 plotOutput("loess1"),
                 plotOutput("loess2")
            ),
            conditionalPanel(condition = "input.stage0",
                plotlyOutput("trendPlot", height = "700px")
            )
            #hr(),
            #h3(textOutput("rfpred")),
            #hr(),
            #plotOutput("residuals"),
            #hr(),
            #dataTableOutput("features")
        )
    )
))
