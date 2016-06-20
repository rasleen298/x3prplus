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
                 hidden(checkboxInput("stage2", "Stage 2")),
                 hidden(checkboxInput("stage3", "Stage 3")),
                 hidden(checkboxInput("stage4", "Stage 4")),
                 hidden(checkboxInput("stage5", "Stage 5"))
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
                h4("Stage 3 Options"),
                
                sliderInput("span", "Loess Span", min = 0.01, max = 0.99, value = 0.03, step = 0.01),
                
                actionButton("confirm3", "Confirm Span")
            ),
            
            conditionalPanel(condition = "input.stage3 && !input.stage4",
                h4("Stage 4 Options"),
                
                actionButton("suggestalign", "Automatically Suggest"),
                
                hr(),
                
                numericInput("alignment", "Alignment", min = -1000, max = 1000, step = 1, value = 0),
                
                actionButton("confirm4", "Confirm Alignment")
            ),
            
            conditionalPanel(condition = "input.stage4 && !input.stage5",
                 h4("Stage 5 Options"),
                 
                 actionButton("confirm5", "Confirm Features")
            ),
            
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
              conditionalPanel(condition = "input.stage5",
                   h2("Stage 6: Predicted Probability"),
                   hr(),
                   div(id = "info", HTML("We use these features to train a Random Forest to help differentiate between a match and a non-match. Using the forest, we predict on the features you just extracted. Your predicted probability of a match is given below.")),
                   
                   h3(textOutput("rfpred")),
                   
                   hr()
              ),
            conditionalPanel(condition = "!input.stage0 || input.stage5",
                 h2("Stage 0: Preliminary Information"),
                 hr(),
                 div(id = "info", HTML("This app will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun barrel. We compare at the bullet land level.<br><br>To begin, upload the two .x3p files representing the two bullet lands you wish to compare."))
            ),
            conditionalPanel(condition = "input.stage0 && !input.stage1 || input.stage5",
                 h2("Stage 1: Finding a Stable Region"),
                 hr(),
                 div(id = "info", HTML("Below you will find surface topologies of the two bullet lands you have uploaded. You can rotate, pan, zoom, and perform a number of other functions to examine the surfaces.<br><br>Our goal is to find a <b>stable region</b>. We want an area of the bullet where there is minimal noise or tank rash, but plenty of pronounced striation markings.<br><br>Our algorithm steps through cross-sections of each land at a fixed step size, and uses the CCF (cross-correlation function) to determine stability (a high CCF means that subsequent cross-sections are similar to each other). We begin this procedure near the area where striation markings are typically most pronounced.<br><br>You may choose the location to take a cross-section, or allow our algorithm to do so for you."))           
            ),
            conditionalPanel(condition = "input.stage1 && !input.stage2 || input.stage5",
                 h2("Stage 2: Removing Grooves"),
                 hr(),
                 div(id = "info", HTML("The cross-sections you have taken are shown below. Our next goal will be to remove the grooves, which contain no relevant information for matching, and greatly exceed the size of a typical striation mark.<br><br>Our algorithm uses a double-pass smoothing method to determine the location of the grooves. You may once again use our algorithm to suggest groove locations, or define them yourself. As you adjust the sliders, the plot will automatically update.")),
                 hr(),
                 
                 plotOutput("crosssection")
            ),
            conditionalPanel(condition = "input.stage2 && !input.stage3 || input.stage5",
                 h2("Stage 3: Removing Global Structure"),
                 hr(),
                 div(id = "info", HTML("We have removed the grooves, but the global structure of the cross-section dominates the overall appearance, making striae more difficult to locate.<br><br>We are going to fit a loess regression to model this structure. The loess regression includes a span parameter which adjusts the amount of smoothing used. Different values will yield different output. We default to a span of 0.03, but this may be adjusted as desired.")),
                 hr(),
                 
                 plotOutput("loess1"),
                 plotOutput("loess2")
            ),
            conditionalPanel(condition = "input.stage3 && !input.stage4 || input.stage5",
                 h2("Stage 4: Aligning Signatures"),
                 hr(),
                 div(id = "info", HTML("The residuals from the loess fit we have extracted in the previous stage are called the bullet <b>signatures</b>. They will form the basis for the rest of the analysis.<br><br>Because the signatures are defined by the residuals, the peaks and valleys visible in this plot represent the striation markings we are looking for. In order to make matching easier, our next step is to align the two signatures. Our algorithm suggests an optimal alignment, but it can be adjusted if necessary.")),
                 
                 plotOutput("alignment")
            ),
            conditionalPanel(condition = "input.stage4",
                 h2("Stage 5: Extract Features"),
                 hr(),
                 div(id = "info", HTML("We now have smoothed, aligned bullet signatures. This gives us a number of features we can extract.<br><br>At this point, there is really nothing left to configure about the algorithm. The features extracted are displayed below. Press Confirm Features when you are ready to get your predicted probability of a match.")),
                 
                 dataTableOutput("features")
            ),
            conditionalPanel(condition = "input.stage0",
                plotlyOutput("trendPlot", height = "700px")
            )
        )
    )
))
