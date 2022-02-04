#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tibble)
library(tidyr)
library(magrittr)
library(dplyr)
library(RColorBrewer)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gradient Descent in R"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("beta0",
                        "Intercept",
                        value = 0),
            numericInput("beta1",
                        "Slope",
                        value = 0),
            numericInput("mean",
                         "Error mean",
                         value = 0),
            numericInput("sd",
                         "Error SD",
                         value = 1),
            actionButton("generate", label = "Generate data"),
            numericInput("rho",
                         "Learning rate",
                         value = 0.001),
            sliderInput("epoch",
                        "Epochs",
                        min = 0,
                        max = 1000,
                        value = 100,
                        step = 10),
            actionButton("run", label = "Run model"),
            numericInput("epochi",
                         "Epoch to see prediction",
                         min = 0,
                         max = 1000,
                         value = 100)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("dataPlot"),
           plotOutput("modelPlot"),
           plotOutput("evalPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe(updateSliderInput(session, "epochi",
                              max = input$epoch,
                              value = input$epoch))
    
    x <- matrix(c(1:100), ncol = 1)
    Xmeans <- mean(x)
    Xstds <- sd(x)
    Xstand <- cbind(matrix(data = 1, nrow = nrow(x)), ((x - Xmeans)/Xstds))
    
    df.xy <- eventReactive(input$generate, {
            y <- input$beta0 + x*input$beta1 + rnorm(100, input$mean, input$sd)
            df.xy <- data.frame("x" = x, "y" = y)
    }) # close df.xy
    # y <- 50 + x*2 + rnorm(100, 0, 20)
    # 
    # df.xy <- data.frame("x" = x, "y" = y)
    # 
    # ggplot(data = df.xy, aes(x=x, y=y)) +
    #     geom_point() +
    #     theme_bw()
    output$dataPlot <- renderPlot({
        ggplot(data = df.xy(), aes(x = x, y = y)) +
            geom_point() +
            theme_bw()
    }) # close dataPlot
    
    out.list <- eventReactive(input$run, {
        y <- as.matrix(df.xy())[,2,drop=FALSE]
        Ymeans <- mean(y)
        Ystds <- sd(y)
        Ystand <- (y - Ymeans)/Ystds
        w <- matrix(data = 0, nrow = ncol(Xstand))
        out.mat <- matrix(NA, nrow = input$epoch, ncol = (ncol(Xstand) + 2))
        
        
        for(i in 1:input$epoch){
            sqerror_sum <- 0
            for(j in 1:nrow(Xstand)){
                yhat <- as.numeric(Xstand[j,] %*% w)
                error <- as.numeric(Ystand[j,] - yhat)
                w <- w + input$rho * t(Xstand[j,, drop = FALSE]) * error
                
                sqerror_sum <- sqerror_sum + error^2
                
            }
            rmse <- sqrt(sqerror_sum/nrow(Xstand))
            out.mat[i,] <- as.numeric(cbind(i, t(w), rmse))
            
        }
        
        out.df <- data.frame(out.mat) %>%
            rename("Epoch" = "X1", "W1" = "X2", "W2" = "X3", "RMSE" = "X4") %>%
            pivot_longer(cols = c("W1", "W2", "RMSE"),
                         names_to = "Parameters",
                         values_to = "Values")
        
        w.i <- out.mat[input$epochi,c(2,3), drop = FALSE]
         
        y.hat.stand <- Xstand %*% t(w.i)
        y.hat <- y.hat.stand * Ystds + Ymeans
         
        y.full.df <- data.frame("Y" = as.numeric(y), "Yhat" = y.hat) %>%
            add_column("x" = x[,1]) %>%
            pivot_longer(cols = c("Y", "Yhat"), names_to = "Output", values_to = "y")
        
        list(out.df, y.full.df)
    }) # close out.list
    
    
    output$modelPlot <- renderPlot({
        ggplot(data = out.list()[[2]], aes(x = x, y = y, color = Output)) +
            geom_point() +
            scale_color_brewer(palette = "Set2") +
            theme_bw()
    })
    
    output$evalPlot <- renderPlot({
        ggplot(out.list()[[1]]) +
            geom_point(aes(x = Epoch, y = Values, color = Parameters)) +
            scale_color_brewer(palette = "Dark2") +
            facet_grid(rows = vars(Parameters), scales = "free")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
