library(shiny)
library(tidyverse)
library(ggthemes)
library(directlabels)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Comparative Statics"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Income",
                        "Average Annual Income (thousands of dollars):",
                        min = 15,
                        max = 75,
                        value = 35),
            sliderInput("ps",
                        "Price of Sugar ($/lb):",
                        min = 0.05,
                        max = 5,
                        value = 0.2),
            sliderInput("pc",
                        "Price of Cocoa ($/lb):",
                        min = 1,
                        max = 6,
                        value = 3)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        p <- 0:12
        D1 <- 8.56 - p - 0.3*0.2 + 0.1*35 
        S1 <- 9.6 + 0.5*p - 0.2*3
        
        if(input$Income == 35 & input$ps == 0.2){
        base <- data.frame(p,D1,S1)
        } else {
            D2 <-  8.56 - p - 0.3*input$ps + 0.1*input$Income 
            base <- data.frame(p,D1,S1,D2)
        }
        if(input$pc == 3){
            
        }else{
            base$S2 <- 9.6 + 0.5*p - 0.2*input$pc
        }
        base <- base %>% 
            gather(key = "Function", value = "Q", -p)


        
        ggplot(base, aes(Q,p,color = Function))+
            geom_line()+
            scale_color_stata()+
            theme_bw()+
            xlab("Q, Millions of tons of coffee per year")+
            ylab("p, $ per lb")+
            theme(legend.position = "none")+
            geom_dl(aes(label = Function), method = list(dl.combine( "last.points"), cex = 0.8))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
