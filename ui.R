library(shiny)


shinyUI(
  fluidPage(theme = 'slate.css',
    titlePanel('Mínimos Cuadrados Ordinarios'),
    sidebarLayout(
      sidebarPanel(
        
        tags$h3("Estimacion Beta"),
        
        sliderInput("beta0", "Beta_0:", 
                    min=6, max=8, value=7, step=0.1),
        
        sliderInput("beta1", "Beta_1:", 
                    min=2, max=4, value=3, step=0.1)
        ),
      mainPanel(
        plotOutput("graph1"),
        plotOutput("graph2")  
      )
    )
  )
)