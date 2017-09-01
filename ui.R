library(shiny)


shinyUI(
  fluidPage(theme = 'bootstrap.css',
    titlePanel('Mínimos Cuadrados Ordinarios'),
    sidebarLayout(
      sidebarPanel(
        
        tags$h3("Parámetros"),
        
        sliderInput("beta0", "Beta 0:", 
                    min=-8, max=8, value=0, step=0.1),
        
        sliderInput("beta1", "Beta 1:", 
                    min=0, max=8, value=1, step=0.1)
        ),
      mainPanel(
        tags$h3('Modelo ajustado'),
        plotOutput('graph1'),
        tags$h3('Suma de cuadrados'),
        plotOutput('graph2')  
      )
    )
  )
)