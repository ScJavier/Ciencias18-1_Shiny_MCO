library("shiny")
library("ggplot2")

# Simulation and Shiny Application of Flue Season Dynamics
shinyServer(function(input, output) {
  
  output$graph1 <- renderPlot({
    
    beta0 <- input$beta0  # Infected
    beta1  <- input$beta1   # Total Population
    
    set.seed(1994)
    # Change in Susceptible
    x  <- round(runif(16, -5, 5), 1)
    b0 <- round(runif(1, -2, 2), 2)
    b1 <- round(runif(1, 1, 5), 2)
    
    # Change in Contageous
    y  <- b0 +b1*x + rnorm(16, 0, 2)
    
    q2 <- function(bet0, bet1){
      m <- length(bet0); n <- length(bet1)
      res <- matrix(0, m, n)
      for (i in 1:m){
        for (j in 1:n){
          aux <- y - bet0[i] - bet1[j] * x
          res[i, j] <- sum(aux^2)
        }
      }
      return(res)
    }
    
    re <- round(q2(beta0, beta1), 2)
    
    par(mar = c(2, 2.5, 1, 1))
    
    p <- plot(x,y,axes = F,bg="blue3",pch=21,col="blue")
    axis(1, at = seq(-5, 5, by = 1))
    axis(2, at = seq(-22, 24, by = 2), las = 2)
    r <- abline(a = beta0, b = beta1, col = 'red2', lwd = 2)
    abline(h=0,col="cyan3")
    abline(v=0,col="cyan3")
    text(-2, 15, paste0('y = ', paste(beta0),' + ', paste(beta1), 'x, Q = ', re), col = 'red2')
    text(-2, 10, paste0('b0 = ', paste(b0),' b1= ', paste(b1)), col = 'blue2')
    box()
  })
  
  output$graph2 <- renderPlot({
    
    beta0 <- input$beta0  # Infected
    beta1  <- input$beta1   # Total Population
    
    set.seed(1994)
    # Change in Susceptible
    x  <- round(runif(16, -5, 5), 1)
    b0 <- round(runif(1, -2, 2), 2)
    b1 <- round(runif(1, 1, 5), 2)
    # Change in Contageous
    y  <- b0 +b1*x + rnorm(16, 0, 2)
    
    q2 <- function(bet0, bet1){
      m <- length(bet0); n <- length(bet1)
      res <- matrix(0, m, n)
      for (i in 1:m){
        for (j in 1:n){
          aux <- y - bet0[i] - bet1[j] * x
          res[i, j] <- sum(aux^2)
        }
      }
      return(res)
    }
    
    b0x <- seq(-10, 10, by = 0.1)
    b1y <- seq(-10, 10, by = 0.1)
    q2z <- q2(b0x, b1y)
    
    curvas <- c(seq(30, 120, by = 20), seq(100, 300, by = 100), seq(400, 2000, by = 200))
    
    par(mar = c(2, 2.5, 1, 1))
    
    image(b0x,b1y,q2z,col=topo.colors(16),breaks = sort(curvas),
          xlim = c(-7, 8), ylim = c(0,8.5), axes = F)
    contour(b0x, b1y, q2z, levels = curvas, add = TRUE)
    axis(1, at = seq(-7, 8, by = 1))
    axis(2, at = seq(0, 9, by = 1),las=2)
    box()
    points(beta0, beta1, pch = 16, col = 'red2')
  })
  
})