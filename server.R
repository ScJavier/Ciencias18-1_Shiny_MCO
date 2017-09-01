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
    
    plot(x, y, axes = F, pch = 16, col = 'steelblue', cex = 2)
    axis(1, at = seq(-5, 5, by = 1))
    axis(2, at = seq(-22, 24, by = 4), las = 2)
    r <- abline(a = beta0, b = beta1, col = 'red4', lwd = 3)
    abline(h=0, col = 'darkgray', lwd = 3, lty = 3)
    abline(v=0, col = 'darkgray', lwd = 3, lty = 3)
    text(-2, 15, paste0('Q = ', re), col = 'red4', cex = 1.5)
    text(-2, 10, paste0('b0 = ', paste(b0),'    b1= ', paste(b1)), col = 'steelblue', cex = 1.5)
    box()
  })
  
  output$graph2 <- renderPlot({
    beta0 <- input$beta0
    beta1  <- input$beta1
    
    set.seed(1994)
    x  <- round(runif(16, -5, 5), 1)
    b0 <- round(runif(1, -2, 2), 2)
    b1 <- round(runif(1, 1, 5), 2)
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
    
    par(mar = c(2, 2.5, 1, 1))
    
    colores <- colorRampPalette(c('steelblue', '#FFF9C4', 'red4'))
    curvas <- c(seq(0, 200, length.out=5), seq(200, 1000, length.out=5), seq(1000, 6000, length.out=10))
    
    library(lattice)
    image(b0x, b1y, q2z, col = colores(length(curvas)-1), breaks = sort(curvas),
         xlim = c(-8, 8), ylim = c(0, 8), axes = F, useRaster = T)
    contour(b0x, b1y, q2z, levels = curvas, add = TRUE, col = 'black', drawlabels = F)
    points(beta0, beta1, pch = 3, cex = 2, col = 'green2', lwd = 3)
    axis(1, seq(-8, 8, by = 2))
    axis(2, seq(0, 8, by = 1), las = 2)
  })
})