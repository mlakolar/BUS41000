library(shiny)
library(ggplot2)

rnormcor <- function(x,rho) rnorm(1,rho*x,sqrt(1-rho^2))

shinyServer(function(input,output){
  output$distPlot<-renderPlot({
      n     <- input$obs             # length of vector
      rho   <- input$rho             # desired correlation = cos(angle)
      if(rho == 1)
        rho = 0.9999
      end
      theta <- acos(rho)             # corresponding angle
      x1    <- rnorm(n, 0, 1)        # fixed given data
      x2    <- rnorm(n, 0, 1)        # new random data
      x2sc  <- sqrt(sum(x2^2))
      X     <- cbind(x1, x2)         # matrix
      Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
      
      Id   <- diag(n)                               # identity matrix
      Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
      P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
      x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
      Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
      Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
      
      x2 <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
      x2 <- x2 * x2sc
      
      p<-qplot(x1,x2,alpha=0.85)+geom_smooth(method="lm",se=F,size=1.1)+theme(legend.position = "none")
      p<-p+labs(x="x1",y="x2",title=paste("Scatter plot with correlation =",rho,sep=" "))
      plot(p)
  })
})

