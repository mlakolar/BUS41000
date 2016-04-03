library(shiny)
library(MASS)
library(maptools)

shinyServer(function(input,output){
    
  n   = reactive( input$obs )
  rho = reactive( input$rho )
  mx  = reactive( input$meanA )
  my  = reactive( input$meanB )
  sx  = reactive( input$stdA )
  sy  = reactive( input$stdB )
  sxy = reactive( sx()*sy()*rho() )
  w   = reactive( input$w ) 
      
  data = reactive( mvrnorm(n(), c(mx(), my()), matrix(c(sx()^2, sxy(), sxy(), sy()^2), nrow=2) ) )
  port = reactive({
      x = data()
      return( x[,1]*w()+(1-w())*x[,2] )
  })
  
  output$outScatter<-renderPlot({
      x = data()
      plot(x, type='p', xlab="Asset A", ylab="Asset B")
      abline(v=mean(x[,1]), lw=2, col="red")
      abline(h=mean(x[,2]), lw=2, col="red")
      #text(x=x[,1], y=x[,2], labels=format(port(), digits=2), pos=1)
      if(input$showLabels) {
        pointLabel(x[,1], x[,2], labels=format(port(), digits=2), cex=1)
      }
  })
  
  xmin = reactive( min(data()[,1], data()[,2], port()) - 2*max(mx(), my()) ) 
  xmax = reactive( max(data()[,1], data()[,2], port()) + 2*max(mx(), my()) ) 
  
  output$outHist<-renderPlot({
      x = data()
      par(mfrow=c(1,3))
      hist(port(), breaks=input$nbins, main="Returns on Portfolio", xlab="Returns", xlim=c(xmin(), xmax()))
      hist(x[,1], breaks=input$nbins, main="Returns on Asset A", xlab="Returns", xlim=c(xmin(), xmax()))
      hist(x[,2], breaks=input$nbins, main="Returns on Asset B", xlab="Returns", xlim=c(xmin(), xmax()))
  })
  
  output$outScatter1 <- renderPlot({
      df = data.frame(A=data()[,1], B=data()[,2], Port=port())
      mean.mfr = sapply(df, mean)       # compute mean for each column
      sd.mfr = sapply(df, sd)           # compute standard deviation for each column

      # create a scatter plot using mean and standard deviations
      plot(sd.mfr, mean.mfr, type="p",  main="", xlab = "std. dev.", ylab = "mean",
            xlim=c(min(sd.mfr), max(sd.mfr)), 
            ylim=c(min(mean.mfr), max(mean.mfr)))
      pointLabel(sd.mfr, mean.mfr, labels=c("Asset A", "Asset B", "Port"), cex=1)
  })  
  
})

