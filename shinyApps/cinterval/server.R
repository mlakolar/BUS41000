library(shiny)

shinyServer(function(input, output) {
  
  compute.ci <- reactive({
    #    set.seed(1)
    if (input$dist=="rpois") {
      pop <-  do.call(input$dist, list(n=1000, lambda=1))
    } else if (input$dist == "rbinom") {
      pop <-  do.call(input$dist, list(n=10000, size=1, 0.6))
    }
    else {
      pop <-  do.call(input$dist, list(n=1000))
    }
    nobs <- input$nobs
    nsamples <- input$nsamples
    alpha <- input$alpha
    
    mu <- mean(pop)
    crit1 <- qnorm(p = alpha/2, mean = 0, sd = 1)
    crit2 <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
    xbars <- numeric(length = nsamples)
    sderr <- numeric(length = nsamples)
    ci.upp <- numeric(length = nsamples)
    ci.low <- numeric(length = nsamples)
    within <- numeric(length = nsamples)*0
    
    for (i in 1:nsamples) {
      s <- sample(pop, size=nobs)
      xbars[i] <- mean(s)
      sderr[i] <- sd(s)/sqrt(length(s)) 
      ci.upp[i] <- xbars[i] + crit2*sderr[i]
      ci.low[i] <- xbars[i] - crit2*sderr[i]
      if (mu >= ci.low[i] & mu <= ci.upp[i]) {
        within[i]=1
      }
    }
    return (list(fun=input$dist, pop=pop, xbars=xbars, sderr=sderr, 
                 within=within, ci.upp=ci.upp, ci.low=ci.low,
                 crit1=crit1, crit2=crit2, mu=mu, nobs=nobs, nsamples=nsamples,
                 alpha=alpha, s=s))
  })
  
  output$plot <- renderPlot({
    distname <- switch(input$dist,
                       rexp = "Exponential distribution",
                       rnorm = "Normal distribution",
                       rlnorm = "Log-normal distribution",
                       runif = "Uniform distribution",
                       rpois = "Poisson distribution",
                       rbinom = "Bernoulli distribution")
    # Unload data
    r <- compute.ci()
    alpha <- r$alpha
    xbars <-r$xbars
    mu <- r$mu
    within <- r$within
    crit1 <- r$crit1
    crit2 <- r$crit2
    ci.upp <- r$ci.upp
    ci.low <- r$ci.low
    nobs <- r$nobs
    s <- r$s
    nsamples <- r$nsamples
    sderr <- r$sderr
    # Plot results
    cb <- c("#000000",  "#999999",  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    names(cb) <- c("black", "grey", "orange", "turquoise", "green", "yellow", "blue", "red", "pink")
    
    layout(matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,3,3), nrow = 7, ncol=2, byrow = TRUE))
    par(cex=1.1)
    scaling=par()$cex
    # Panel 1
    hist(s, xlab="X",
         main=paste0("Each sample consists of ", nobs, 
                     " observations \n", "from the ", distname), col=cb["red"])
    # Panel 2
    x=seq(-5, 5, len=100)
    y <- dnorm(x)
    total.within <- sum(within)
    prop.within <- total.within/nsamples*100
    plot(x, y, t="l", ylab="Probability density", 
         xlab=expression(paste("Z scores of sample means ", 
                               bar(X))), xaxs="i", yaxs="i", bty="l", xaxt="n", xpd=NA)
    mtext(3, line = 3, text = paste0(total.within, " out of ", 
                                     nsamples, " (", format(prop.within, dig=2), "%) of the sample means"), cex=scaling)
    mtext(3, line = 1.5, 
          text = expression(paste(bar(X), 
                                  " are within ", Z[alpha/2]%.%sigma, " of the population mean ", mu)), cex=scaling)
    axis(1, at=c(-5, crit1, 0, crit2, 5), 
         lab=c(expression(-5*sigma), bquote(.(format(crit1, dig=3))*sigma), expression(mu), 
               bquote(.(format(crit2, dig=3))*sigma), expression(5*sigma)))
    abline(v=0, lwd=2, lty=2, col="black")
    # Lower tail
    coord.x=c(-5, seq(-5, crit1, 0.01), crit1)
    coord.y=c(0, dnorm(seq(-5, crit1, 0.01)), 0) 
    polygon(coord.x, coord.y, col=cb['red'])
    # Upper tail
    coord.x=c(crit2, seq(crit2, 5, 0.01), 5)
    coord.y=c(0, dnorm(seq(crit2, 5,  0.01)), 0) 
    polygon(coord.x, coord.y, col=cb['red'])
    # Upper tail
    # 95% region
    coord.x=c(crit1, seq(crit1, crit2, 0.01), crit2)
    coord.y=c(0, dnorm(seq(crit1, crit2,  0.01)), 0) 
    polygon(coord.x, coord.y, col=cb['blue'])
    
    arrows(x1=mean(c(crit2, 5)), y1=0.01, x0=mean(c(crit2, 5))+0.1, y0=0.08, 
           length=0.13, lwd=2, col=cb["red"])
    text(x=mean(c(crit2, 5))+0.1, y=0.1, 
         substitute(paste(alpha/2==v), 
                    list(v=format(alpha/2, dig=3))), col=cb["red"], xpd=NA)
    # Lower tail
    arrows(x1=mean(c(crit1, -5)), y1=0.01, x0=mean(c(crit1, -5))-0.1, y0=0.08, 
           length=0.13, lwd=2, col=cb["red"])
    text(x=mean(c(crit1, -5))-0.1, y=0.1, 
         substitute(paste(alpha/2==v), list(v=format(alpha/2, dig=3))), col=cb["red"], xpd=NA)
    text(x=crit1, y=max(y), expression(-Z[alpha/2]), xpd=NA, col=cb["blue"], pos=3, offset=0)
    abline(v=crit1, lwd=2, lty=2, col=cb["blue"])
    text(x=crit2, y=max(y), expression(Z[alpha/2]), xpd=NA, col=cb["blue"], pos=3, offset=0)
    abline(v=crit2, lwd=2, lty=2, col=cb["blue"])
    abline(v=0, lwd=2, lty=2)
    # Add sample means
    suppressWarnings(rug ((xbars-mu)/sderr, side = 1, lwd=2, ticksize = 0.05))
    
    # Panel c
    nsegs <- min(nsamples, 100)
    extreme.x <- max(abs(c(ci.low[1:nsegs], ci.upp[1:nsegs]))-mu)
    plot(ci.upp[1:nsegs], 1:nsegs, t="n", xaxt="n", ylab="First 100 samples", 
         xlim=c(-extreme.x+mu, extreme.x+mu),
         xlab=expression(paste("Sample means (", bar(X), ")")), 
         main=paste0(format((1-alpha)*100, dig=2), "% confidence intervals of samples of size ", nobs,
                     "\n ", total.within, " out of ", nsamples, " confidence intervals contain the population mean"), 
         col=ifelse(within[1:nsegs]==0, cb["red"], "black"))
    axis(1, at=mu, label=expression(mu))
    abline(v=mu, col=cb["black"], lwd=1)
    segments(x0=ci.low[1:nsegs], x1=ci.upp[1:nsegs], y0=1:nsegs, y1=1:nsegs, 
             col=ifelse(within==0, cb["red"], cb["blue"]), lwd=2)
  })
})