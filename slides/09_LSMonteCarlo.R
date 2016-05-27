##########################################################################
##	Perform Monte Carlo demonstration of least squares lines to show
##	the sampling distribution. 
##########################################################################

rm(list=ls())

## Sample size
n <- 100

## set regression coefficients
beta.0 <- -3
beta.1 <- 2

sd.true <- 1

## set inputs (covariates)
x <- runif(n)-1/2



i <- 0
par(mfrow=c(1,1), cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
l <- b.estimates <- NULL

sd.b.1 <- sd.true/sqrt(sum((x - mean(x))^2))


## while the user wishes to continue
while(TRUE) {
    
    ## sample from the SLR with above sample.SLR function
    i <- i+1
    
    epsilon <- rnorm(length(x), sd=sd.true)
    y <- beta.0 + beta.1*x + epsilon
    fit <- lm(y ~ x)
    l <- cbind(l, predict(fit, type="response"))
    b.estimates <- rbind(b.estimates, coef(fit))
    
    ## plotting the results
    par(mfrow=c(1,2), mar=c(5,4,4,1))
    
    ## plot the points
    plot(x, y, cex=2, col=1, ylim=c(beta.0 - beta.1/2, beta.0 + beta.1/2), ylab="y", xlim=c(-1/2, 1/2))
    #xlim=c(-1/2, 1/2) because X is uniform(-1/2, 1/2)
    #change to col=i to have the points change color every time.
    title(paste("b_0 = ",  signif(coef(fit)[1],2), ", b_1 = ", signif(coef(fit)[2],2), sep=""))
    ## plot the accumulated regression lines
    #       matplot(x, l, lwd=2, col=1:i, type="l", lty=1, add=TRUE, ylab="y")
    
    ## plot just the current line and the true line
    #       matplot(x, l, lwd=2, col=1:i, type="l", lty=1, add=TRUE, ylab="y")
    abline(beta.0, beta.1, col="blue", lwd=2)
    abline(lm(y~x), col=1, lty=2, lwd=2)
    
    ## plot the density of the samples of b_1 so far
    slope.estimates <- round(10*b.estimates[,2],0)/10
    x.values <- sort(unique(slope.estimates))
    density <- table(slope.estimates)/(length(slope.estimates)/10)
    plot(x=seq(from=(beta.1 - 2*sd.true), to=(beta.1 + 2*sd.true),length.out=10) , col=0, ylim=c(0,max(density)), xlim=c(beta.1 - 3*sd.true, beta.1 + 3*sd.true), xlab="Slope Estimates", ylab="", main="Frequency", yaxt="n", cex.lab=1.25, xaxt="n")
    axis(side=1, at=c(beta.1 - 3*sd.true, beta.1 - 1*sd.true, beta.1, beta.1 + 1*sd.true, beta.1 + 3*sd.true), labels=c("-3 sd", "-1 sd", "true", "+1 sd", "+3 sd"))
    abline(v=beta.1, col="green", lty="longdash" , lwd=2)
    segments(x0 = x.values , y0=0, y1 = density, col="darkgrey", lty=1, lwd=5)
    segments(x0 = round(10*coef(fit)[2],0)/10 , y0=0, y1 = density[which(x.values == round(10*coef(fit)[2],0)/10  )], col=2, lty=1, lwd=5)
    
    
    ## continue sampling?
    if(readline("press RETURN to continue, q to stop: ") == "q") break
    
}


curve(dnorm(x, mean=beta.1, sd=sd.b.1), from=(beta.1 - 3*sd.true), to=(beta.1 + 3*sd.true), add=TRUE, col="blue", lwd=2)


