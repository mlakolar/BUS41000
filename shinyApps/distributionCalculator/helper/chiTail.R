chiTail <-
function(L=NULL,U=NULL,M=NULL, df = 10, curveColor=1, border=1, col="#569BBD", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
	#if(U <= 30){xlim <- c(0,30)}
	#if(U > 30){xlim <- c(0,U+0.01*U)}
	temp <- diff(range(xlim))
	x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
	y    <- dchisq(x, df)
	ylim <- range(c(0,y))
	plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")

	if(!is.null(L)){
	    these <- (x <= L)
	    X <- c(x[these][1], x[these], rev(x[these])[1])
	    Y <- c(0, y[these], 0)
	    axis(1, at = c(0,L,xlim[2]), 
	         label = c(0,round(L,4),round(xlim[2],4)))
	    polygon(X, Y, border=border, col=col)
	}
	if(!is.null(U)){
	    these <- (x >= U)
	    X <- c(x[these][1], x[these], rev(x[these])[1])
	    Y <- c(0, y[these], 0)
	    axis(1, at = c(0,U,xlim[2]), 
	         label = c(0,round(U,4),round(xlim[2],4)))
	    polygon(X, Y, border=border, col=col)
	}
	if(all(!is.null(M[1:2]))){
	    these <- (x >= M[1] & x <= M[2])
	    X <- c(x[these][1], x[these], rev(x[these])[1])
	    Y <- c(0, y[these], 0)
	    axis(1, at = c(0,M[1],M[2],xlim[2]), 
	         label = c(0,round(M[1],4),round(M[2],4),round(xlim[2],4)))
	    polygon(X, Y, border=border, col=col)
	}
	abline(h=0)
}

chiTailCDF <-
    function(L=NULL,U=NULL,M=NULL, df = 10, curveColor=1, border=1, col="#569BBD", xlim=NULL, ylim=NULL, xlab='', ylab='', detail=999){
        #if(U <= 30){xlim <- c(0,30)}
        #if(U > 30){xlim <- c(0,U+0.01*U)}
        temp <- diff(range(xlim))
        x    <- seq(xlim[1] - temp/4, xlim[2] + temp/4, length.out=detail)
        y    <- pchisq(x, df)
        ylim <- c(0,1)
        plot(x, y, type='l', xlim=xlim, ylim=ylim, axes=FALSE, col=curveColor, xlab = "", ylab = "")
        abline(h=1, col=curveColor, lt=2)

        if(!is.null(L[1])){
            y1=pchisq(L, df)
            lines(x=c(L,L), y=c(0,y1), lt=2, col=col)
            lines(x=c(xlim[1] - temp/4,L), y=c(y1,y1), lt=2, col=col)
            
            lines(x=c(xlim[1],xlim[1]),y=c(0,y1),lt=3,col=col)
            axis(1, at = c(0,L,xlim[2]), 
                 label = c(0,round(L,4),round(xlim[2],4)))
        }
        if(!is.null(U[1])){
            y1=pchisq(U, df)
            lines(x=c(U,U), y=c(0,y1), lt=2, col=col)
            lines(x=c(xlim[1] - temp/4,U), y=c(y1,y1), lt=2, col=col)
            
            lines(x=c(xlim[1],xlim[1]),y=c(y1,1),lt=3,col=col)
            axis(1, at = c(0,U,xlim[2]), 
                 label = c(0,round(U,4),round(xlim[2],4)))
        }
        if(all(!is.null(M[1:2]))){
            L=M[1]
            U=M[2]
            
            y1=pchisq(L, df)
            lines(x=c(L,L), y=c(0,y1), lt=2, col=col)
            lines(x=c(xlim[1] - temp/4,L), y=c(y1,y1), lt=2, col=col)
            
            y2=pchisq(U, df)
            lines(x=c(U,U), y=c(0,y2), lt=2, col=col)
            lines(x=c(xlim[1] - temp/4,U), y=c(y2,y2), lt=2, col=col)
            
            lines(x=c(xlim[1],xlim[1]),y=c(y1,y2),lt=3,col=col)
            axis(1, at = c(0,M[1],M[2],xlim[2]), 
                 label = c(0,round(M[1],4),round(M[2],4),round(xlim[2],4)))
        }        
        buildAxis(2, c(y,0), n=3, nMax=3, cex.axis=1)
        abline(h=0)
    }
