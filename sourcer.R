# load packages

# library( tidyverse )


# RMD settings



panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use="pairwise.complete.obs")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    
    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
    
    text(0.5, 0.5, txt, cex = 2 )
    text(.7, .8, Signif, cex=3, col=2)
}


panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
  cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  points(x, y, pch = 19, col = gray(0.5,0.5), 
         bg = bg, cex = 1.7)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
      col = col.smooth, lwd=2, ...)
}



draw_ci <- function( b1, se )
{
	upper.ci <- b1 + 1.96*se 
	lower.ci <- b1 - 1.96*se

	plot( c(lower.ci,upper.ci), c(1,1), 
	      xlim=c(lower.ci-0.5,1), ylim=c(0,3),
	      xlab="", ylab="", axes=F, bty="n",
	      type="l", lwd=3, col="darkorange", 
	      main="Model 1" )
	points( b1, 1, col="darkorange", pch=19, cex=3 )
	text( b1, 1, b1, pos=3, col="gray30", cex=1.5, offset=1 )
	text( c(lower.ci,upper.ci), 1, round(c(lower.ci,upper.ci),2), 
	      pos=c(2,4), cex=0.8, col="gray40")
	abline( v=0, lty=2, lwd=2, col="gray40" )
	axis( side=1, at=0, labels="B1=0" )
}


jplot <- function( x1, x2, lab1="", lab2="", draw.line=T, ... )
{

	plot( x1, x2,
	      pch=19, 
	      col=gray(0.6, alpha = 0.2), 
	      cex=3.5,  
	      bty = "n",
	      xlab=lab1, 
	      ylab=lab2, cex.lab=1.5,
        ... )

	if( draw.line==T ){ 
		ok <- is.finite(x1) & is.finite(x2)
		lines( lowess(x2[ok]~x1[ok]), col="red", lwd=3 ) }

}

