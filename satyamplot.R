## Setup up coordinate system (with x==y aspect ratio):
plot(c(-2,3), c(-1,5), type = "n", xlab="x", ylab="y", asp = 1)
## the x- and y-axis, and an integer grid
abline(h=0, v=0, col = "gray60")
text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
abline(h = -1:5, v = -2:3, col = "lightgray", lty=3)
abline(a=1, b=2, col = 2)
text(1,3, "abline( 1, 2 )", col=2, adj=c(-.1,-.1))
plot(Niftyvol,type="l",ylab="Volatility",xlab="",col="blue",col.main="red",cex.main=1.5,las=3)
abline(v=as.Date("2009-01-06"),col= "black")
abline(v=as.Date("2008-12-16"),col= "black")
text(as.Date("2008-12-16"),80, "Maytas deal", col = "red", adj = c(1,0.6),cex=0.7)
text(as.Date("2009-01-06"),60, "Raju's confession", col = "red", adj = c(0,0.2),cex=0.7)
