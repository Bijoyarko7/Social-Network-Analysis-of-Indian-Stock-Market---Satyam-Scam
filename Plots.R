
v1 <- c(2008-12-17,2009-01-07)

# Define the labels of tick marks
v2 <- c("Maytas deal collapsed","Fraud became common knowledge")
plot(modularity,type="l",ylab="count",xlab="",col="blue",col.main="red",cex.main=1.5,las=3,xaxt="n")
axis(1, modularity$Date, format(modularity$Date, "%b %Y"), cex.axis = .6)
abline(v=as.Date("2009-01-01"),col= "black")
abline(v=as.Date("2008-12-01"),col= "black")
abline(v=as.Date("2009-04-13"),col= "black")
axis(side = 1, 
     at = v1, 
     labels = v2,
     tck=-.05)
text(as.Date("2008-12-16"),0.35, "Maytas deal collapsed", col = "RED", adj = c(0.4,0.2))
text(as.Date("2009-01-06"),0.35, "Raju's confession", col = "RED", adj = c(0.4,0.2))
