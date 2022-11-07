#Price volatility plots with event timeline

TCS_prices$dailyvol<- rollapply(TCS_prices$`Sq. returns`,
                               
                               22,
                               
                               sd,
                               
                               na.rm=TRUE,
                               
                               fill=NA,
                               
                               align='right'
                               
)
TCS_prices$month<-month(TCS_prices$Date)

monthly <- aggregate(x = Satyam_Prices$`Sq. returns`,
                     
                     by=list(unique.values = Satyam_Prices$Dates),           
                     
                     FUN = mean)

plot(Satyam_Prices,type="l",ylab="stock price",xlab="",col="blue",col.main="red",cex.main=1.5,las=3)
abline(v=as.Date("2009-01-06"),col= "black")
abline(v=as.Date("2008-12-16"),col= "black")
text(as.Date("2008-12-16"),7000, "Maytas deal", col = "red", adj = c(1,0.6),cex=0.7)
text(as.Date("2009-01-06"),6000, "Raju's confession", col = "red", adj = c(0,0.2),cex=0.7)
lines(tcs_to_plot,type="l",col="red",col.main="red",cex.main=1.5,las=3)
