##Calculating annualised volatility

NiftyIT_price$Date<-month(NiftyIT_price$Date)

##Calculating returns INR/USD using daily data
NiftyIT_price$return <- c(0, diff(log(NiftyIT_price$Close)))



##Calculating rolling annualised vol of daily returns

NiftyIT_price$daily.vol <- rollapply(NiftyIT_price$return,
                                     
                                     22,
                                     
                                     sd,
                                     
                                     na.rm=TRUE,
                                     
                                     fill=NA,
                                     
                                     align='right'
                                     
)


NiftyIT_price$ann.vol <- (NiftyIT_price$daily.vol)*sqrt(252)*100



##Converting into monthly data for plotting convenience
##Convert into monthly data by taking the average

monthly <- aggregate(x = NiftyIT_price$ann.vol,
                     
                     by=list(unique.values = NiftyIT_price$Date),           
                     
                     FUN = mean)

plot(PLOTs$`Operating cost to average total assets`,type="l",ylab="Volatility",col="blue",col.main="red",cex.main=1.5,las=3)
