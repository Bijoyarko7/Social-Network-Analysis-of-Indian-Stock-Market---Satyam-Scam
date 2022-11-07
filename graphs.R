banks<-read.csv(file="DATA/loans_bankshare.csv",head=TRUE,sep=",")

#Summarise the data just read
#summary(banks)

grange <- range(0, banks$PSUbanksshare, banks$Pvtbanksshare, banks$Forbanksshare)

#colors <- c("blue","red","black")

pdf(paste0("../DOC/GRAPHS/loans_bankshare.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(banks$PSUbanksshare, type="l", lty=1, col="black", lwd=2, ylim=grange, axes=FALSE, ann=FALSE)

axis(1, at=1:25, lab=c(1991:2015))

axis(2, las=1, at=10*0:grange[2])

box()

lines(banks$Pvtbanksshare, lty=2, col="black", lwd=2)

lines(banks$Forbanksshare, lty=3, col="black", lwd=2)

#summary(as.numeric(banks$Forbanksshare))

#title(main="Evolution of shares of bank groups", col.main="black", font.main=4)

#title(xlab="Years")
title(ylab="Fraction of total banking-sector loans (%)")

grid()

plot.new()

legend("bottom",
        "center",
        #10,
       #grange[2],
       legend=c("PSU banks","Private banks", "Foreign banks"),
       cex=1.0,
       #col=c("blue","red","black"),
       bty = 'n',
       lty=c(1,2,3),
       lwd = c(3,3,3),
       ncol = 3)



dev.off()
###################################################################


hhsavings <-read.csv(file="DATA/hhsavings.csv",head=TRUE,sep=",")

hhsavings1 <- hhsavings[   , -1]

years <- hhsavings$years


pdf(paste0("../DOC/GRAPHS/hhsavings.pdf"),
    height = 10, width = 10)

layout(rbind(1,2), heights = c(7, 1))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

#par(mar = c(5.1,5.1,1.5,2), mgp = c(3,1,0), las = 0)

#box(lty= "solid")
    #par(mar = c(5,5,0,3), mgp = c(3.5,1,0), las = 1)
    
    savingsShares <- barplot(t(hhsavings1),
                             names.arg =years,
                             cex.names = 0.9,
                             col = c("gray",
                                 "black",
                                 "ghostwhite",
                                 "gray56",
                                "lightgray",
                                 "dimgrey"),
                             #xlab = "Years",
                             ylab =
                                 "Fraction of total household savings (%)",
                             width = 1,
                             cex.axis= 0.9,
                             space = 1
                                        #ylim = c(0, 100)
                             #xaxs = "i"
                             )

axis(1, at=savingsShares, lab=c(1991:2013))

box(lty = "solid")

plot.new()

legend("bottom",
        "center",
        #10,
       #grange[2],
       legend=c("Currency","Deposits", "Shares & Debentures", "Claims on government"       , "Insurance Funds", "Provident & Pension Funds"),
       cex=1.0,
       fill = c("gray",
                 "black",
                 "ghostwhite",
                 "gray56",
                 "lightgray",
                 "dimgrey"),
       bty = 'n',
       ncol = 2)
    
    dev.off()


####################################################################

asstcreddepo<-read.csv(file="DATA/assetcreditdeposit_grrate.csv",head=TRUE,sep=",")


grange <- range(-5, asstcreddepo$Credit, asstcreddepo$Deposits)


pdf(paste0("../DOC/GRAPHS/creditdeposit_grrate.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(asstcreddepo$Credit, type="l", lty=1, col="black", lwd=2, ylim=grange, ann=FALSE, xaxt="n")

abline(h=0, lty=1, col="black")

axis(1, at=1:24, lab=c(1992:2015))

#axis(2, las=1, at=c(-5, 35)

box()


lines(asstcreddepo$Deposits, lty=2, col="black", lwd=2)


#title(xlab="Years")
title(ylab="Year on year growth rate (%)")

grid()

plot.new()

legend("bottom",
        "center",
        #10,
       #grange[2],
       legend=c("Loans", "Deposits"),
       cex=1.0,
       #col=c("blue","black"),
       bty = 'n',
       lty=c(1,2),
       lwd = c(3,3),
       ncol = 2)



dev.off()
######################################################################
asstcreddepo<-read.csv(file="DATA/assetcreditdeposit_grrate.csv",head=TRUE,sep=",")


grange <- range(0, asstcreddepo$Assets)

colors <- c("black")

pdf(paste0("../DOC/GRAPHS/asset_grrate.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(asstcreddepo$Assets, type="l", lty=1, col=colors[1], lwd=2, ylim=grange, axes=FALSE, ann=FALSE)


axis(1, at=1:24, lab=c(1992:2015))

axis(2, las=1, at=10*0:grange[2])

box()


#title(xlab="Years")
title(ylab="Year on year growth rate (%)")

grid()

plot.new()

legend("center",
        #10,
       #grange[2],
       legend=c("Banking sector assets"),
       cex=1.0,
       col=c("black"),
       bty = 'n',
       lty=c(1),
       lwd = c(3),
       ncol = 1)



dev.off()
####################################################################
creddepo2gdp<-read.csv(file="DATA/creditdeposit_gdp.csv",head=TRUE,sep=",")


grange <- range(10, creddepo2gdp$"Credit_GDP", creddepo2gdp$"Deposit_GDP")

#colors <- c("blue","black")

pdf(paste0("../DOC/GRAPHS/creditdeposit_gdp.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(creddepo2gdp$"Credit_GDP", type="l", lty=1, col="black", lwd=2, ylim=grange, ann=FALSE, xaxt="n")

axis(1, at=1:25, lab=c(1991:2015))

#axis(2, las=1, at=10*0:grange[2])

box()


lines(creddepo2gdp$"Deposit_GDP", lty=2, col="black", lwd=2)


#title(xlab="Years")
title(ylab="Fraction of GDP (%)")

grid()

plot.new()

legend("bottom",
        "center",
        #10,
       #grange[2],
       legend=c("Loans", "Deposits"),
       cex=1.0,
       #col=c("blue","black"),
       bty = 'n',
       lty=c(1,2),
       lwd = c(3,3),
       ncol = 2)



dev.off()

######################################################################
creditgrowth<-read.csv(file="DATA/creditgrowth_banks.csv",head=TRUE,sep=",")
#head(creditgrowth)

grange <- range(-10, creditgrowth$"PSU.Banks", creditgrowth$"Private.Banks", creditgrowth$"Foreign.Banks")

colors <- c("black")

pdf(paste0("../DOC/GRAPHS/creditgrowth_banks.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(creditgrowth$"PSU.Banks", type="l", lty=1, col=colors[1], lwd=2, ylim=grange, ann=FALSE, xaxt="n")

axis(1, at=1:24, lab=c(1992:2015))

#axis(2, las=1, at=10*0:max_y)

box()

lines(creditgrowth$"Private.Banks", lty=2, col=colors[1], lwd=2)

lines(creditgrowth$"Foreign.Banks", lty=3, col=colors[1], lwd=2)


#title(xlab="Years")
title(ylab="Year on year growth rate of loans (%)")

grid()

plot.new()

legend("center",
        #10,
       #grange[2],
       legend=c("PSU banks","Private banks", "Foreign banks"),
       cex=1.0,
       #col=c("blue","red","black"),
       bty = 'n',
       lty=c(1,2,3),
       lwd = c(3,3,3),
       ncol = 3)



dev.off()

####################################################################


npagrowth<-read.csv(file="DATA/npa_grrate.csv",head=TRUE,sep=",")

grange <- range(-20, npagrowth$"Gross_NPAs", npagrowth$"Net_NPAs")

colors <- c("black")

pdf(paste0("../DOC/GRAPHS/npa_grrate.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(npagrowth$"Gross_NPAs", type="l", lty=1, col=colors[1], lwd=2, ylim=grange, ann=FALSE, xaxt="n")

abline(h=0, lty=1, col="black")

axis(1, at=1:19, lab=c(1997:2015))

#axis(2, las=1, at=10*0:grange[2])

box()


lines(npagrowth$"Net_NPAs", lty=2, col=colors[1], lwd=2)


#title(xlab="Years")
title(ylab="Year on year growth rate (%)")

grid()

plot.new()

legend("bottom",
        "center",
        #10,
       #grange[2],
       legend=c("Gross NPAs", "Net NPAs"),
       cex=1.0,
       #col=c("blue","black"),
       bty = 'n',
       lty=c(1,2),
       lwd = c(3,3),
       ncol = 2)



dev.off()

#####################################################################
npashare<-read.csv(file="DATA/npa_bankshare.csv",head=TRUE,sep=",")


grange <- range(0, npashare$"PSU.Banks", npashare$"Private.Banks")

colors <- c("black")

pdf(paste0("../DOC/GRAPHS/npa_bankshare.pdf"),
    height = 10, width = 10)

#png(filename="graphs.png",
    #bg="white",
    #height = 600,
    #width = 800,
    #)

layout(rbind(1,2), heights = c(7, 1))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
      #mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)

plot(npashare$"PSU.Banks", type="l", lty=1, col=colors[1], lwd=2, ylim=grange, axes=FALSE, ann=FALSE)

axis(1, at=1:19, lab=c(1996:2014))

axis(2, las=1, at=5*0:grange[2])

box()

lines(npashare$"Private.Banks", lty=2, col=colors[1], lwd=2)


#title(xlab="Years")
title(ylab="Fraction of banking-sector gross NPAs (%)")

grid()

plot.new()

legend("bottom",
       "center",
        #10,
       #grange[2],
       legend=c("PSU banks","Private banks"),
       cex=1.0,
       #col=c("blue","red"),
       bty = 'n',
       lty=c(1,2),
       lwd = c(3,3),
       ncol = 2)



dev.off()
