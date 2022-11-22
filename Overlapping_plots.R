library(lubridate)
library(readr)
plot59 <- read_csv("plot59.csv", col_types = cols(`Fiscal Years` = col_date(format = "%Y")))
pdf(paste0("C:\\Users\\User\\OneDrive\\Desktop\\THESIS related data work\\FINAL\\R files\\Social-Network-Analysis-of-Indian-Stock-Market---Satyam-Scam\\CARs_plot.pdf"),
    height = 5, width = 10)

layout(rbind(1,2), heights = c(6, 2))

par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)#Plotting_The_Timeseries_Graph
p<- plot(DE_ratio_import$Year,DE_ratio_import$W,xlab = "",ylab = "In percentage terms", type = "l",col="blue",col.main="red",cex.main=1.5, ylim = c(-0.5,4),lwd=1.5)+lines(DE_ratio_import$Year,DE_ratio_import$TCS, xaxt = "n", type = "l",yaxt="n",col="red",col.main="red",lwd=1.5,cex.main=1.5,las=3)+lines(DE_ratio_import$Year,DE_ratio_import$Infy, xaxt = "n", type = "l",yaxt="n",col="black",col.main="red",cex.main=1.5,las=3,lwd=1.5)
legend("topright",legend=c(),cex=1.0,col=c("blue","red","black"),bty = 'n',lty=1,lwd = 1.5,horiz=FALSE)
dev.off()
pdf( "de-1.pdf", width = 12, height = 8 )
p<-plot(DE_ratio_import$Year,DE_ratio_import$W,xlab = "",ylab = "In proportion terms", type = "l",col="blue",col.main="red",cex.main=1.5, ylim = c(-1,5),lwd=1.5)
lines(DE_ratio_import$Year,DE_ratio_import$TCS, xaxt = "n", type = "l",yaxt="n",col="red",col.main="red",lwd=1.5,cex.main=1.5,las=3)
lines(DE_ratio_import$Year,DE_ratio_import$Infy, xaxt = "n", type = "l",yaxt="n",col="black",col.main="red",cex.main=1.5,las=3,lwd=1.5)
lines(Net_Margin_import$Year,Net_Margin_import$First, xaxt = "n", type = "l",yaxt="n",col="green",col.main="red",cex.main=1.5,las=3,lwd=1.5)
lines(Net_Margin_import$Year,Net_Margin_import$Hexa, xaxt = "n", type = "l",yaxt="n",col="yellow",col.main="red",cex.main=1.5,las=3,lwd=1.5)
legend("topright",legend=c("W","TCS","Infy"),cex=1.0,col=c("blue","red","black"),bty = 'n',lty=1,lwd = 1.5,horiz=TRUE)

print(p)

dev.off()
graphics.off()
#lines(plot55$`Fiscal Years`, plot55$Private, xaxt = "n", type = "l",yaxt="n",col="#66FF00",col.main="red",lwd=2,cex.main=1.5,las=3)
#lines(plot55$`Fiscal Years`, plot55$Foreign, xaxt = "n", type = "l",yaxt="n",col="#CC6600",col.main="red",lwd=2,cex.main=1.5,las=3)
#Fixing_axis_type
axis(1,plot61$`Fiscal Years`, format(plot61$`Fiscal Years`, "%Y"), cex.axis = .7)
box()
#Adding_legend
plot.new()


