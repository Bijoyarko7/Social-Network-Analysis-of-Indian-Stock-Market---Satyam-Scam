library(lubridate)
library(readr)
library(ggplot2)
plot59 <- read_csv("plot59.csv", col_types = cols(`Fiscal Years` = col_date(format = "%Y")))
pdf(paste0("C:\\Users\\User\\OneDrive\\Desktop\\THESIS related data work\\FINAL\\Thesis Write-up\\CARs_plot.pdf"),
    height = 8, width = 12)

#png(filename="graphs.png",
#bg="white",
#height = 600,
#width = 800,
#)

#layout(rbind(1,2), heights = c(6, 2))

#par(pty = "m", plt = c(0.1, 1, 0, 0.85),
#mgp = c(3, 1, 0),
#omd = c(0.01, 0.94, 0.01, 0.94))
event_CARs.3 <- event_CARs.2[event_CARs.2$Company %in% c("I3","CMC","First","HCL","Hexa","Infy","MP","Oracle","Sonata","TCS","TechM","W"),]

#par(mar = c(1,4,1,2), mgp = c(3,1,0), las = 0)#Plotting_The_Timeseries_Graph
plot(event_CARs.3$CARs,xlab = "Company",ylim = c(-10,15),col = ifelse(event_CARs.3$groupdummy == "1", "blue", "red"),ylab = "Cumulative Abnormal Returns",pch=16,type="p",xaxt = "n",cex=1.5)
axis(1,at=c(1:12),labels = c("I3","CMC","First","HCL","Hexa","Infy","MP","Oracle","Sonata","TCS","TechM","W"))
#type = "l",col="blue",col.main="red",cex.main=1.5, ylim = c(-0.5,4),lwd=1.5)+lines(DE_ratio_import$Year,DE_ratio_import$TCS, xaxt = "n", type = "l",yaxt="n",col="red",col.main="red",lwd=1.5,cex.main=1.5,las=3)+lines(DE_ratio_import$Year,DE_ratio_import$Infy, xaxt = "n", type = "l",yaxt="n",col="black",col.main="red",cex.main=1.5,las=3,lwd=1.5)
legend("topright",legend=c("Good","Bad"),cex=1.0,col=c("blue","red"),bty = 'n',lty=1,lwd = 1.5,horiz=FALSE)

#plot(Melanoma$age, Melanoma$time, 
 #    main = "Survival Time from Malignant Melanoma",
  #   xlab = "Age (in years)",
   #  ylab = "Survival Time (in days)",
    # col = ifelse(Melanoma$sex == "1", "blue", "red"))
#legend("topleft", 
 #      pch = c(1, 1), 
  #     c("Female", "Male"), 
   #    col = c("red", "blue")) 
CARS_good <- subset(event_CARs.3, groupdummy == "1")
CARS_bad <- subset(event_CARs.3, groupdummy == "0")
abline(lm(CARS_bad$CARs ~ CARS_bad$CARs), col = "red")
abline(lm(CARS_good$CARs ~ CARS_good$CARs), col = "blue")
#ggsave(filename="C:\\Users\\User\\OneDrive\\Desktop\\THESIS related data work\\FINAL\\R files\\Social-Network-Analysis-of-Indian-Stock-Market---Satyam-Scam\\CARs_plot.pdf", plot=p, width=12, height=8, units="in")
dev.off()
graphics.off()
