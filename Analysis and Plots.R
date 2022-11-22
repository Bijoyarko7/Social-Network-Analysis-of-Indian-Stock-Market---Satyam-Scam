######################################################################################################################################################
###Code Author: Bijoy Ratan Ghosh
###Start Date: May 2019
###Latest Iteration: November 2022
###Objective: Social Network Analysis, Data Analysis, Balance Sheet Analysis, Event Study CAR Analysis related to M.Phil. thesis.
###Dependency: CMIE Prowess Data
######################################################################################################################################################
######################################################################################################################################################

################################################################################################################################################################################################
############################Below segment of this code helps importing the necessary data and building Minimal spanning tree for social network analysis########################################
################################################################################################################################################################################################

###Necessary_packages

install.packages("readr")
library(readr)
# igraph to make a graph-object and visualize it
install.packages("igraph")
library(igraph)


###Importing files

Prices.oct_nov08 <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//prices csv//oct08.csv",show_col_types = FALSE)
Niftyoctnov <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work///FINAL/nifty prices/octnov08.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))


###Getting returns from raw price data

Prices.oct_nov08$I3ret <- c(0, 100*diff(log(Prices.oct_nov08$I3)))
Prices.oct_nov08$CMCret <- c(0, 100*diff(log(Prices.oct_nov08$CMC)))
Prices.oct_nov08$Firstret <- c(0, 100*diff(log(Prices.oct_nov08$First)))
Prices.oct_nov08$HCLret <- c(0, 100*diff(log(Prices.oct_nov08$HCL)))
Prices.oct_nov08$Hexaret <- c(0, 100*diff(log(Prices.oct_nov08$Hexa)))
Prices.oct_nov08$Infyret <- c(0, 100*diff(log(Prices.oct_nov08$Infy)))
Prices.oct_nov08$MPret <- c(0, 100*diff(log(Prices.oct_nov08$MP)))
Prices.oct_nov08$Oracleret <- c(0, 100*diff(log(Prices.oct_nov08$Oracle)))
Prices.oct_nov08$Satyamret <- c(0, 100*diff(log(Prices.oct_nov08$Satyam)))
Prices.oct_nov08$Sonataret <- c(0, 100*diff(log(Prices.oct_nov08$Sonata)))
Prices.oct_nov08$TCSret <- c(0, 100*diff(log(Prices.oct_nov08$TCS)))
Prices.oct_nov08$TechMret <- c(0, 100*diff(log(Prices.oct_nov08$TechM)))
Prices.oct_nov08$Wret <- c(0, 100*diff(log(Prices.oct_nov08$W)))
Niftyoctnov$Closeret <- c(0, 100*diff(log(Niftyoctnov$Close)))


###Running market model to get residual daily returns

I3.reg<- lm(Prices.oct_nov08$I3ret ~ Niftyoctnov$Closeret)
CMC.reg<- lm(Prices.oct_nov08$CMCret ~ Niftyoctnov$Closeret)
First.reg<- lm(Prices.oct_nov08$Firstret ~ Niftyoctnov$Closeret)
HCL.reg<- lm(Prices.oct_nov08$HCLret ~ Niftyoctnov$Closeret)
Hexa.reg<- lm(Prices.oct_nov08$Hexaret ~ Niftyoctnov$Closeret)
Infy.reg<- lm(Prices.oct_nov08$Infyret ~ Niftyoctnov$Closeret)
MP.reg<- lm(Prices.oct_nov08$MPret ~ Niftyoctnov$Closeret)
Oracle.reg<- lm(Prices.oct_nov08$Oracleret ~ Niftyoctnov$Closeret)
Satyam.reg<- lm(Prices.oct_nov08$Satyamret ~ Niftyoctnov$Closeret)
Sonata.reg<- lm(Prices.oct_nov08$Sonataret ~ Niftyoctnov$Closeret)
TCS.reg<- lm(Prices.oct_nov08$TCSret ~ Niftyoctnov$Closeret)
TechM.reg<- lm(Prices.oct_nov08$TechMret ~ Niftyoctnov$Closeret)
W.reg<- lm(Prices.oct_nov08$Wret ~ Niftyoctnov$Closeret)

beta.I3 <- I3.reg$coefficients[2]
beta.CMC <- CMC.reg$coefficients[2]
beta.First <- First.reg$coefficients[2]
beta.HCL <- HCL.reg$coefficients[2]
beta.Hexa <- Hexa.reg$coefficients[2]
beta.Infy <- Infy.reg$coefficients[2]
beta.MP <- MP.reg$coefficients[2]
beta.Oracle <- Oracle.reg$coefficients[2]
beta.Satyam <- Satyam.reg$coefficients[2]
beta.Sonata <- Sonata.reg$coefficients[2]
beta.TCS <- TCS.reg$coefficients[2]
beta.TechM <- TechM.reg$coefficients[2]
beta.W <- W.reg$coefficients[2]

const.I3 <- I3.reg$coefficients[1]
const.CMC <- CMC.reg$coefficients[1]
const.First <- First.reg$coefficients[1]
const.HCL <- HCL.reg$coefficients[1]
const.Hexa <- Hexa.reg$coefficients[1]
const.Infy <- Infy.reg$coefficients[1]
const.MP <- MP.reg$coefficients[1]
const.Oracle <- Oracle.reg$coefficients[1]
const.Satyam <- Satyam.reg$coefficients[1]
const.Sonata <- Sonata.reg$coefficients[1]
const.TCS <- TCS.reg$coefficients[1]
const.TechM <- TechM.reg$coefficients[1]
const.W <- W.reg$coefficients[1]

Prices.oct_nov08$I3 <- Prices.oct_nov08$I3ret - const.I3 - beta.I3*Niftyoctnov$Closeret
Prices.oct_nov08$CMC <- Prices.oct_nov08$CMCret - const.CMC - beta.CMC*Niftyoctnov$Closeret
Prices.oct_nov08$First <- Prices.oct_nov08$Firstret - const.First - beta.First*Niftyoctnov$Closeret
Prices.oct_nov08$HCL <- Prices.oct_nov08$HCLret - const.HCL - beta.HCL*Niftyoctnov$Closeret
Prices.oct_nov08$Hexa <- Prices.oct_nov08$Hexaret - const.Hexa - beta.Hexa*Niftyoctnov$Closeret
Prices.oct_nov08$Infy <- Prices.oct_nov08$Infyret - const.Infy - beta.Infy*Niftyoctnov$Closeret
Prices.oct_nov08$MP <- Prices.oct_nov08$MPret - const.MP - beta.MP*Niftyoctnov$Closeret
Prices.oct_nov08$Oracle <- Prices.oct_nov08$Oracleret - const.Oracle - beta.Oracle*Niftyoctnov$Closeret
Prices.oct_nov08$Satyam <- Prices.oct_nov08$Satyamret - const.Satyam - beta.Satyam*Niftyoctnov$Closeret
Prices.oct_nov08$Sonata <- Prices.oct_nov08$Sonataret - const.Sonata - beta.Sonata*Niftyoctnov$Closeret
Prices.oct_nov08$TCS <- Prices.oct_nov08$TCSret - const.TCS - beta.TCS*Niftyoctnov$Closeret
Prices.oct_nov08$TechM <- Prices.oct_nov08$TechMret - const.TechM - beta.TechM*Niftyoctnov$Closeret
Prices.oct_nov08$W <- Prices.oct_nov08$Wret - const.W - beta.W*Niftyoctnov$Closeret


###Subsetting for Network plots

Netreturn<- Prices.oct_nov08[,1:13]

###Correlation matrix

cor_mat <- cor(Netreturn)


###Generate the Network

adjm<-sqrt(2*(1-cor_mat))          #Generating the metric for network plot
g <- graph_from_adjacency_matrix(adjm, mode="undirected", weighted=TRUE)

###Generating the mst from graph

minimal<-mst(g)

###Coloring vertices

V(minimal)$color <- "green"
V(minimal)["Satyam"]$color<-"red"

###Final Plot
plot(minimal)

#######################################################Netout_MST_Plots end here################################################################################################################ 

##Below code can be used to create networks with threshold
#Thresholdmatrix_0.5
diag(cor_mat)<-0
cor_mat[cor_mat<0.5]<-0
cor_mat[cor_mat>=0.5]<-1
g <- graph_from_adjacency_matrix(cor_mat, mode="undirected", weighted=TRUE)
V(g)$color <- "green"
V(g)["Satyam"]$color<-"red"
plot(g)

################################################################################################################################################################################################
############################Below segment of this code helps in studying Cumulative Abnormal Returns (CARs)#####################################################################################
################################################################################################################################################################################################

###0.Necessary_packages
install.packages("dplyr")
install.packages("broom")
install.packages("readr")
library(readr)
library(dplyr)
library(broom)

###1.Importing_files

#Raw prices for estimation window
Prices.sep_nov08 <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//CARs Analysis//2008-sep_nov-ITstocks.csv",show_col_types = FALSE)
Prices.sep_nov08$Newdate <- as.Date(Prices.sep_nov08$Date,format="%d %b %Y")
Nifty50.sep_nov08 <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//CARs Analysis//2008-sep_nov-Nifty50.csv",show_col_types = FALSE)
Nifty50.sep_nov08$Newdate <- as.Date(Nifty50.sep_nov08$Date,format="%m/%d/%Y")

#Raw prices for event window
Prices.dec08_jan09 <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//CARs Analysis//dec08jan09-ITstocks.csv",show_col_types = FALSE)
Prices.dec08_jan09$Newdate <- as.Date(Prices.dec08_jan09$Date,format="%d %b %Y")
Nifty50.dec08_jan09 <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//CARs Analysis//dec08jan09 - Nifty50.csv",show_col_types = FALSE)
Nifty50.dec08_jan09$Newdate <- as.Date(Nifty50.dec08_jan09$Date,format="%m/%d/%Y")


###2.Working with Date Variable and getting returns as a variable from raw prices

Prices.sep_nov08 <- Prices.sep_nov08 %>%left_join(Nifty50.sep_nov08,by=c("Newdate"))
Prices.sep_nov08 <- Prices.sep_nov08 %>%
  mutate(Niftyret = c(0, 100*diff(log(Prices.sep_nov08$Close))),
         I3ret = c(0, 100*diff(log(Prices.sep_nov08$`3I Infotech Ltd.`))),
         CMCret = c(0, 100*diff(log(Prices.sep_nov08$`C M C Ltd. [Merged]`))),
         Firstret = c(0, 100*diff(log(Prices.sep_nov08$`Firstsource Solutions Ltd.`))),
         HCLret = c(0, 100*diff(log(Prices.sep_nov08$`H C L Technologies Ltd.`))),
         Hexaret = c(0, 100*diff(log(Prices.sep_nov08$`Hexaware Technologies Ltd.`))),
         Infyret = c(0, 100*diff(log(Prices.sep_nov08$`Infosys Ltd.`))),
         MPret = c(0, 100*diff(log(Prices.sep_nov08$`Mphasis Ltd.`))),
         Oracleret = c(0, 100*diff(log(Prices.sep_nov08$`Oracle Financial Services Software Ltd.`))),
         Satyamret = c(0, 100*diff(log(Prices.sep_nov08$`Satyam Computer Services Ltd. [Merged]`))),
         Sonataret = c(0, 100*diff(log(Prices.sep_nov08$`Sonata Software Ltd.`))),
         TCSret = c(0, 100*diff(log(Prices.sep_nov08$`Tata Consultancy Services Ltd.`))),
         TechMret = c(0, 100*diff(log(Prices.sep_nov08$`Tech Mahindra Ltd.`))),
         Wret = c(0, 100*diff(log(Prices.sep_nov08$`Wipro Ltd.`))))

Prices.dec08_jan09 <- Prices.dec08_jan09 %>%left_join(Nifty50.dec08_jan09,by=c("Newdate"))
Prices.dec08_jan09 <- Prices.dec08_jan09 %>%
  mutate(Niftyret = c(0, 100*diff(log(Prices.dec08_jan09$Close))),
         I3ret = c(0, 100*diff(log(Prices.dec08_jan09$`3I Infotech Ltd.`))),
         CMCret = c(0, 100*diff(log(Prices.dec08_jan09$`C M C Ltd. [Merged]`))),
         Firstret = c(0, 100*diff(log(Prices.dec08_jan09$`Firstsource Solutions Ltd.`))),
         HCLret = c(0, 100*diff(log(Prices.dec08_jan09$`H C L Technologies Ltd.`))),
         Hexaret = c(0, 100*diff(log(Prices.dec08_jan09$`Hexaware Technologies Ltd.`))),
         Infyret = c(0, 100*diff(log(Prices.dec08_jan09$`Infosys Ltd.`))),
         MPret = c(0, 100*diff(log(Prices.dec08_jan09$`Mphasis Ltd.`))),
         Oracleret = c(0, 100*diff(log(Prices.dec08_jan09$`Oracle Financial Services Software Ltd.`))),
         Satyamret = c(0, 100*diff(log(Prices.dec08_jan09$`Satyam Computer Services Ltd. [Merged]`))),
         Sonataret = c(0, 100*diff(log(Prices.dec08_jan09$`Sonata Software Ltd.`))),
         TCSret = c(0, 100*diff(log(Prices.dec08_jan09$`Tata Consultancy Services Ltd.`))),
         TechMret = c(0, 100*diff(log(Prices.dec08_jan09$`Tech Mahindra Ltd.`))),
         Wret = c(0, 100*diff(log(Prices.dec08_jan09$`Wipro Ltd.`))))


###3.Running Market Model Regression to take out the market effect from individual returns

I3.reg<- lm(Prices.sep_nov08$I3ret ~ Prices.sep_nov08$Niftyret)
CMC.reg<- lm(Prices.sep_nov08$CMCret ~ Prices.sep_nov08$Niftyret)
First.reg<- lm(Prices.sep_nov08$Firstret ~ Prices.sep_nov08$Niftyret)
HCL.reg<- lm(Prices.sep_nov08$HCLret ~ Prices.sep_nov08$Niftyret)
Hexa.reg<- lm(Prices.sep_nov08$Hexaret ~ Prices.sep_nov08$Niftyret)
Infy.reg<- lm(Prices.sep_nov08$Infyret ~ Prices.sep_nov08$Niftyret)
MP.reg<- lm(Prices.sep_nov08$MPret ~ Prices.sep_nov08$Niftyret)
Oracle.reg<- lm(Prices.sep_nov08$Oracleret ~ Prices.sep_nov08$Niftyret)
Satyam.reg<- lm(Prices.sep_nov08$Satyamret ~ Prices.sep_nov08$Niftyret)
Sonata.reg<- lm(Prices.sep_nov08$Sonataret ~ Prices.sep_nov08$Niftyret)
TCS.reg<- lm(Prices.sep_nov08$TCSret ~ Prices.sep_nov08$Niftyret)
TechM.reg<- lm(Prices.sep_nov08$TechMret ~ Prices.sep_nov08$Niftyret)
W.reg<- lm(Prices.sep_nov08$Wret ~ Prices.sep_nov08$Niftyret)

beta.I3 <- I3.reg$coefficients[2]
beta.CMC <- CMC.reg$coefficients[2]
beta.First <- First.reg$coefficients[2]
beta.HCL <- HCL.reg$coefficients[2]
beta.Hexa <- Hexa.reg$coefficients[2]
beta.Infy <- Infy.reg$coefficients[2]
beta.MP <- MP.reg$coefficients[2]
beta.Oracle <- Oracle.reg$coefficients[2]
beta.Satyam <- Satyam.reg$coefficients[2]
beta.Sonata <- Sonata.reg$coefficients[2]
beta.TCS <- TCS.reg$coefficients[2]
beta.TechM <- TechM.reg$coefficients[2]
beta.W <- W.reg$coefficients[2]

const.I3 <- I3.reg$coefficients[1]
const.CMC <- CMC.reg$coefficients[1]
const.First <- First.reg$coefficients[1]
const.HCL <- HCL.reg$coefficients[1]
const.Hexa <- Hexa.reg$coefficients[1]
const.Infy <- Infy.reg$coefficients[1]
const.MP <- MP.reg$coefficients[1]
const.Oracle <- Oracle.reg$coefficients[1]
const.Satyam <- Satyam.reg$coefficients[1]
const.Sonata <- Sonata.reg$coefficients[1]
const.TCS <- TCS.reg$coefficients[1]
const.TechM <- TechM.reg$coefficients[1]
const.W <- W.reg$coefficients[1]


###4.Cumulative Abnormal returns over the event window

#Abnormal returns
Prices.dec08_jan09 <- Prices.dec08_jan09 %>%
  mutate(I3_AR = Prices.dec08_jan09$I3ret - const.I3 - beta.I3*Prices.dec08_jan09$Niftyret,
         CMC_AR = Prices.dec08_jan09$CMCret - const.CMC - beta.CMC*Prices.dec08_jan09$Niftyret,
         First_AR = Prices.dec08_jan09$Firstret - const.First - beta.First*Prices.dec08_jan09$Niftyret,
         HCL_AR = Prices.dec08_jan09$HCLret - const.HCL - beta.HCL*Prices.dec08_jan09$Niftyret,
         Hexa_AR = Prices.dec08_jan09$Hexaret - const.Hexa - beta.Hexa*Prices.dec08_jan09$Niftyret,
         Infy_AR = Prices.dec08_jan09$Infyret - const.Infy - beta.Infy*Prices.dec08_jan09$Niftyret,
         MP_AR = Prices.dec08_jan09$MPret - const.MP - beta.MP*Prices.dec08_jan09$Niftyret,
         Oracle_AR = Prices.dec08_jan09$Oracleret - const.Oracle - beta.Oracle*Prices.dec08_jan09$Niftyret,
         Satyam_AR = Prices.dec08_jan09$Satyamret - const.Satyam - beta.Satyam*Prices.dec08_jan09$Niftyret,
         Sonata_AR = Prices.dec08_jan09$Sonataret - const.Sonata - beta.Sonata*Prices.dec08_jan09$Niftyret,
         TCS_AR = Prices.dec08_jan09$TCSret - const.TCS - beta.TCS*Prices.dec08_jan09$Niftyret,
         TechM_AR = Prices.dec08_jan09$TechMret - const.TechM - beta.TechM*Prices.dec08_jan09$Niftyret,
         W_AR = Prices.dec08_jan09$Wret - const.W - beta.W*Prices.dec08_jan09$Niftyret)

#Focusing on (-1,+1) window to get the firm specific CARs of each event
event_ARs <- Prices.dec08_jan09 %>%
  filter(Newdate>="2009-01-06" & Newdate<="2009-01-09") 

#CARs
event_CARs <- event_ARs %>%
  mutate(I3_CAR = sum(I3_AR),
         CMC_CAR = sum(CMC_AR),
         First_CAR = sum(First_AR),
         HCL_CAR = sum(HCL_AR),
         Hexa_CAR = sum(Hexa_AR),
         Infy_CAR=sum(Infy_AR),
         MP_CAR=sum(MP_AR),
         Oracle_CAR=sum(Oracle_AR),
         Satyam_CAR=sum(Satyam_AR),
         Sonata_CAR=sum(Sonata_AR),
         TCS_CAR=sum(TCS_AR),
         TechM_CAR=sum(TechM_AR),
         W_CAR=sum(W_AR)) %>%
  filter(Newdate=="2009-01-09") %>%
  select(I3_CAR:W_CAR)

Company <- c("I3","CMC","First","HCL","Hexa","Infy","MP","Oracle","Satyam","Sonata","TCS","TechM","W")

CARs <- t(event_CARs)

groupdummy <- c(0,0,0,0,0,1,0,0,0,0,1,0,1)

event_CARs.2 <- data.frame(Company,CARs,groupdummy)

write.csv(event_CARs.2,file = "C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//CARs Analysis//CARs.csv")


##############################################################CARs Code ends here#######################################################################################



################################################################################################################################################################################################
############################Below segment of this code helps in mapping different volatility and raw price plots that have occured throughout the thesis#####################################################################################
################################################################################################################################################################################################
library(lubridate)
install.packages("zoo")
library(zoo)

NiftyIT_price <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//Nifty 2008dec31-200930sep.csv",col_types = cols(Date = col_date(format = "%d-%b-%Y")))


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

plot(monthly$x,type="l",ylab="Volatility",col="blue",col.main="red",cex.main=1.5,las=3)

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


###############################################################################Volatility and raw price Plot ends here#####################################################################


################################################################################################################################################################################################
############################Below segment of this code helps in getting all the overlapping balance sheet plots that appear throughout the thesis#####################################################################################
################################################################################################################################################################################################

#Packages
library(lubridate)
library(readr)

#creating the pdf that would be the output
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


