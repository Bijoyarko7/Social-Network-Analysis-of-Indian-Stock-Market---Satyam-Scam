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

#######################################################Netout_MST_Plots end here########################################## 

##Below code can be used to create networks with threshold
#Thresholdmatrix_0.5
diag(cor_mat)<-0
cor_mat[cor_mat<0.5]<-0
cor_mat[cor_mat>=0.5]<-1
g <- graph_from_adjacency_matrix(cor_mat, mode="undirected", weighted=TRUE)
V(g)$color <- "green"
V(g)["Satyam"]$color<-"red"
plot(g)
