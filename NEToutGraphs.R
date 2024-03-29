                                      ###0.Necessary_packages

install.packages("readr")
library(readr)
# igraph to make a graph-object and visualize it
install.packages("igraph")
library(igraph)


                                      ###1.Importing files

Prices.oct_nov08 <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//prices csv//oct08.csv",show_col_types = FALSE)
Niftyoctnov <- read_csv("C://Users//User//OneDrive//Desktop//THESIS related data work///FINAL/nifty prices/octnov08.csv", col_types = cols(Date = col_date(format = "%m/%d/%Y")))


                                      ###2.Getting returns from raw price data

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


                                       ###3.Running market model to get residual daily returns

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


                                       ###4.Subsetting for Network plots

Netreturn<- Prices.oct_nov08[,1:13]

                                       ###5.Correlation matrix

cor_mat <- cor(Netreturn)


                                       ###6.Generate the Network

adjm<-sqrt(2*(1-cor_mat))          #Generating the metric for network plot
g <- graph_from_adjacency_matrix(adjm, mode="undirected", weighted=TRUE)

                                       ###7.Generating the mst from graph

minimal<-mst(g)

                                       ###8.Coloring vertices

V(minimal)$color <- "green"
V(minimal)["Satyam"]$color<-"red"

                                       ###9.Final Plot
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


