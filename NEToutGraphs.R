library(readr)
X2019augsep <- read_csv("F:/THESIS related data work/FINAL/prices csv/oct08.csv", 
                        col_types = cols(X1 = col_date(format = "%m/%d/%Y")))
Niftyaugsep <- read_csv("F:/THESIS related data work/FINAL/nifty prices/octnov08.csv", 
                        col_types = cols(Date = col_date(format = "%m/%d/%Y")))
X2019augsep$I3ret <- c(0, 100*diff(log(X2019augsep$I3)))
X2019augsep$CMCret <- c(0, 100*diff(log(X2019augsep$CMC)))
X2019augsep$Firstret <- c(0, 100*diff(log(X2019augsep$First)))
X2019augsep$HCLret <- c(0, 100*diff(log(X2019augsep$HCL)))
X2019augsep$Hexaret <- c(0, 100*diff(log(X2019augsep$Hexa)))
X2019augsep$Infyret <- c(0, 100*diff(log(X2019augsep$Infy)))
X2019augsep$MPret <- c(0, 100*diff(log(X2019augsep$MP)))
X2019augsep$Oracleret <- c(0, 100*diff(log(X2019augsep$Oracle)))
X2019augsep$Satyamret <- c(0, 100*diff(log(X2019augsep$Satyam)))
X2019augsep$Sonataret <- c(0, 100*diff(log(X2019augsep$Sonata)))
X2019augsep$TCSret <- c(0, 100*diff(log(X2019augsep$TCS)))
X2019augsep$TechMret <- c(0, 100*diff(log(X2019augsep$TechM)))
X2019augsep$Wret <- c(0, 100*diff(log(X2019augsep$W)))
Niftyaugsep$Closeret <- c(0, 100*diff(log(Niftyaugsep$Close)))

I3.reg<- lm(X2019augsep$I3ret ~ Niftyaugsep$Closeret)
CMC.reg<- lm(X2019augsep$CMCret ~ Niftyaugsep$Closeret)
First.reg<- lm(X2019augsep$Firstret ~ Niftyaugsep$Closeret)
HCL.reg<- lm(X2019augsep$HCLret ~ Niftyaugsep$Closeret)
Hexa.reg<- lm(X2019augsep$Hexaret ~ Niftyaugsep$Closeret)
Infy.reg<- lm(X2019augsep$Infyret ~ Niftyaugsep$Closeret)
MP.reg<- lm(X2019augsep$MPret ~ Niftyaugsep$Closeret)
Oracle.reg<- lm(X2019augsep$Oracleret ~ Niftyaugsep$Closeret)
Satyam.reg<- lm(X2019augsep$Satyamret ~ Niftyaugsep$Closeret)
Sonata.reg<- lm(X2019augsep$Sonataret ~ Niftyaugsep$Closeret)
TCS.reg<- lm(X2019augsep$TCSret ~ Niftyaugsep$Closeret)
TechM.reg<- lm(X2019augsep$TechMret ~ Niftyaugsep$Closeret)
W.reg<- lm(X2019augsep$Wret ~ Niftyaugsep$Closeret)

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

X2019augsep$I3 <- X2019augsep$I3ret - const.I3 - beta.I3*Niftyaugsep$Closeret
X2019augsep$CMC <- X2019augsep$CMCret - const.CMC - beta.CMC*Niftyaugsep$Closeret
X2019augsep$First <- X2019augsep$Firstret - const.First - beta.First*Niftyaugsep$Closeret
X2019augsep$HCL <- X2019augsep$HCLret - const.HCL - beta.HCL*Niftyaugsep$Closeret
X2019augsep$Hexa <- X2019augsep$Hexaret - const.Hexa - beta.Hexa*Niftyaugsep$Closeret
X2019augsep$Infy <- X2019augsep$Infyret - const.Infy - beta.Infy*Niftyaugsep$Closeret
X2019augsep$MP <- X2019augsep$MPret - const.MP - beta.MP*Niftyaugsep$Closeret
X2019augsep$Oracle <- X2019augsep$Oracleret - const.Oracle - beta.Oracle*Niftyaugsep$Closeret
X2019augsep$Satyam <- X2019augsep$Satyamret - const.Satyam - beta.Satyam*Niftyaugsep$Closeret
X2019augsep$Sonata <- X2019augsep$Sonataret - const.Sonata - beta.Sonata*Niftyaugsep$Closeret
X2019augsep$TCS <- X2019augsep$TCSret - const.TCS - beta.TCS*Niftyaugsep$Closeret
X2019augsep$TechM <- X2019augsep$TechMret - const.TechM - beta.TechM*Niftyaugsep$Closeret
X2019augsep$W <- X2019augsep$Wret - const.W - beta.W*Niftyaugsep$Closeret

#Subsetting for graph

Netreturn<- X2019augsep[,1:13]

#Correlation matrix

cor_mat <- cor(Netreturn)

# igraph to make a graph-object and visualize it

library(igraph)

#Generate the graph

adjm<-sqrt(2*(1-cor_mat))
g <- graph_from_adjacency_matrix(adjm, mode="undirected", weighted=TRUE)

#Generating the mst from graph

minimal<-mst(g)

#Coloring vertices
V(minimal)$color <- "green"
V(minimal)["Satyam"]$color<-"red"

plot(minimal)




#Thresholdmatrix_0.5
diag(cor_mat)<-0
cor_mat[cor_mat<0.5]<-0
cor_mat[cor_mat>=0.5]<-1
g <- graph_from_adjacency_matrix(cor_mat, mode="undirected", weighted=TRUE)
V(g)$color <- "green"
V(g)["Satyam"]$color<-"red"
plot(g)


