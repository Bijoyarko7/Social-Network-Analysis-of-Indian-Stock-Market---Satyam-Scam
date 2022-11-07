library(readr)
X2019augsep <- read_csv("C:/Users/User/Desktop/THESIS related data work/.csv", 
                        col_types = cols(X1 = col_date(format = "%m/%d/%Y")))
Niftyaugsep <- read_csv("C:/Users/User/Desktop/THESIS related data work/Niftydecfeb.csv", 
                        col_types = cols(Date = col_date(format = "%m/%d/%Y")))


#Generating returns 
X2019augsep$Axisret <- c(0, 100*diff(log(X2019augsep$`Axis Bank Ltd.`)))
X2019augsep$BOBret <- c(0, 100*diff(log(X2019augsep$`Bank Of Baroda`)))
X2019augsep$BOIret <- c(0, 100*diff(log(X2019augsep$`Bank Of India`)))
X2019augsep$BOMret <- c(0, 100*diff(log(X2019augsep$`Bank Of Maharashtra`)))
X2019augsep$CBret <- c(0, 100*diff(log(X2019augsep$`Canara Bank`)))
X2019augsep$CBOIret <- c(0, 100*diff(log(X2019augsep$`Central Bank Of India`)))
X2019augsep$FBLret <- c(0, 100*diff(log(X2019augsep$`Federal Bank Ltd.`)))
X2019augsep$HDFCret <- c(0, 100*diff(log(X2019augsep$`H D F C Bank Ltd.`)))
X2019augsep$ICICIret <- c(0, 100*diff(log(X2019augsep$`I C I C I Bank Ltd.`)))
X2019augsep$IDBIret <- c(0, 100*diff(log(X2019augsep$`I D B I Bank Ltd.`)))
X2019augsep$IDFCFirstret <- c(0, 100*diff(log(X2019augsep$`I D F C First Bank Ltd.`)))
X2019augsep$IndianBankret <- c(0, 100*diff(log(X2019augsep$`Indian Bank`)))
X2019augsep$IndianOverseasBankret <- c(0, 100*diff(log(X2019augsep$`Indian Overseas Bank`)))
X2019augsep$Indusindret <- c(0, 100*diff(log(X2019augsep$`Indusind Bank Ltd.`)))
X2019augsep$PNBret <- c(0, 100*diff(log(X2019augsep$`Punjab National Bank`)))
X2019augsep$SBIret <- c(0, 100*diff(log(X2019augsep$`State Bank Of India`)))
X2019augsep$Ucoret <- c(0, 100*diff(log(X2019augsep$`Uco Bank`)))
X2019augsep$UBIret <- c(0, 100*diff(log(X2019augsep$`Union Bank Of India`)))
X2019augsep$YESret <- c(0, 100*diff(log(X2019augsep$`Yes Bank Ltd.`)))



Niftyaugsep$Closeret <- c(0, 100*diff(log(Niftyaugsep$Close)))


#Regression

Axis.reg<- lm(X2019augsep$Axisret ~ Niftyaugsep$Closeret)
BOB.reg<- lm(X2019augsep$BOBret ~ Niftyaugsep$Closeret)
BOI.reg<- lm(X2019augsep$BOIret ~ Niftyaugsep$Closeret)
BOM.reg<- lm(X2019augsep$BOMret ~ Niftyaugsep$Closeret)
CB.reg<- lm(X2019augsep$CBret ~ Niftyaugsep$Closeret)
CBOI.reg<- lm(X2019augsep$CBOIret ~ Niftyaugsep$Closeret)
FBL.reg<- lm(X2019augsep$FBLret ~ Niftyaugsep$Closeret)
HDFC.reg<- lm(X2019augsep$HDFCret ~ Niftyaugsep$Closeret)
ICICI.reg<- lm(X2019augsep$ICICIret ~ Niftyaugsep$Closeret)
IDBI.reg<- lm(X2019augsep$IDBIret ~ Niftyaugsep$Closeret)
IDFCFirst.reg<- lm(X2019augsep$IDFCFirstret ~ Niftyaugsep$Closeret)
IndianBank.reg<- lm(X2019augsep$IndianBankret ~ Niftyaugsep$Closeret)
IndianOverseasBank.reg<- lm(X2019augsep$IndianOverseasBankret ~ Niftyaugsep$Closeret)
Indusind.reg<- lm(X2019augsep$Indusindret ~ Niftyaugsep$Closeret)
PNB.reg<- lm(X2019augsep$PNBret ~ Niftyaugsep$Closeret)
SBI.reg<- lm(X2019augsep$SBIret ~ Niftyaugsep$Closeret)
Uco.reg<- lm(X2019augsep$Ucoret ~ Niftyaugsep$Closeret)
UBI.reg<- lm(X2019augsep$UBIret ~ Niftyaugsep$Closeret)
Yes.reg<- lm(X2019augsep$YESret ~ Niftyaugsep$Closeret)


beta.Axis <- Axis.reg$coefficients[2]
beta.BOB <- BOB.reg$coefficients[2]
beta.BOI <- BOI.reg$coefficients[2]
beta.BOM <- BOM.reg$coefficients[2]
beta.CB <- CB.reg$coefficients[2]
beta.CBOI <- CBOI.reg$coefficients[2]
beta.FBL <- FBL.reg$coefficients[2]
beta.HDFC <- HDFC.reg$coefficients[2]
beta.ICICI <- ICICI.reg$coefficients[2]
beta.IDBI <- IDBI.reg$coefficients[2]
beta.IDFCFirst <- IDFCFirst.reg$coefficients[2]
beta.IndianBank <- IndianBank.reg$coefficients[2]
beta.IndianOverseasBank <- IndianOverseasBank.reg$coefficients[2]
beta.Indusind <- Indusind.reg$coefficients[2]
beta.PNB <- PNB.reg$coefficients[2]
beta.SBI <- SBI.reg$coefficients[2]
beta.Uco <- Uco.reg$coefficients[2]
beta.UBI <- UBI.reg$coefficients[2]
beta.Yes <- Yes.reg$coefficients[2]

const.Axis <- Axis.reg$coefficients[1]
const.BOB <- BOB.reg$coefficients[1]
const.BOI <- BOI.reg$coefficients[1]
const.BOM <- BOM.reg$coefficients[1]
const.CB <- CB.reg$coefficients[1]
const.CBOI <- CBOI.reg$coefficients[1]
const.FBL <- FBL.reg$coefficients[1]
const.HDFC <- HDFC.reg$coefficients[1]
const.ICICI <- ICICI.reg$coefficients[1]
const.IDBI <- IDBI.reg$coefficients[1]
const.IDFCFirst <- IDFCFirst.reg$coefficients[1]
const.IndianBank <- IndianBank.reg$coefficients[1]
const.IndianOverseasBank <- IndianOverseasBank.reg$coefficients[1]
const.Indusind <- Indusind.reg$coefficients[1]
const.PNB <- PNB.reg$coefficients[1]
const.SBI <- SBI.reg$coefficients[1]
const.Uco <- Uco.reg$coefficients[1]
const.UBI <- UBI.reg$coefficients[1]
const.Yes <- Yes.reg$coefficients[1]


#Returns after netting out Nifty

X2019augsep$Axis <- X2019augsep$Axisret - const.Axis - beta.Axis*Niftyaugsep$Closeret
X2019augsep$BOB <- X2019augsep$BOBret - const.BOB - beta.BOB*Niftyaugsep$Closeret
X2019augsep$BOI <- X2019augsep$BOIret - const.BOI - beta.BOI*Niftyaugsep$Closeret
X2019augsep$BOM <- X2019augsep$BOMret - const.BOM - beta.BOM*Niftyaugsep$Closeret
X2019augsep$CB <- X2019augsep$CBret - const.CB - beta.CB*Niftyaugsep$Closeret
X2019augsep$CBOI <- X2019augsep$CBOIret - const.CBOI - beta.CBOI*Niftyaugsep$Closeret
X2019augsep$FBL <- X2019augsep$FBLret - const.FBL - beta.FBL*Niftyaugsep$Closeret
X2019augsep$HDFC <- X2019augsep$HDFCret - const.HDFC - beta.HDFC*Niftyaugsep$Closeret
X2019augsep$ICICI <- X2019augsep$ICICIret - const.ICICI - beta.ICICI*Niftyaugsep$Closeret
X2019augsep$IDBI <- X2019augsep$IDBIret - const.IDBI - beta.IDBI*Niftyaugsep$Closeret
X2019augsep$IDFCFirst <- X2019augsep$IDFCFirstret - const.IDFCFirst - beta.IDFCFirst*Niftyaugsep$Closeret
X2019augsep$IndianBank <- X2019augsep$IndianBankret - const.IndianBank - beta.IndianBank*Niftyaugsep$Closeret
X2019augsep$IndianOverseas <- X2019augsep$IndianOverseasBankret - const.IndianOverseasBank - beta.IndianOverseasBank*Niftyaugsep$Closeret
X2019augsep$Indusind<- X2019augsep$Indusindret - const.Indusind - beta.Indusind*Niftyaugsep$Closeret
X2019augsep$PNB <- X2019augsep$PNBret - const.PNB - beta.PNB*Niftyaugsep$Closeret
X2019augsep$SBI <- X2019augsep$SBIret - const.SBI - beta.SBI*Niftyaugsep$Closeret
X2019augsep$Uco <- X2019augsep$Ucoret - const.Uco - beta.Uco*Niftyaugsep$Closeret
X2019augsep$UBI <- X2019augsep$UBIret - const.UBI - beta.UBI*Niftyaugsep$Closeret
X2019augsep$Yes <- X2019augsep$YESret - const.Yes - beta.Yes*Niftyaugsep$Closeret


#Subsetting for graph

Netreturn<- X2019augsep[,40:58]

#Correlation matrix

cor_mat <- cor(Netreturn)

# igraph to make a graph-object and visualize it

library(igraph)

#Generate the graph
g <- graph_from_adjacency_matrix(cor_mat, mode="undirected", weighted=TRUE)

#Generating the mst from graph

minimal<-mst(g)

V(minimal)$label.cex=0.8
V(minimal)$size=7

#plotting mst

plot(minimal)
write.graph(minimal,"decfeb.dot","dot")
#Coloring vertices
V(minimal)$color <- "green"
V(minimal)["Satyam"]$color<-"red"

