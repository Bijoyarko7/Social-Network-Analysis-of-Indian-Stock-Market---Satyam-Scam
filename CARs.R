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
  mutate(I3_CAR = mean(I3_AR),
         CMC_CAR = mean(CMC_AR),
         First_CAR = mean(First_AR),
         HCL_CAR = mean(HCL_AR),
         Hexa_CAR = mean(Hexa_AR),
         Infy_CAR=mean(Infy_AR),
         MP_CAR=mean(MP_AR),
         Oracle_CAR=mean(Oracle_AR),
         Satyam_CAR=mean(Satyam_AR),
         Sonata_CAR=mean(Sonata_AR),
         TCS_CAR=mean(TCS_AR),
         TechM_CAR=mean(TechM_AR),
         W_CAR=mean(W_AR)) %>%
  filter(Newdate=="2009-01-09") %>%
  select(I3_CAR:W_CAR)

Company <- c("I3","CMC","First","HCL","Hexa","Infy","MP","Oracle","Satyam","Sonata","TCS","TechM","W")

CARs <- t(event_CARs)

groupdummy <- c(0,0,0,0,0,1,0,0,0,0,1,0,1)

event_CARs.2 <- data.frame(Company,CARs,groupdummy)

write.csv(event_CARs.2,file = "C://Users//User//OneDrive//Desktop//THESIS related data work//FINAL//CARs Analysis//CARs.csv")


##############################################################CARs Code ends here#######################################################################################
########################################################################################################################################################################
########################################################################################################################################################################

#Tried Regressions on CARs to check dummy_group effect

CAR.reg<- lm(event_CARs.2$CARs ~ event_CARs.2$groupdummy)


beta.CARs <- CAR.reg$coefficients[2]

const.CARs <- CAR.reg$coefficients[1]


model = lm(event_CARs.2$CARs ~ event_CARs.2$groupdummy) # fit a model

summary(model) # usual summary of a model fit

tidy(model) # get coefficient table as a data frame

glance(model) # get rest of stats as a data frame

glance(model)$p.value # get p value


