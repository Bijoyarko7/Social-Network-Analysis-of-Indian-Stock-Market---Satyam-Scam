###Code written by: Rajeswari Sengupta
###Date: February 2020
###Objective: Estimate DEA model to measure bank productivity for every year for Foreign banks

library("deaR")
library("dplyr")
##Read the data file for one year
work<-read.csv(file="C:\\Users\\User\\Desktop\\THESIS related data work\\RA\\RA work\\Actual_Project\\allpanel.csv",head=TRUE,sep=",")

##CREATING A DATAFRAME THAT WILL ONLY HAVE THE FOREIGN BANKS, USING THE DUMMY VARIABLE
Privatework = work%>%filter(work$PVT==1)

##Calculating operating expenses
for2018$Op_expense <- (for2018$Total_expense - for2018$Emp_expense -
for2018$Int_expense)

##Adapting the data to the reading format for basic dea model in INTERMEDIATION approach where Deposit is an input
data_2018for <- read_data(datadea =  for2018,
                       inputs = c(2,3,10),
                       outputs = 6)


##Running the input-oriented DEA model with CRS and storing the result
result_crs2018_for<- model_basic(data_2018for,
                          orientation="io",
                          rts="crs")



##Summary of results
summary(result_crs2018_for, exportExcel = TRUE, filename = "C:\\Users\\User\\Desktop\\THESIS related data work\\RA\\RA work\\Actual_Project\\Results\\Foreign\\result_crs2018_for.xlsx")


##Adapting the data to the reading format for basic dea model in VALUE-ADDED approach where Deposit is an output and interest expense is an input
data_2018for_va <- read_data(datadea =  for2018,
                        inputs = c(3,5,10),
                        outputs = c(2,6))


##Running the input-oriented DEA model with CRS
result_crs2018_for_va <- model_basic(data_2018for_va,
                        orientation="io",
                        rts="crs")

##Summary of results
summary(result_crs2018_for_va, exportExcel = TRUE, filename = "C:\\Users\\User\\Desktop\\THESIS related data work\\RA\\RA work\\Actual_Project\\Results\\Foreign\\result_crs2018_for_va.xlsx")

##Robustness checks with VRS in INTERMEDIATION approach
#result_vrs2015_for <- model_basic(data_2015for,
#orientation="io",
#rts="vrs")

#summary(result_vrs2015_for, exportExcel = TRUE, filename = "../RESULTS/DEA/FOREIGN/result_vrs2015_for")


##Robustness checks with VRS in VALUE-ADDED approach
##result_vrs2015_for_va <- model_basic(data_2015for_va,
##orientation="io",
##rts="vrs")

##summary(result_vrs2015_for_va, exportExcel = TRUE, filename = "../RESULTS/DEA/FOREIGN/result_vrs2015_for_va")

