#This is only for zipcode 16801 (State College)
#If you have issues with the file not found
#Need to use the following in console to the folder the file is in:
#setwd("C:/Users/asdf/folder etc...")
data=read.csv("16801WestPennPower.csv",header=TRUE,sep=",")
#Column names for reference
colnames(data)

renewable=subset(data,Renewable == "Y")
renewable


lowestprice = data[which(data==min(data$Price)),]

lowestrenew = renewable[which(data==min(data$Price)),]
lowestrenew

