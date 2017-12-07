#TEMPORARY FUNCTION FOR SAMPLE SHINY APP
#Real functions will be expanded to include zipcode, distributor, multiple renewables
data=read.csv("16801WestPennPower.csv",header=TRUE,sep=",")
#Column names for reference
colnames(data)
length(data$Price)


renewable=subset(data,Renewable == "Y")
lowestprice = data[which(data==min(data$Price)),]
lowestrenew = renewable[which(data==min(data$Price)),]

cf_coal = 909
cf_naturalgas = 465
cf_oil = 821
cf_renewable = 0

cf_choose = function(energytype){
  if (energytype == "Oil"){return(cf_oil)}
  else if (energytype =="Natural Gas"){return(cf_naturalgas)}
  else if (energytype == "Coal"){return(cf_coal)}
  else if (energytype == "Renewable"){return(cf_renewable)}
}

shinylowcost = function(zipcode,dist,kwh){
  return (lowestprice$Price * kwh)
}
shinylowrenew = function(zipcode,dist,kwh){
  return (lowestrenew$Price * kwh)
}
shinycflow = function (zipcode,dist,kwh){
  energytype = cf_choose(lowestprice$Type.1)
  return (energytype * kwh)
}
shinycfrenew = function (zipcode,dist,kwh){
  energytype = cf_choose(lowestrenew$Type.1)
  return (energytype * kwh)
}

cfall =  data.frame(Supplier=NA,Price=NA,CO2=NA)

shinycfall = function(zipcode,dist,kwh){
  for(i in 1:length(data$Price)){
    cost = (data$Price[i] * kwh)
    cf= (as.numeric(cf_choose(data$Type.1[i])) * kwh)
    cfall = rbind(cfall, data.frame(Supplier =as.character(data$Supplier[i]), Price = as.numeric(cost), CO2 = as.numeric(cf)))

  }
  return(cfall)
}
