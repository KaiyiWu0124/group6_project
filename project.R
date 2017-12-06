setwd("C:/Users/Kaiyi/Downloads/ProjectData")  ## set work directory

zipcode = "15021"  ## user input

## processing 1st input 
##extract data set (saved as penndata) according to zipcode
zipdist = read.csv("zipdist.csv")
distributor = apply(zipdist, 2, function(x) which(x == "15021"))
n = length(distributor$Zip)
penndata = NA
for(i in 1:n){
  distr = as.character(zipdist[distributor$Zip[i],4])
  dataseg = read.csv(paste(zipcode, distr,".csv", sep = ""))
  penndata = rbind(penndata, dataseg)
}
penndata = penndata[2:nrow(penndata),]
penndata = penndata[, c(1,2,10)]
## only take 12 month contract or no term contract
penndata = penndata[penndata$Term.Length == "12 months"|penndata$Term.Length == "No term length",]
## finish building up data set



## supplier cost not available

energy.type = c("renewable", "oil") ## user input
energy.demand = 10000   ## user input

sel = which.min(penndata$Price)

opt_cost = function(energy.demand){
 
}

 


cf_naturalgas = 465
cf_oil = 821
cf_renewable = 0