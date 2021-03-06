#TEMPORARY FUNCTION FOR SAMPLE SHINY APP
#Real functions will be expanded to include zipcode, distributor, multiple renewables
data=read.csv("16801WestPennPower.csv",header=TRUE,sep=",")
#Column names for reference
colnames(data)
length(data$Price)



#Returns 1 KWH = Grams of CO2 produced by energy type
cf_choose = function(energytype){
  cf_coal = 909
  cf_naturalgas = 465
  cf_oil = 821
  cf_renewable = 0
  if (energytype == "Oil"){return(cf_oil)}
  else if (energytype =="Natural Gas"){return(cf_naturalgas)}
  else if (energytype == "Coal"){return(cf_coal)}
  else if (energytype == "Renewable"){return(cf_renewable)}
}
#Resorts Data for plotting + converts price-per-kilowatt to price + Calculates total CO2 in kg
sort.data = function(data,kwh,energytype){
  #Reorders datatable by Price and reindexes row numbers
  data = data[order(data$Price),];rownames(data) = 1:nrow(data)
  #Converts Price per kwh to cost
  data$Price = data$Price *kwh
  #Adds Column with Kg of CO2 produced with cf_choose function: (G of CO2 per KWH)/1000 * KWH
  for(i in 1:length(energytype)){
    data$CO[i]=((cf_choose(energytype[i])/1000)*kwh)
  }
  return(data)
} 


#Plots ggplot of cost/total co/highlights your bill and lowest cost/ lowest renewable cost
plot.costco=function(data,kwh,bill){
  #Resorting data into workable format
  plotdata = sort.data(data,kwh,data$Type.1)
  #X values for graph since data is already sorted by price.
  xvalue = 1:nrow(data)
  ## = NEED TO ADD
  ##Finds the lowest renewable
  ##renewable=subset(data,Renewable == "Y")
  lowestrenew = renewable[which(plotdata==min(plotdata$Price)),]
  lowestrenewtitle = paste("The Lowest Renewable Supplier is:",lowestrenew$Supplier,"@ $",lowestrenew$Price,"per kiloWatthour")
  ##Lowest Cost
  lowestcost = plotdata[which(plotdata==min(plotdata$Price)),]
  lowestcosttitle = paste("The Lowest Cost Supplier is:",lowestcost$Supplier,"@ $",lowestrenew$Price,"per kiloWatthour")
  ##TO BE ADDED: Color hack so it shows lowest cost/lowest renewable 
  ##plotdata$color=ifelse(rownames(plotdata) ==(lowestrenew|lowestcost),"True","False")
  #Plot function:
  plotcost=ggplot(plotdata)+
    #Price Bar
    geom_bar(stat="identity", aes(x=reorder(xvalue,-Price),y=Price),color="black",fill="blue")+
    #CO2 Bar
    geom_bar(stat="identity",aes(x=xvalue,y=(CO/10)),color="green",alpha=.3)+
    #Your Bill Point
    geom_point(aes(y=bill,x=median(xvalue)),size=5,color="red")+
    #Hides Old Axis labels
    theme(plot.title = element_text(hjust=0, size=12),plot.subtitle = element_text(hjust=0, size=12),axis.title=element_text(face="bold",size=14),axis.text.y=element_text(face="bold",size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank())+
    #New Axis Label
    labs(title=(lowestcosttitle),subtitle=(lowestrenewtitle),x="Suppliers",y="Cost (blue)")+
    #2nd Y Axis Label/Scaling
    scale_y_continuous(sec.axis= sec_axis(~./max(plotdata$CO)*10000,name="Total Amount of CO2 in kg (Green)"))+
    annotate("text",x=median(xvalue),y=bill+5, label="Your Current Bill")
  return(plotcost)
}
plot.costco(data,100,100)


