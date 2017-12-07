#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#TEMPORARY FUNCTION FOR SAMPLE SHINY APP
#Real functions will be expanded to include zipcode, distributor, multiple renewables
library(shiny)
library(ggplot2)
data=read.csv("16801WestPennPower.csv",header=TRUE,sep=",")
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
sort.data = function(data,kwh,bill){
  #Reorders datatable by Price and reindexes row numbers
  data = data[order(data$Price),];rownames(data) = 1:nrow(data)
  #Converts Price per kwh to cost
  data$Price = data$Price *kwh
  #Adds Column with Kg of CO2 produced with cf_choose function: (G of CO2 per KWH)/1000 * KWH
  for(i in 1:length(data$Price)){
    data$CO[i]=(bill/data$Price[i]) *16.44
    #((cf_choose(energytype[i])/1000)*kwh)
  }
  return(data)
} 


#Plots ggplot of cost/total co/highlights your bill and lowest cost/ lowest renewable cost
plot.costco=function(data,kwh,bill){
  #Resorting data into workable format
  plotdata = sort.data(data,kwh,bill)
  #X values for graph since data is already sorted by price.
  xvalue = 1:nrow(data)
  ## = NEED TO ADD
  ##Finds the lowest renewable
  renewable=subset(data,Renewable == "Y")
  yourco = (bill/kwh)*16.44
  lowestrenew = renewable[which(plotdata==min(plotdata$Price)),]
  lowestrenewtitle = paste("The Lowest Renewable Supplier is:",lowestrenew$Supplier,"@ $",lowestrenew$Price)
  ##Lowest Cost
  lowestcost = plotdata[which(plotdata==min(plotdata$Price)),]
  lowestcosttitle = paste("The Lowest Cost Supplier is:",lowestcost$Supplier,"@ $",lowestrenew$Price)
  ##TO BE ADDED: Color hack so it shows lowest cost/lowest renewable 
  ##plotdata$color=ifelse(rownames(plotdata) ==(lowestrenew|lowestcost),"True","False")
  #Plot function:

  plotcost=ggplot(plotdata)+
    #Price Bar
    geom_bar(stat="identity", aes(x=reorder(xvalue,-Price),y=Price),color="black",fill="blue")+
    #CO2 Bar
    geom_bar(stat="identity",aes(x=xvalue,y=CO/2),color="green")+
    #Your Bill Point
    geom_point(aes(y=bill,x=median(xvalue)),size=5,color="red")+
    #Your estminated carbon footprint
    geom_point(aes(y=yourco,x=median(xvalue)),size=5,color="pink",alpha=1)+
    #Hides Old Axis labels
    theme(plot.title = element_text(hjust=0, size=12),plot.subtitle = element_text(hjust=0, size=12),axis.title=element_text(face="bold",size=14),axis.text.y=element_text(face="bold",size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position="none")+
    #New Axis Label
    labs(title=(lowestcosttitle),subtitle=(lowestrenewtitle),x="Suppliers",y="Cost (blue)")+
    #2nd Y Axis Label/Scaling
    scale_y_continuous(sec.axis= sec_axis(~./max(plotdata$CO),name="CO2 emissions in metric tons per year (Green)"))+
    annotate("text",x=median(xvalue),y=bill+5, label="Your Current Bill")
  return(plotcost)
}


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Energy Optimizer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("zip",
                     "Zipcode:",value=16801,min=NA,max=NA),
        selectInput("dist","Your Current Utility Company:",selected="West Penn Power",list("Citizens' Electric Company","Duquesne Light","Met-Ed","PECO Energy","Penelec","Penn Power","Pike County Light & Power","PPL Electric Utilities","UGI","Wellsboro Electric Company","West Penn Power")),
        numericInput("kwh","Amount of kWh on last bill:",249),
        numericInput("bill","How much was your last month's power bill:",19.2)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plotcost")
        )
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plotcost = renderPlot({
    plot.costco(data=data,kwh=input$kwh,bill=input$bill)
  }, height = 900,width=800)
}
# Run the application 
shinyApp(ui = ui, server = server)

