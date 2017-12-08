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

##Need to change this for package.
data=read.csv("16801WestPennPower.csv",header=TRUE,sep=",")

#Temp disabled.  Using different co2 formula
cf_choose = function(energytype,kwh){
  cf_coal = (63.729)
  cf_naturalgas = (25.136*kwh)
  cf_oil = (.750*kwh)
  cf_renewable = 0
  if (energytype == "Oil"){return(cf_oil)}
  else if (energytype =="Natural Gas"){return(cf_naturalgas)}
  else if (energytype == "Coal"){return(cf_coal)}
  else if (energytype == "Renewable"){return(cf_renewable)}
}

#Resorts Data for plotting + converts price-per-kilowatt to price + Calculates total CO2 in kg
sort.data = function(data,kwh,bill){
  #Reorders datatable by Price (least to highest) and reindexes row numbers
  data = data[order(data$Price),];rownames(data) = 1:nrow(data)
  #This calculates CO2 and adds to dataframe.  Can be referenced as data$CO
  for(i in 1:length(data$Type.1)){
    data$CO[i]=(cf_choose(data$Type.1[i],kwh))
  }
  #Converts Price per kwh to total cost.  Can remove if needed.
  data$Price = data$Price *kwh
  return(data)
} 


#Plots ggplot of cost/total co/highlights your bill and lowest cost/ lowest renewable cost
plot.costco=function(data,kwh,bill){
  #Resorting data into workable format
  plotdata = sort.data(data,kwh,bill)
  #X values for graph since data is already sorted by price.
  xvalue = 1:nrow(data)
  ##Finds the lowest renewable: 
  ##Returns a dataframe with 1 row
  renewable=subset(data,Renewable == "Y")
  lowestrenew = renewable[which(plotdata==min(plotdata$Price)),]
  lowestrenewtitle = paste("The Lowest Renewable Supplier is:",lowestrenew$Supplier,"@ $",lowestrenew$Price)
  ##Lowest Cost
  lowestcost = plotdata[which(plotdata==min(plotdata$Price)),]
  lowestcosttitle = paste("The Lowest Cost Supplier is:",lowestcost$Supplier,"@ $",lowestrenew$Price)
  ##TO BE ADDED: Color hack so it shows lowest cost/lowest renewable/other data
  ##plotdata$color=ifelse(rownames(plotdata) ==(lowestrenew|lowestcost),"True","False")
  
  #Plot function:
  plotcost=ggplot(plotdata)+
    #Price Bar
    geom_bar(stat="identity", aes(x=reorder(xvalue,-Price),y=Price),color="black",fill="blue")+
    #CO2 Bar.  CO is divided by 1000 so visually it doesn't mess up the graph.  Axis ticket marks below multiplied by 1000 to compensate
    geom_bar(stat="identity",aes(x=xvalue,y=CO/1000),color="green")+
    #Your current Bill Point
    #geom_point(aes(y=bill,x=median(xvalue)),size=5,color="red")+
    #Your estminated carbon footprint as point
    #geom_point(aes(y=yourco,x=median(xvalue)),size=5,color="pink",alpha=1)+
    #Hides Old Axis labels
    theme(plot.title = element_text(hjust=0, size=12),plot.subtitle = element_text(hjust=0, size=12),axis.title=element_text(face="bold",size=14),axis.text.y=element_text(face="bold",size=12),axis.text.x=element_blank(),axis.ticks.x=element_blank(),legend.position="none")+
    #New Axis Label
    labs(title=(lowestcosttitle),subtitle=(lowestrenewtitle),x="Suppliers",y="Cost (blue)")+
    #2nd Y Axis Label/Scaling
    scale_y_continuous(sec.axis= sec_axis(~./max(plotdata$CO)*1000,name="CO2 emissions in pounds per year (Green)"))+
    #Text for your current bill
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

