##This reads data from zipdist.csv to scrape csv file from link.
## See Readme for more details.
library(RSelenium)
rsDriver()

#sets working directory for functions
setwd("C:/Users/PC/Desktop/zipdata")
zipdist=read.csv("updatezip.csv",stringsAsFactors = FALSE,header=TRUE)

##Docker commands for reference:
#docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
#Have emulated browser save to specific folder:
#docker run -d -v //c/Users/PC/Desktop/zipdata://home/seluser/Downloads -p 4445:4444 -p 5901:5900 selenium/standalone-firefox-debug:2.53.1
#Shutdown/Reset Docker
#docker stop $(docker ps -a -q)
#docker rm $(docker ps -a -q)

##Browser Settings
#Change download.dir per computer settings
prof = makeFirefoxProfile(list(browser.download.dir = "C:/Users/PC/Desktop/zipdata"
                               , browser.download.folderList = 2L
                               , browser.download.manager.showWhenStarting = FALSE
                               , browser.helperApps.neverAsk.saveToDisk =  "text/csv"))
browser <- remoteDriver(remoteServerAddr = "192.168.99.100" 
                        , port = 4445L
                        , browserName = "firefox"
                        ,extraCapabilities = prof
)



url = 'http://www.papowerswitch.com/shop-for-electricity/shop-for-your-home'
#Function reads zipdist.csv for distributor name and returns appropriate browser element id to use for distributor.

distrib = function(dist){
  if (dist == "CitizensElectric"){
    return(browser$findElement(using = 'id', value="22788"))
  }else if(dist== "DuquesneLight"){
    return(browser$findElement(using = 'id', value="22779"))
  }else if(dist == "PECO"){
    return(browser$findElement(using = 'id', value="22781"))
  }else if(dist== "Penelec"){
    return(browser$findElement(using = 'id', value="22783"))
  }else if(dist == "PennPower"){
    return(browser$findElement(using = 'id', value="22782"))
  }else if(dist == "PikeLight&Power"){
    return(browser$findElement(using = 'id', value="22784"))
  }else if(dist == "PPLElectricUtilities"){
    return(browser$findElement(using = 'id', value="22785"))
  }else if(dist == "Met-Ed"){
    return(browser$findElement(using = 'id', value="22780"))
  }else if(dist == "UGI"){
    return(browser$findElement(using = 'id', value="22786"))
  }else if(dist == "WellsboroElectric"){
    return(browser$findElement(using = 'id', value="22787"))
  }else if(dist == "WestPennPower"){
    return(browser$findElement(using = 'id', value="22778"))
  }
}

#Brute force function to confirm rename of the downloaded file from site to (#zipcode#distname).csv
#Sometimes first function would not work.
#Any feedback on this is appreciated.
confirmrename=function(newfilename){
  while(file.exists("rates.csv")==TRUE){
    file.rename("rates.csv", newfilename)
  }
}


#Opens Website
browser$open()

#Loop navigates website and downloads csv file based on dataframe data for inputs.
for (i in 1:length(zipdist$Zip)){
  browser$navigate(url)
  zip = as.character(zipdist$Zip[i])
  dist = zipdist$Distributor[i]
  #If no distributors exist for the zipcode, goes to next iteration
  if (is.na(dist)==TRUE){
    print("NA")
    next
  }
  #Enters Zipcode
  szip = browser$findElement(using = 'css selector', "#edit-zipcode")
  szip$clearElement()
  szip$sendKeysToElement(list(zip))
  #Pick Fixed
  sFix = browser$findElement(using = 'css selector', "#edit-field-type-value-fixed")
  sFix$clickElement()
  #Clicks Filter
  sfilter = browser$findElement(using = 'css selector', "#edit-submit-residential-rate-search2")
  sfilter$clickElement()
  #Sleep delay cause javascript/browser is lamezo.
  Sys.sleep(1)
  #Picks the distributor if prompted
  if (length(browser$findElements(using = 'class', "multi-distributor")) > 0 ){
    clickdist=distrib(dist)
    clickdist$clickElement()
  }
  #Picks Regular Residental if prompted. Always the first option.
  if(length(browser$findElements(using = 'class', "multi-ratetype")) > 0){
  sres = browser$findElement(using = 'class', "multi-ratetype")
  sres$clickElement()
  }
  #Downloads csv file
  sdl = browser$findElement(using = 'id', value="export-results")
  sdl$clickElement()
  #Delay to allow file to be downloaded.  
  Sys.sleep(2)
  #Renames saved file
  newfilename = paste(zip,dist,'.csv',sep="")
  file.rename("rates.csv", newfilename)
  #Delay to allow file indexing to update.  Would bug out sometimes.
  Sys.sleep(1)
  #Brute Forcing filerename in case it doesn't rename
  confirmrename(newfilename)
  #Deleting cookies
  browser$deleteAllCookies()
  print(paste(i,"Done",zip,dist))
}

browser$close()
rsDriver$server$stop()

