##This script finds all the distributors for the corresponding Zip Code
##See Readme for more details

#setwd("C:/Users/PC/Desktop/zipdata")
#Original csv containing all zipcodes from PA for reference/input.  Deleted/replaced with output of this script.
zipcode=read.csv("zips.csv",stringsAsFactors = FALSE,header=TRUE)
#Data writes to this dataframe 
newzip = data.frame(ncol=NA,nrow=NA)
colnames(newzip)=c("Zip","Distributor")
#REQUIRES DOCKER

library(RSelenium)
rsDriver()

##Docker commands for reference
##Docker Start Command:
#docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1
##Docker Stop Commands:
#docker stop $(docker ps -a -q)
#docker rm $(docker ps -a -q)

#Browser Settings
#Change download dir per your settings.
prof = makeFirefoxProfile(list(browser.download.dir = "C:/Users/PC/Desktop/zipdata"
                               , browser.download.folderList = 2L
                               , browser.download.manager.showWhenStarting = FALSE
                               , browser.helperApps.neverAsk.saveToDisk =  "text/csv"))
browser <- remoteDriver(remoteServerAddr = "192.168.99.100" 
                        , port = 4445L
                        , browserName = "firefox"
                        ,extraCapabilities = prof
)
#URL to be scraped
url = 'http://www.papowerswitch.com/shop-for-electricity/shop-for-your-home'
#Opens Website
browser$open()
for (i in 1:length(zipcode$Zip)){
  browser$navigate(url)
  zip = as.character(zipcode$Zip[i])
  #Enters Zipcode
  szip = browser$findElement(using = 'css selector', "#edit-zipcode")
  szip$clearElement()
  szip$sendKeysToElement(list(zip))
  #Clicks Filter
  sfilter = browser$findElement(using = 'css selector', "#edit-submit-residential-rate-search2")
  sfilter$clickElement()
  #Delay here to allow webpage to fully load. 
  Sys.sleep(1.5)
  print(zip)
  #Finds names if there is more than 1 distributor for zipcode
  if (length(browser$findElements(using = 'class', "multi-distributor")) > 0 ){
    results = browser$findElement(using = 'class', "result-list")
    checkresults = results$getElementText()
    distlist = unlist(strsplit(as.character(checkresults),"\n"))
    for (j in 1:length(distlist)){
      newzip = rbind(newzip,data.frame(Zip=zip,Distributor=distlist[j]))
    }
    #Gets distributor name
  }else if(length(browser$findElements(using='class',"distributor-name")) > 0) {
    results1 = browser$findElement(using='class',"distributor-name")
    checkresult1 = results1$getElementText()
    distlist1 = unlist(strsplit(as.character(checkresult1),"\n"))
    newzip = rbind(newzip,data.frame(Zip=zip,Distributor=distlist1[1]))
    #Gets name if page shows different residental pricing.
    }else if(length(browser$findElements(using = 'class', "multi-ratetype")) > 0){
      results2 = browser$findElement(using='xpath',"//strong")
      distlist2 = results2$getElementText()
      newzip = rbind(newzip,data.frame(Zip=zip,Distributor=as.character(distlist2[1])))
    #If there is no distributor for the zipcode, enters NA
      }else{
        newzip=rbind(newzip,data.frame(Zip=zip,Distributor=NA))
      }
  #Another delay so things don't bug out: Thanks javascript!
  Sys.sleep(1)
  #Delete Cookies due to javascript
  browser$deleteAllCookies()
}
browser$close()
write.csv(newzip,"zipdist.csv")
