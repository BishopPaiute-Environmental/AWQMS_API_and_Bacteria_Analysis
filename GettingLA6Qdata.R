library(tidyverse)
library(rvest)
library(janitor)
library(lubridate)
library(pdftools)
library(openxlsx)
library(readxl)
library(plotly)
library(htmlwidgets)
library(viridisLite)
library(RColorBrewer)
library(colorspace)
library(randomcoloR)

setwd("X:/WQCP/R/CODE/DISCHARGE_PLANT_6")

# DATA IS OFF BY A YEAR _ NEEDS TO BE FIXED 
# IMPORT ARCHIVE DATA AND MAKE PLOTS OF IT--------------------------------------------------------

plant6Qarchive <- read_excel("X:/WQCP/DATA/Discharge_Plant6/LA_AqueductArchive.xlsx", sheet = "BishopCr@Plant6", skip = 2, col_names = TRUE,
                             col_types = c("date", "numeric","numeric","date", "numeric","numeric","date", "numeric","numeric",
                                           "date", "numeric","numeric","date", "numeric","numeric","date", "numeric","numeric",
                                           "date", "numeric","numeric","date", "numeric","numeric","date", "numeric","numeric",
                                           "date", "numeric","numeric","date", "numeric","numeric","date", "numeric","numeric",
                                           "date", "numeric","numeric","date", "numeric","numeric","date", "numeric","numeric",
                                           "date", "numeric","numeric","date", "numeric","numeric","date", "numeric"))

plant6Qarchive <- plant6Qarchive %>% 
  select(-c(1:3))
plant6Qarchive <- plant6Qarchive[-1,]

plant6Qarchivecolname <- paste0(rep(c(2007:2023),each=3),"-",c("Date","Q","Blank"))
colnames(plant6Qarchive) <- plant6Qarchivecolname

plant6Qarchive2 <- plant6Qarchive

#for (c in colnames(plant6Qarchive)){
for (c in seq_along(plant6Qarchive2)){

  name <- names(plant6Qarchive2)[[c]]

  if (str_detect(name,"Date")){
    plant6Qarchive2[[c]] <- as.Date(plant6Qarchive2[[c]] , format = "%Y-%m-%d")   
    #name <- names(plant6Qarchive)[[c]]
    colselect <- as.data.frame(plant6Qarchive2[[c]])

    yearselect <- year(colselect[1,])
    colyear <- str_sub(name,1,4)
    colyear <- as.numeric(colyear)
    yeardiff <- colyear-yearselect
    #print(paste(name,yearselect,colyear,yeardiff))
    
    plant6Qarchive2[[c]] <- plant6Qarchive2[[c]] %m+% years(yeardiff)
    
  }

}


 
plant6Qarchive3 <- plant6Qarchive2

#DELETES EVERY 3RD COLUMN
plant6Qarchive3 <- plant6Qarchive3 %>%
   select(everything()[c(TRUE, TRUE, FALSE)])

#ALSO DELETES EVERY 3RD COLUMN, BUT THE HARDER AND LESS GRACEFUL WAY
# plant6Qarchive3 <- plant6Qarchive3 %>% 
#   select(1:2,4:5,7:8,10:11,13:14,16:17,19:20,22:23,25:26,28:29,31:32,34:35,37:38,40:41,43:44,46:47)

plant6Qarchive3 <- plant6Qarchive3[rowSums(is.na(plant6Qarchive3)) != ncol(plant6Qarchive3), ]

plant6Qarchivelong <- NULL

chunk <- 2
#https://stackoverflow.com/questions/44578048/splitting-large-data-frame-by-column-into-smaller-data-frames-not-lists-using
for (i in 1:16) { # i = 1
  xname  <- paste("data", i, sep = "_")
  col.min  <-  (i - 1) * chunk + 1
  col.max  <-  min(i * chunk, ncol(plant6Qarchive3))
  #assign(xname, plant6Qarchive3[,col.min:col.max])
  temp <- plant6Qarchive3[,col.min:col.max]
  
  colnames(temp) <- c("Date","Q")
  
  plant6Qarchivelong <- bind_rows(plant6Qarchivelong,temp)
}

plant6Qarchivelong <- plant6Qarchivelong[rowSums(is.na(plant6Qarchivelong)) != ncol(plant6Qarchivelong), ]

plant6Qarchivelong$Year <- year(plant6Qarchivelong$Date)
plant6Qarchivelong$DOY <- strftime(plant6Qarchivelong$Date, format = "%j")
plant6Qarchivelong$DOY <- as.numeric(plant6Qarchivelong$DOY)

unique(plant6Qarchivelong$DOY)

# col_vector<-c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')
col_vector<-c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6',
               '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3')
plant6QPlot <- ggplot(data = plant6Qarchivelong, aes(x=DOY,y=Q,colour=Year)) +
  #geom_line() +
  geom_line(aes(colour = factor(Year))) + 
  scale_x_continuous(breaks=c(32,92,152,213,274,335), 
                     labels=c("Feb","Apr","June", "Aug","Oct","Dec")) +
  scale_color_manual(values = col_vector) +
  labs(title = "Discharge at LA Aqueduct Plant 6", 
       x = "", 
       y = "Q (cfs)", 
       color = "Year") 
plant6QPlot

plant6QPlotly <- ggplotly(plant6QPlot)
plant6QPlotly

saveWidget(plant6QPlotly,"LAAqueduct_Plant6_Plotly.html")
# GET REAL-TIME DATA ------------------------------------------------------


#WE COULD HAVE THIS CODE SEND AN EMAIL WITH INFO ABOUT WHAT AS DOWNLOADED
#https://mailtrap.io/blog/r-send-email/

#THE 39980 URL SEEMS TO MATCH THE DAILY REPORT VALUE DESCRIBED BY BRY
content <- read_html("https://wsoweb.ladwp.com/Aqueduct/realtime/39980.htm")


tables <- content %>% html_table(fill = TRUE)
plant6Qrealtime <- tables[[1]]

plant6Qrealtime <- plant6Qrealtime %>%
   janitor::row_to_names(1)

plant6Qrealtime <- rename(plant6Qrealtime,DateTime = `Date/Time`)

plant6Qrealtime$Reading <- as.numeric(plant6Qrealtime$Reading)

plant6Qrealtime$DateTime <- mdy_hms(plant6Qrealtime$DateTime)

plant6Qrealtime <- plant6Qrealtime %>%
  mutate(Month = month(DateTime),
         Day = day(DateTime),
         Year = year(DateTime),
         Date = date(DateTime))

realtimedates <- unique(plant6Qrealtime$Date)

rtdmeans <- NULL

for (rtd in realtimedates) {
  rtdmean <- NULL
  rtdmeans1 <- NULL
  
  plant6QrealtimeF <- plant6Qrealtime %>%
    filter(Date == rtd)
  
  rtdnumreading <- count(plant6QrealtimeF)
  
  rtdmaxtime <- as.numeric(format(max(plant6QrealtimeF$DateTime),"%H"))
  
  if(rtdmaxtime >= 22){
      rtdmean <- mean(plant6QrealtimeF$Reading)
      
      rtdmeans1$Date <- unique(plant6QrealtimeF$Date)
      rtdmeans1$MeanQ <- rtdmean
      
  }#if(rtdmaxtime >= 11)
  rtdmeans <- bind_rows(rtdmeans,rtdmeans1)
  
}#for (rtd in realtimedates)

#Saves the file ONLY the first time when it does not already exist
if(file.exists("LA_AqueductData_DailyMeansFromRealTime_MASTER.rds")=="FALSE"){
  
  old <- rtdmeans
  save(old,file = "LA_AqueductData_DailyMeansFromRealTime_MASTER.rds")

  }



# Get existing data and append new data
load("LA_AqueductData_DailyMeansFromRealTime_MASTER.rds")
new <- bind_rows(old,rtdmeans)
new <- new %>%
  distinct(.keep_all = TRUE)
save(new,file = "LA_AqueductData_DailyMeansFromRealTime_MASTER.rds")
wball <- createWorkbook()
addWorksheet(wball,"BCatPlant6DailyMeanQ")
writeData(wball,"BCatPlant6DailyMeanQ",new,startCol = 1,startRow = 1, colNames = TRUE)
## openxlsx converts columns of class 'Date' to Excel dates with the format
## given by
getOption("openxlsx.dateFormat", "mm/dd/yyyy")
## this can be set via (for example)
options(openxlsx.dateFormat = "yyyy/mm/dd")
## custom date formats can be made up of any combination of: d, dd, ddd, dddd,
## m, mm, mmm, mmmm, mmmmm, yy, yyyy

## numFmt == 'DATE' will use the date format specified by the above
addStyle(wball, 1, style = createStyle(numFmt = "DATE"), rows = 1:5000, cols = 1, gridExpand = TRUE)
saveWorkbook(wball,"LA_AqueductData_DailyMeansFromRealTime_MASTER.xlsx", overwrite = TRUE)


# GET PDFS ----------------------------------------------------------------


daterange <- seq(as.Date(Sys.Date()-14),as.Date(Sys.Date()), by="days")

daterange <- format(daterange,'%Y_%m_%d')

BCatPlant6ALL <- NULL
for (d in daterange){

  BCatPlant6ALL1 <- NULL
  
  link <-  paste0("https://www.ladwp.com/cs/idcplg?IdcService=GET_FILE&dDocName=NDR",
                  d, 
                  ".PDF&RevisionSelectionMethod=LatestReleased")
  download.file(link, paste0("LA_AqueductData_Report_",d,".pdf"), mode = "wb")
  
  filename <- paste0("LA_AqueductData_Report_",d,".pdf")
  pdfdata <- as.data.frame(pdf_data(filename))
  BCatPlant6 <- pdfdata[154,6]
  
  BCatPlant6ALL1$Date <- d
  BCatPlant6ALL1$MeanQ <- BCatPlant6
  
  BCatPlant6ALL <- bind_rows(BCatPlant6ALL,BCatPlant6ALL1)
  
}
#rm(oldreport)
#Saves the file ONLY the first time when it does not already exist
if(file.exists("LA_AqueductData_DailyMeansFromReports_MASTER.rds")=="FALSE"){
  
  oldreport <- BCatPlant6ALL
  save(oldreport,file = "LA_AqueductData_DailyMeansFromReports_MASTER.rds")
  
  while (!file.exists("LA_AqueductData_DailyMeansFromReports_MASTER.rds")) {
    Sys.sleep(1)
  }
  
}

load("LA_AqueductData_DailyMeansFromReports_MASTER.rds")
newreport <- bind_rows(oldreport,BCatPlant6ALL)
newreport <- newreport %>%
  distinct(.keep_all = TRUE)
save(new,file = "LA_AqueductData_DailyMeansFromReports_MASTER.rds")
wballreport <- createWorkbook()
addWorksheet(wballreport,"BCatPlant6DailyMeanQReport")
writeData(wballreport,"BCatPlant6DailyMeanQReport",newreport,startCol = 1,startRow = 1, colNames = TRUE)
saveWorkbook(wballreport,"LA_AqueductData_DailyMeansFromReports_MASTER.xlsx", overwrite = TRUE)

