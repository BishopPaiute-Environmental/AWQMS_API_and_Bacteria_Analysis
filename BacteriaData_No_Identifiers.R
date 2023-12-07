#### Bishop Paiute Tribe - Environmental Management Office - Water Quality Control Program                            ####
###  R Script to obtain organizational water quality data from AWQMS using an Application Programming Interface (API) ###
###  As well as code to generate geometric means from E coli data Collected per CWA 106 Guidelines                    ###  

## Disclaimer:

# The code provided here is intended for data analysis purposes only. While efforts have been made to ensure its accuracy and reliability, the code is provided 'as is' without any warranty of any kind, either express or implied. 
# This code is offered freely and may be used, modified, or distributed for data analysis purposes by individuals or organizations. However, the original organization, The Bishop Paiute Tribe, its employees, or contributors shall not be held liable for any direct, indirect, incidental, special, exemplary, or consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data, or profits; or business interruption) resulting from the use of this code, even if advised of the possibility of such damage.
# Users are encouraged to review and modify the code to suit their specific needs and are responsible for any consequences of its use.
# By using this code, you agree to these terms and conditions.


getwd()

#Only run newpackages and install.packages(newpackages) if you have not installed packages. YOu only have to do this once on your computer.
#newpackages = c("tidyverse","readr","dplyr","ggplot2","janitor","readxl","stringr","lubridate","rlang","vctrs","rtool",
#                                  "reshape2","dataRetrieval","av","magick","patchwork","jpeg","ggima#                  "plotly","digest","httr","jsonlite","data.table","readxl","ggimage","jpeg","gapminder","EnvStats","zoo","xlsx")
#                 
#install.packages("magick") 

#Load libraries when you start a new R session
library(digest)
library(httr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(readxl)
library(vctrs)
library(rlang)
library(reshape2)
library(dataRetrieval)
library(plotly)
library(av)
library(RColorBrewer)
library(grid)
library(ggimage)
library(jpeg)
library(patchwork)
library(magick)
library(gganimate)
library(gapminder)
library(plotly)
library(EnvStats)
library(zoo)
library(xlsx)
library(htmlwidgets)
library(reticulate)
library(lubridate)



# Access AWQMS and Download Data ------------------------------------------


encryptionKey <- "PLACE_YOUR_ORGANIZATIONS_ENCRYPTION_KEY_HERE" # This 88-character string is a unique identifier for an individual user account within the AWQMS system. You can request the encryption key for your data by emailing awqms@goldsystems.com. 
userLoginName <- "PLACE_YOUR_ORGANIZATIONS_USER_NAME_HERE" # The username is encrypted in the header and will be used for authentication

#The MIN_DATE and MAX_DATE is for the data you want to access from AWQMS
MIN_DATE<-c("10-01-2009")
MAX_DATE<-c("09-30-2023")

SITES<-c("SW-1,SW-2,SW-3,SW-4") #Example of monitoring location identifiers in AWQMS data
uri<-paste0("https://west.gselements.com/api/ResultsVer1?OrganizationIdentifiersCsv=PLACE_YOUR_ORGANIZATIONS_ID_HERE&MonitoringLocationIdentifiersCsv=",SITES,"&MinDate=",MIN_DATE,"&MaxDate=",MAX_DATE)#,"&Characteristic=",CHARACTERISTIC

getUTCTime <- function() {
  time <- Sys.time()
  as.numeric(time)
  attr(time, "tzone") <- "UTC"
  format(time, "%m/%d/%Y %I:%M:%S %p")
}

getSignature <- function(a, b, c, d, encryptionKey) {
  str <- paste(a, b, c, d, sep="")
  print(str)
  base64_enc(hmac(base64_dec(encryptionKey), str, "sha256", raw=TRUE))
}

makeRequest <- function(userLoginName, uri, encryptionKey) {
  time <- getUTCTime()
  signature <- getSignature(userLoginName, time, uri, "GET", encryptionKey)
  print(signature)
  GET(uri,
      add_headers(
        "Content-Type"="application/json",
        "X-UserID"=userLoginName,
        "X-Stamp"=time,
        "X-Signature"=signature
      ))
}

request <- makeRequest(userLoginName, uri, encryptionKey)
request$status_code #if code=403, rerun previous line. Should be 200. 
request <- makeRequest(userLoginName, uri, encryptionKey)
request$status_code #if code=403, rerun previous line. Should be 200.
output <- content(request, "text", encoding = "UTF-8")
data <- fromJSON(output)


# for results

results <- rbindlist(data$Results, idcol = "id", fill = TRUE) #unpacks nested tables in data$ContinuousResults
data_with_results <- merge(data, results, by.x=0, by.y = 'id')          #merges the unpacked data with the data, which will include sties, date etc.
data_with_results$Row.names <- as.numeric(data_with_results$Row.names)
unique(data_with_results$CharacteristicName)

ECOLI<-data_with_results %>% 
  filter(CharacteristicName=="Escherichia coli") %>% 
  filter(StatisticalBaseCode!="Geometric Mean") %>% 
  select(MonitoringLocationIdentifier,StartDate,ResultValue) %>% 
  rename(Site="MonitoringLocationIdentifier",MPN_D="ResultValue",Date="StartDate") %>% 
  arrange(Site,Date)

ECOLI$MPN_D<-str_replace(ECOLI$MPN_D,">","")
ECOLI$MPN_D<-str_replace(ECOLI$MPN_D,"<","")
ECOLI$MPN_D<-as.numeric(ECOLI$MPN_D)
ECOLI$Date<-as.Date(ECOLI$Date)

#write.xlsx(ECOLI, "TEST_GEOMEAN.xlsx")#use if you want to compare R geomean calculator to excel calculations

# Calculate Geometric Mean ------------------------------------------------

remove_a_duplicate<-function(df){
  #find duplicate dates, 
  DATE_DUPS<-WINDOW %>% 
    count(Date) %>% 
    filter(n>1)
  #choose duplicate date that makes most sense
  CHOSEN_DATE<- min(DATE_DUPS$Date) #will eventually choose duplicates
  #choose lowest MPN value for that date 
  WINDOW_2<-WINDOW %>% 
    filter(WINDOW$Date==CHOSEN_DATE)
  #if the values are equal, choose one...else, choose the min
  WINDOW_MIN<- min(WINDOW_2$MPN_D)
  #else:
  WINDOW_WHICH<-which(WINDOW$MPN_D==WINDOW_MIN & WINDOW$Date==CHOSEN_DATE)[1]
  #And Remove it
  WINDOW_3<-slice(WINDOW,-WINDOW_WHICH)
  # WINDOW_3<-WINDOW %>% 
  #   filter(Date!=CHOSEN_DATE |  MPN_D!=WINDOW_MIN) #%>% 
  
  #print(WINDOW_MIN)
  WINDOW_3
}

dates_b<-unique(ECOLI$Date)

dates<-as.POSIXct("dates_b")

d=dates_b

GEOMEAN_DATA<-data.frame("Site"=character(),
                         "Date"=character(),
                         "GMEAN"=numeric(),
                         stringsAsFactors=FALSE)

GEOMEAN_DATA$Date<- as.Date(GEOMEAN_DATA$Date, format = "%Y-%m-%d")

Temp<- GEOMEAN_DATA
Temp[1,]<-NA

SITES<-unique(ECOLI$Site)

for (s in SITES){
  ECOLI_S<-ECOLI %>% filter(Site==s)
  for (d in as.list(dates_b)) {
    
    start<-d - as.difftime(30,unit="days")
    
    WINDOW<-ECOLI_S %>% 
      filter(Date<=d) %>% 
      filter(Date>start)
    n_total<-nrow(WINDOW)
    if (n_total<5) {
      STAT<-NA
    }
    else if(n_total==5){
      STAT<-geoMean(WINDOW$MPN_D) }
    else{
      while (nrow(WINDOW)>5) {
        WINDOW<-remove_a_duplicate(WINDOW)
        
      }
      STAT<-geoMean(WINDOW$MPN_D)
      
    }
    Temp$Site<-s
    Temp$Date<-d
    Temp$GMEAN<-STAT
    #print(paste("STAT=",STAT))
    GEOMEAN_DATA<-rbind(GEOMEAN_DATA,Temp)
    
  }
  
}

GEOMEAN_DATA$GMEAN<-round(GEOMEAN_DATA$GMEAN,digits=1)

#DOES NOT INCLUDE TU SU or Pond-1 or Pond-3
GEOMEAN_DATA2<- GEOMEAN_DATA %>% 
  filter(GEOMEAN_DATA$Site != "TuSu" ) 

GEOMEAN_DATA2<-GEOMEAN_DATA2%>% 
  filter(GEOMEAN_DATA2$Site != "Pond-1" ) 
GEOMEAN_DATA2<-GEOMEAN_DATA2%>% 
  filter(GEOMEAN_DATA2$Site != "Pond-3" )

# Graph Formatting ---------------------------------------------------


#CHANGE DATE LIMITS FOR NEW YEAR
date_lims<-as.Date(c("2009-10-01","2023-09-30"))


#CHANGE IF YOUR LIMITS ARE OUT OF SCALE
bact_lims<-c(0,500) #Geometric mean data
bact_lims2<-c(0,2500) #Discrete data

##For the following, do NOT delete \n. This symbol splits the label/title onto two lines.

#CHANGE THE TITLE if using discrete data 
PLOT_TITLE<-"[Place grpah title here]" 
PLOT_TITLE2<-"[Place graph title here]" 

#CHANGE LABEL if using discrete data
Y_LABEL<- "[Place Y-axis label]" 
Y_LABEL2<- "[Place y-axis title here for 2nd graph]" 

#CHANGE DATE UPDATED
DATE_UPDATED<-"Updated 09/25/2023-BH"  

#CHANGE FILE NAME IF DOING IRRIGATION SITES
FILE_NAME<-"filename.jpg" 
FILE_NAME2<-"filename2.jpg"

#CHANGE POINT SHAPES
P_SHAPES<- c(21,21,22,24,24,23,23) #SW-1, SW-2, SW-3, SW-4
P_SHAPES2<- c(21,21,22,24,24,23,23,25) #SW-1, SW-2, SW-3, SW-4
P_SHAPES3<- c(21,21,22,22,22,24,24,23,23,25) #SW-1, SW-2, SW-3, SW-4
#COLORS OF POINTS
COLORS<- c("cornflowerblue","dodgerblue4","darkolivegreen3","dodgerblue1","chartreuse3","darkgreen","mediumblue")#SW-1, SW-2, SW-3, SW-4
COLORS2<- c("cornflowerblue","dodgerblue4","darkolivegreen3","dodgerblue1","chartreuse3","darkgreen","mediumblue","cyan3")#SW-1, SW-2, SW-3, SW-4
COLORS3<- c("cornflowerblue","dodgerblue4","darkolivegreen3","deeppink","deeppink4","dodgerblue1","chartreuse3","darkgreen","mediumblue","cyan3")#SW-1, SW-2, SW-3, SW-4




# Geometric Mean Graph ----------------------------------------------------
#jpg version
BACT<-ggplot(GEOMEAN_DATA2,aes(x=Date,y=GMEAN))+geom_point(aes(shape=Site,fill=Site,stroke=1.5),size=4)+
  scale_x_date(date_labels="%m/%d/%y",limits = date_lims,breaks = "1 month")+
  geom_hline(aes(yintercept=126,linetype="Tribal WQS 126 MPN"),color="red",size=1.25)+
  scale_y_continuous(limits=bact_lims)+expand_limits(y=0)+ 
  scale_linetype_manual(name="",values = c("solid"))+theme_minimal()+
  labs(title=PLOT_TITLE,y=Y_LABEL,x="Date",caption=DATE_UPDATED)+
  theme(legend.position = "bottom",plot.title = element_text(face="bold",hjust=0.5),legend.box="horizontal")+
  scale_fill_discrete(name="Site")+ 
  theme(text=element_text(size=16),axis.text = element_text(size=16),axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values= COLORS)+
  scale_shape_manual(values=P_SHAPES)#CHANGE or COMMENT OUT if necessary. Guide to point shapes:http://www.sthda.com/english/w

BACT

ggsave(file=FILE_NAME,width = 13,height = 10, units = "in")

#pdf version - we need to have it land in the right place and format the page size
# pdf("testgeomeangraph.pdf",width = 11, height = 8.5)
# ggplot(GEOMEAN_DATA2,aes(x=Date,y=GMEAN))+geom_point(aes(shape=Site,fill=Site,stroke=1.5),size=4)+
#   scale_x_date(date_labels="%m/%d/%y",limits = date_lims,breaks = "1 month")+
#   geom_hline(aes(yintercept=126,linetype="Tribal WQS 126 MPN"),color="red",size=1.25)+
#   scale_y_continuous(limits=bact_lims)+expand_limits(y=0)+
#   scale_linetype_manual(name="",values = c("solid"))+theme_minimal()+
#   labs(title=PLOT_TITLE,y=Y_LABEL,x="Date",caption=DATE_UPDATED)+
#   theme(legend.position = "bottom",plot.title = element_text(face="bold",hjust=0.5),legend.box="horizontal")+
#   scale_fill_discrete(name="Site")+
#   theme(text=element_text(size=16),axis.text = element_text(size=16),axis.text.x = element_text(angle = 45))+
#   scale_fill_manual(values= COLORS)+
#   scale_shape_manual(values=P_SHAPES)#CHANGE or COMMENT OUT if necessary. Guide to point shapes:http://www.sthda.com/english/w
# dev.off()


# Read in EMO Logo and Graph Image ----------------------------------------

#You must have a file in your working directory in order to read this in.
EMO<-image_read("EMO.jpg") 
EMO<-EMO %>% image_resize("400X400") #Resize in Pixels
B<-image_read(FILE_NAME)

BACT_EMO<-image_composite(B,EMO,offset = "+3450+15")

image_write(BACT_EMO,FILE_NAME)



#PLOTLY Graphs:
#Geometric mean graphs for all creek/pond sites
figBACT<-plot_ly(data=GEOMEAN_DATA,x=~Date,y=~GMEAN,color=~Site,symbol=~Site, symbols = c(P_SHAPES3),colors = COLORS3,
                 marker=list(size=15, line = list(color = 'black',width = 2))) %>% 
  layout(shapes=list(type='line', x0= min(date_lims), x1= max(date_lims), y0=126, y1=126, 
                     line=list(dash='solid', width=2,color='red',label='126'))) %>% 
  layout(title=PLOT_TITLE,yaxis = list(title = Y_LABEL,range=c(0,1250)))%>% 
  add_annotations(
    x= "2023-04-01",
    y= 126,
    # xref = "x",
    # yref = "y",
    text = "Tribal WQS 126 MPN",
    # showarrow = T,
    ax = 20,
    ay = -40
  )
figBACT

htmlwidgets::saveWidget(figBACT, "Place_file_directory_location")

#Discrete data graphs for all creek/pond sites
figBACT2<-plot_ly(data=ECOLI,x=~Date,y=~MPN_D,color=~Site,symbol=~Site, symbols = c(P_SHAPES3),colors = COLORS3,
                  marker=list(size=15, line = list(color = 'black',width = 2))) %>% 
  layout(shapes=list(type='line', x0= min(date_lims), x1= max(date_lims), y0=126, y1=126, 
                     line=list(dash='solid', width=2,color='red',label='126'))) %>% 
  layout(title=PLOT_TITLE2,yaxis = list(title = Y_LABEL2,range=c(0,1250)))%>% 
  add_annotations(
    x= "2022-10-01",
    y= 126,
    # xref = "x",
    # yref = "y",
    text = "Tribal WQS 126 MPN",
    # showarrow = T,
    ax =-50,
    ay = -45
  )
figBACT2
htmlwidgets::saveWidget(figBACT2, "Place_file_location_directory_here")

# Raw Data Graphs (Optional) ----------------------------------------------

# BACT2<-ggplot(ECOLI,aes(x=Date,y=MPN_D))+geom_point(aes(shape=Site,fill=Site,stroke=1.5),size=4)+
#   scale_x_date(date_labels="%m/%d/%y",limits = date_lims,breaks = "1 month")+
#   geom_hline(aes(yintercept=126,linetype="Tribal WQS 126 MPN"),color="red",size=1.25)+
#   scale_y_continuous(limits=bact_lims2)+expand_limits(y=0)+ 
#   scale_linetype_manual(name="",values = c("solid"))+theme_minimal()+
#   labs(title=PLOT_TITLE2,y=Y_LABEL2,x="Date",caption=DATE_UPDATED)+
#   theme(legend.position = "bottom",plot.title = element_text(face="bold",hjust=0.5),legend.box="horizontal")+
#   scale_fill_discrete(name="Site")+ 
#   theme(text=element_text(size=16),axis.text = element_text(size=16),axis.text.x = element_text(angle = 45))+
#   scale_fill_manual(values= COLORS2)+
#   scale_shape_manual(values=P_SHAPES2)#CHANGE or COMMENT OUT if necessary. Guide to point shapes:http://www.sthda.com/english/w
# 
# #ggplotly(BACT2)
# 
# ggsave(file=FILE_NAME2,width = 13,height = 10, units = "in")
#B2<-image_read(FILE_NAME2)

#BACT_EMO2<-image_composite(B2,EMO,offset = "+3450+15")

#image_write(BACT_EMO2,FILE_NAME2)

