library(dplyr)
library(ggmap)
library(ggplot2)
library(readr)
library(data.table)
library(reshape2)
library(lubridate)


#Goals for This
# 1. Make map of SF by Crime Data
# -- Split into a few groups
# -- map the individual groups that we split into
# -- done and profit!
# 2. Make map of Marquette and certain restaurants
# 3. Make map of Marquette based off web scraping for restaurant
#
#
#
#

#Use this to save a map to disk
initMap <- function() {
  mapData<-get_map(location=c(lon=-122.4193,lat=37.77474),zoom=12,filename=getwd()) #get map in this location
  save(mapData, file=paste(getwd(),"mapData.rda",sep="/")) # save the map in the current directory
  print(getwd())
  
}

setSFDir <- function() {
  setwd("C:/Users/Sam/Projects/Kaggle/SFCrimeData/")
}

#Description - Loads the map which can be used by ggplot. If the parameters are not set, the default case is to load a map of SF
#Returns - the SF Map or map specified by coordinates
loadMap <- function(centerLong=-122.4193,centerLat=37.77474,mapZoom=12,mapFilename="mapData.rda",mapName="mapData") {
  if (file.exists(mapFilename)) {
    load(mapFilename)
  }
  else {
    mapData <-get_map(location = c(lon=centerLong,lat=centerLat),zoom=mapZoom)
    save(mapData, file=paste(getwd(),mapFilename,sep="/")) # save the map in the current directory
  }
  return(mapData)
}


standardMap <- function() {
  train <- read.csv("train/train.csv") #open CSV file
  #mapSF <- qmap("San Francisco", zoom = 12, color = "bw") #map <- readRDS("../input/sf_map_copyright_openstreetmap_contributors.rds")
  #mapData<-get_map(location=c(lon=-122.4193,lat=37.77474),zoom=12) #get map in this location
  mapData<- loadMap()
  ggmap(mapData)
   counts <- summarise(group_by(train, Category), Counts=length(Category)) # groups each category by it's count
   counts <- counts[order(-counts$Counts),] # sorts categories by their count
   # This removes the "Other Offenses" category
   top12 <- train[train$Category %in% counts$Category[c(1,3:13)],]  #top 12 categories in a data frame
   p <- ggmap(mapData) +
     geom_point(data=top12, aes(x=X, y=Y, color=factor(Category)), alpha=0.05) +
     guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                  title="Type of Crime")) +
     scale_colour_brewer(type="qual",palette="Paired") + 
     ggtitle("Top Crimes in San Francisco") +
     theme_light(base_size=20) +
     theme(axis.line=element_blank(),
           axis.text.x=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks=element_blank(),
           axis.title.x=element_blank(),
           axis.title.y=element_blank())
   ggsave("sf_top_crimes_map.png", p, width=7, height=5, units="in")
}

#Descriiption loads and saves a smaller version of the data frame
shrinkDF <- function(n=50000,filename="train/train.csv",smallFileName="train/TrainSmall.csv") {
  train <- read.csv(filename) #open CSV file
  trainSmall <- train[sample(nrow(train), n), ]
  write.csv(trainSmall,file=smallFileName)
}

customMap <- function() {
  train <- read.csv("train/trainSmall.csv") #open CSV file
  mapData<-loadMap() #get map in this location
  counts <- summarise(group_by(train, Category), Counts=length(Category)) # groups each category by it's count
  # This removes the "Other Offenses" category
  sexOffense <- train[train$Category %in% counts$Category[c(29,30)],] #sex offense forcible and not forcible
  p <- ggmap(mapData) +
    geom_point(data=sexOffense, aes(x=X, y=Y, color=factor(Category)), alpha=0.2) +
    guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                 title="Type of Crime")) +
    scale_colour_brewer(type="qual",palette="Set1") + 
    ggtitle("Top Crimes in San Francisco") +
    theme_light(base_size=20) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ggsave("sf_random_crimes_map4.png", p, width=11.2, height=8, units="in")
}


customCat <- function() {
  train <- read.csv("train/trainSmall.csv") #open CSV file
  mapData<-loadMap() #get map in this location
  counts <- summarise(group_by(train, Category), Counts=length(Category)) # groups each category by it's count
  # This removes the "Other Offenses" category
  
  theft_crimes=c("BURGLARY","ROBBERY","VEHICLE THEFT","TRESPASS","STOLEN PROPERTY","LARCENY/THEFT")
  
  white_collar_crime=c("FORGERY/COUNTERFEITING","FRAUD","EXTORTION","BRIBERY","EMBEZZLEMENT","BAD CHECKS")
  
  dangerous_crimes=c("KIDNAPPING","SEX OFFENSES FORCIBLE","ARSON","ASSAULT")
  
  disorderly_conduct=c("DRIVING UNDER THE INFLUENCE","LIQUOR LAWS","DRUG/NARCOTIC","LOITERING","DRUNKENESS")
  
  "%ni%" <- Negate("%in%")
  
  train=data.table(train)
  train[, crimeType := character(nrow(train))]
  
  
  train[train$Category %in% theft_crimes, crimeType:="THEFT"]
  train[train$Category %in% white_collar_crime, crimeType:="WHITE COLLAR"]
  train[train$Category %in% dangerous_crimes, crimeType:="DANGEROUS"]
  train[train$Category %in% disorderly_conduct, crimeType:="CONDUCT"]
  train[train$Category %ni% theft_crimes & train$Category %ni% white_collar_crime& train$Category %ni% dangerous_crimes & train$Category %ni% disorderly_conduct, crimeType:="OTHER"]
  
  category = prompt()
  if (is.null(category)) return("No prompt selected") 
  else if (length(category)>1) fileName="ALL"
  else fileName= category[1]
  
  p <- ggmap(mapData) +
    geom_point(data=train[train$crimeType %in% category], aes(x=X, y=Y, color=factor(crimeType)), alpha=0.1) +
    guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0),
                                 title="Type of Crime")) +
    scale_colour_brewer(type="qual",palette="Set1") + 
    ggtitle(paste("Top Crimes in San Francisco n (", nrow(train[train$crimeType %in% category,]) ,")",sep="")) +
    theme_light(base_size=20) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ggsave(paste("sf_random_crimes_cust_cat",fileName,".png",sep=""), p, width=11.2, height=8, units="in")
}

prompt <- function() {
  n=0
  while(!(n %in% c(1,2,3,4,5))){
    print("Enter the number for the map you would like to create: ")
    print("1. Theft")
    print("2. White Collar")
    print("3. Dangerous")
    print("4. Poor Conduct")
    print("5. All")
    n <- readline("Enter the number for the map you would like to create: ")
    n <- ifelse(grepl("\\D",n),-1,as.integer(n))
    if(is.na(n)){break}  # breaks when hit enter
  }
  if (n==1) cat=c("THEFT")
  else if (n==2) cat=c("WHITE COLLAR")
  else if (n==3) cat=c("DANGEROUS")
  else if (n==4) cat=c("CONDUCT")
  else if (n==5) cat=c("THEFT","WHITE COLLAR","DANGEROUS","CONDUCT")
  else cat=""
  return(cat)
  
}

marquetteRest <- function() {
  
  marquetteRestData <- read.csv("marquetteRest.csv")
  mapData=loadMap(centerLong = -87.43,centerLat = 46.54751,mapZoom=13,mapFilename = "marquette.rda",mapName="mapData")
  
  
  p <- ggmap(mapData) +
    geom_point(data=marquetteRestData, aes(x=x, y=y, color=factor(Name)), alpha=1) +
    guides(colour = guide_legend(override.aes = list(alpha=1, size=10.0),
                                 title="Type of Crime")) +
    scale_colour_brewer(type="qual",palette="Set1") + 
    ggtitle("Marquette Restaurants") +
    theme_light(base_size=20) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  ggsave("MarquetteRestaurants.png", p, width=11.2, height=8, units="in")
  
  
  
}

# crimeMapOutcome() <- function() {
#   #make sure that Date is properly formatted file fetching training data
#   train <- read_csv("train/train.csv" ,col_types = list(Dates = col_datetime()))
#   
#   #just get the date(YYYY-mm-dd) to get daily rates
#   train <- mutate(train, Date_y_m_d = as.POSIXct(strftime(Dates, format="%Y-%m-%d 00:00:00")))
#   
#   # Number of offences per day
#   all_offenses_per_day <- summarise(group_by(train, Date_y_m_d), Offences_Per_Day=length(Date_y_m_d))
#   
#   p=ggplot(all_offenses_per_day ,aes(y=Offences_Per_Day,x=Date_y_m_d))+
#     geom_line(size = 1,color="green")+
#     labs(y = "Offences per/day", x="Date", title="San Francisco Offence Rate")+
#     geom_smooth(method="lm")
#   
#   ggsave("OffenceRate.png", p, width=14, height=10, units="in")
#   
#   
#   
#   train_true_crime <- filter(train, Category != "OTHER OFFENSES", Category != "NON-CRIMINAL")
#   
#   # Number of crime per day
#   crimes_per_day <- summarise(group_by(train_true_crime, Date_y_m_d), Count_Per_Day=length(Date_y_m_d))
#   
#   p=ggplot(crimes_per_day,aes(y=Count_Per_Day,x=Date_y_m_d))+
#     geom_line(size = 1,color="blue")+
#     labs(y = "Crime per/day", x="Date", title="San Francisco Crime Rate")+
#     geom_smooth(method="lm")
#   
#   ggsave("CrimeRate.png", p, width=14, height=10, units="in")
#   
#   
#   #get all the crimes have no resolution
#   #tolower to get even the rows with variations of none (NONE, None, none, etc...)
#   train_no_resolution <- filter(train_true_crime, tolower(Resolution) == "none")
#   
#   crimes_not_resolved <- summarise(group_by(train_no_resolution, Date_y_m_d), Count_Per_Day=length(Date_y_m_d))
#   
#   p=ggplot(crimes_not_resolved,aes(y=Count_Per_Day,x=Date_y_m_d))+
#     geom_line(size = 1,color="red")+
#     labs(y = "Unresolved per/day", x="Date", title="San Francisco Unresolved Case Rate")+
#     geom_smooth(method="lm")
#   
#   ggsave("UnresolvedCase.png", p, width=14, height=10, units="in")
#   
# }

propertyCrime <- function() {
  
  # Read in the data, but we need Dates
  train <- read_csv("train/train.csv" ,col_types = list(Dates = col_datetime()))
  
  train <- filter(train, Category != "NON-CRIMINAL", Category != "WARRANTS")
  train$Year <- year(ymd_hms(train$Dates))
  
  crimes_per_year <- table(train$Category,train$Year)
  
  crimes_per_year <- melt(crimes_per_year)
  
  names(crimes_per_year) <- c("Category","Year","Count")
  
  crimes_per_year=data.table(crimes_per_year)
  
  
  #according to lawyers.com and findlaw.com
  crime_against_property=c("VEHICLE THEFT", "ROBBERY", "BURGLARY", "FRAUD", "ARSON", "TRESPASS",
                           "LARCENY/THEFT", "FORGERY/COUNTERFEITING", "RECOVERED VEHICLE", 
                           "FRAUD",  "STOLEN PROPERTY", "BAD CHECKS" )
  
  crime_against_person=c("ASSAULT", "RUNAWAY", "SUICIDE", "FAMILY OFFENSES", "MISSING PERSON", 
                         "SEX OFFENSES FORCIBLE", "SEX OFFENSES NON FORCIBLE", "KIDNAPPING", "EXTORTION", "EMBEZZLEMENT")
  
  crime_against_public_order=c("DRIVING UNDER THE INFLUENCE", "DISORDERLY CONDUCT", "DRUNKENNESS",
                               "LOITERING", "LIQUOR LAWS", "WEAPON LAWS", "DRUG/NARCOTIC", 
                               "PROSTITUTION", "BRIBERY", "PORNOGRAPHY/OBSCENE MAT", "VANDALISM", "GAMBLING", "SUSPICIOUS OCC")
  
  crime_against_other=c( "OTHER OFFENSES", "SECONDARY CODES", "TREA")
  
  
  "%ni%" <- Negate("%in%")
  
  crimes_per_year[, Against := character(nrow(crimes_per_year))]
  
  crimes_per_year[Category %in% crime_against_property, Against := "PROPERTY"]
  crimes_per_year[Category %in% crime_against_person, Against := "PERSON"]
  crimes_per_year[Category %in% crime_against_public_order, Against := "PUBLIC ORDER"]
  crimes_per_year[Category %in% crime_against_other, Against := "OTHER"]
  
  g <- ggplot(crimes_per_year,aes(x=Year, y=Count,fill = Against)) + 
    geom_bar(stat = "identity")+
    coord_flip() +
    facet_grid(.~Against) +
    labs(title="Most crimes committed in SF are against Properties, Not People")
  
  
  ggsave("Most_Crimes_On_Properties_Not_People.png", g, width=14, height=10, units="in")
  
}