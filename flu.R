rm(list = ls())
library(fiftystater)
library(mapdata)
library(maps)
library(ggplot2)
library(ggmap)
library(twitteR)
library(httr)
library(dplyr)
library(sp)
library(maptools)
setwd("D:\\OneDrive\\study\\data intensive computing\\L1P3\\twitteR_flu")
flu.data <- read.csv("flu.csv")
head(flu.data)

##########################
# get raw data from twitter
##########################
setup_twitter_oauth(consumer_key  = "***********", 
                    consumer_secret = "**********************")

fludata.raw.1 <- twListToDF(searchTwitter("flu", n = 1800))
write.table(fludata.raw.1, "flu_raw.csv", sep = ",", row.names = F, col.names = T)
id.tail <- tail(fludata.raw.1$id, 1)
while(TRUE){
  fludata.raw <- twListToDF(searchTwitter("flu", n = 1800, maxID  = id.tail))
  id.tail <- tail(fludata.raw$id, 1)
  write.table(fludata.raw, "flu_raw.csv", sep = ",", row.names = F, col.names = F,append = T)
}

###############################
# classify data into with 
# location and without location
###############################

#with location
fludata.all <- read.csv("flu_raw.csv", header = T,  sep = ",",quote = "")
fludata.format <- fludata.all[fludata.all$X == "",]
fludata.format <- fludata.format[,1:16]
write.table(fludata.format[grepl("^[[:digit:]]+",fludata.format$latitude),],
            "flu.csv", sep = ",", row.names = F, col.names = F, append = T)

#without location
fludata.noL <- fludata.format[is.na(fludata.format$latitude) & is.na(fludata.format$longitude),]

################################
#  try to get location from user
################################
users <- c()
# for(i in 48737:nrow(fludata.noL)){
#   remaining <- as.numeric(getCurRateLimitInfo("users")[4,3])
#   if(remaining<5){
#     reset <- as.POSIXct(getCurRateLimitInfo("users")[4,4], tz = "UTC")
#     attributes(reset)$tzone <- "EST"
#     Sys.sleep(difftime(reset, Sys.time(), units = "secs"))
#   }
#   users <- c(users, getUser(fludata.noL$screenName[i]))
# }#48736
i<-61602
while(i < nrow(fludata.noL)){
  rate <- getCurRateLimitInfo("users")[10,]
  remaining <- as.numeric(rate[3])
  if(remaining<5){
    reset <- as.POSIXct(rate[4], tz = "UTC")
    attributes(reset)$tzone <- "EST"
    Sys.sleep(difftime(reset, Sys.time(), units = "secs"))
  }
  users <- c(users, lookupUsers(users = as.character(fludata.noL$screenName[i:i+2000]),includeNA = T))
  i <- i+2001 
}


#write loc into csv
loc <- c()
for(j in 1:length(users)){
  loc <- c(loc, users[[j]]$location)
}
write.table(loc, "tempLoc.csv", row.names = F, append = T, col.names = F)

#get states
temp.loc <- read.csv("tempLoc.csv", header = F)
state.names <- paste(c(state.name,state.abb), collapse = "|")
m <- regexpr(state.names, temp.loc$V1, perl=TRUE)
loc.match <- regmatches(x = temp.loc$V1,m = m) ####### location list

loc.ful <- state.name[match(loc.match, state.name)]
loc.ful <- loc.ful[!is.na(loc.ful)]

loc.abb <- state.abb[match(loc.match, state.abb)]
loc.abb <- loc.abb[!is.na(loc.abb)]

loc.ful <- c(loc.ful, state.name[match(loc.abb, state.abb)])
loc.ful <- tolower(loc.ful)

###########################
# get state from location
###########################

position <- flu.data[,15:16]
states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
states.spacial <- map2SpatialPolygons(states, IDs=states$names,
                                      proj4string=CRS("+proj=longlat +datum=wgs84"))
position.spacial <- SpatialPoints(position, 
                                  proj4string=CRS("+proj=longlat +datum=wgs84"))
indices <- over(position.spacial, states.spacial)

loc.match.2 <- sapply(states.spacial@polygons, function(x) x@ID)[indices]
loc.match.2 <- loc.match.2[!is.na(loc.match.2)]

##########################################################
# save the location distribution information into csv file
# temperarily
##########################################################
flu.loc <- c(loc.ful, loc.match.2)
write.table(flu.loc, "flu_LOC.csv", row.names = F, col.names = F)

flu.loc <- read.table("flu_LOC.csv")$V1
head(flu.loc)
flu.loc <- c(as.character(flu.loc), tolower(state.name))
plot.data <- as.data.frame(table(flu.loc))
write.table(plot.data,"plotData.csv", sep = ",", row.names = F, col.names = T)
###########################
# plot geoMap
###########################

level.color = c("yellowgreen","moccasin","darkorange","darkorange2","red2","red4")
X11()
ggplot(plot.data, aes(map_id = flu.loc)) + 
  geom_map(aes(fill = Freq), map = fifty_states) +
  scale_fill_gradientn(colors = level.color) +
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "right", 
        panel.background = element_blank())+
  ggtitle("Flu distribution according to tweets")+
  borders("state",colour = "black")











