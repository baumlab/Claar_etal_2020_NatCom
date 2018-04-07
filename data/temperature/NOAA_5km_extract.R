
# Load necessary packages
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
require(svMisc)

# Clear the working environment
rm(list=ls())

latlon <- read.csv("data/temperature/disturbance_latlons.csv")
files=list.files("data/temperature/NOAA_5km/",full.names = TRUE)

nc <- nc_open("data/temperature/NOAA_5km/b5km_dhw_20150101.nc")
dhw_full <- ncvar_get( nc, "CRW_DHW")
# get longitude and latitude
lon <- ncvar_get(nc,"lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(nc,"lat")
nlat <- dim(lat)
head(lat)
nc_close(nc)

# dhw_slice <- dhw_full[452,1841]
# lon[451]
# lat[1760]
##########################################HIGH#################################

LonIdx <- 452 # High -157.45
LatIdx <- 1760 # High 2.025 


dhwlist <- list()
for (i in files) {
# Open the netcdf file
nc <- nc_open(i)
dhwlist[[i]] <- ncvar_get( nc, "CRW_DHW")[LonIdx, LatIdx]
# Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
nc_close(nc)
print(i)
Sys.sleep(0.01)
flush.console()
}

dhw <- do.call(rbind, dhwlist)
dhw_high <- data.frame(dhw)
colnames(dhw_high) <- c("id","dhw")

dhw_high$date <- rownames(dhw_high)
dhw_high$date <- gsub(x = dhw_high$date,pattern="data/temperature/NOAA_5km/b5km_dhw_",replacement = "")
dhw_high$date <- gsub(x = dhw_high$date,pattern=".nc",replacement = "")
dhw_high$date <- as.POSIXct(dhw_high$date,format="%Y%m%d")

dhw_high_max <- max(dhw_high$dhw)
dhw_high[which(dhw_high$dhw==max(dhw_high$dhw)),]

##################################LOW##################

LonIdx <- 451 # Low -157.5
LatIdx <- 1764 # Low 1.85

dhwlist <- list()
for (i in files) {
  # Open the netcdf file
  nc <- nc_open(i)
  dhwlist[[i]] <- ncvar_get( nc, "CRW_DHW")[LonIdx, LatIdx]
  # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
  nc_close(nc)
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dhw <- do.call(rbind, dhwlist)
dhw_low <- data.frame(dhw)
colnames(dhw_low) <- c("id","dhw")

dhw_low$date <- rownames(dhw_low)
dhw_low$date <- gsub(x = dhw_low$date,pattern="data/temperature/NOAA_5km/b5km_dhw_",replacement = "")
dhw_low$date <- gsub(x = dhw_low$date,pattern=".nc",replacement = "")
dhw_low$date <- as.POSIXct(dhw_low$date,format="%Y%m%d")

dhw_low_max <- max(dhw_low$dhw)
dhw_low[which(dhw_low$dhw==max(dhw_low$dhw)),]

###############################3

LonIdx <- 449 # Medium -157.55
LatIdx <- 1762 # Medium 1.9

dhwlist <- list()
for (i in files) {
  # Open the netcdf file
  nc <- nc_open(i)
  dhwlist[[i]] <- ncvar_get( nc, "CRW_DHW")[LonIdx, LatIdx]
  # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
  nc_close(nc)
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dhw <- do.call(rbind, dhwlist)
dhw_medium <- data.frame(dhw)
colnames(dhw_medium) <- c("id","dhw")

dhw_medium$date <- rownames(dhw_medium)
dhw_medium$date <- gsub(x = dhw_medium$date,pattern="data/temperature/NOAA_5km/b5km_dhw_",replacement = "")
dhw_medium$date <- gsub(x = dhw_medium$date,pattern=".nc",replacement = "")
dhw_medium$date <- as.POSIXct(dhw_medium$date,format="%Y%m%d")

dhw_medium_max <- max(dhw_medium$dhw)
dhw_medium[which(dhw_medium$dhw==max(dhw_medium$dhw)),]

######################################

LonIdx <- 454 # Very Low -157.3
LatIdx <- 1764 # Very Low 1.8

dhwlist <- list()
for (i in files) {
  # Open the netcdf file
  nc <- nc_open(i)
  dhwlist[[i]] <- ncvar_get( nc, "CRW_DHW")[LonIdx, LatIdx]
  # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
  nc_close(nc)
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dhw <- do.call(rbind, dhwlist)
dhw_very_low <- data.frame(dhw)
colnames(dhw_very_low) <- c("id","dhw")

dhw_very_low$date <- rownames(dhw_very_low)
dhw_very_low$date <- gsub(x = dhw_very_low$date,pattern="data/temperature/NOAA_5km/b5km_dhw_",replacement = "")
dhw_very_low$date <- gsub(x = dhw_very_low$date,pattern=".nc",replacement = "")
dhw_very_low$date <- as.POSIXct(dhw_very_low$date,format="%Y%m%d")

dhw_very_low_max <- max(dhw_very_low$dhw)
dhw_very_low[which(dhw_very_low$dhw==max(dhw_very_low$dhw)),]

######################################

LonIdx <- 451 # Very High -157.5
LatIdx <- 1760 # Very High 2.0

dhwlist <- list()
for (i in files) {
  # Open the netcdf file
  nc <- nc_open(i)
  dhwlist[[i]] <- ncvar_get( nc, "CRW_DHW")[LonIdx, LatIdx]
  # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
  nc_close(nc)
  print(i)
  Sys.sleep(0.01)
  flush.console()
}

dhw <- do.call(rbind, dhwlist)
dhw_very_high <- data.frame(dhw)
colnames(dhw_very_high) <- c("id","dhw")

dhw_very_high$date <- rownames(dhw_very_high)
dhw_very_high$date <- gsub(x = dhw_very_high$date,pattern="data/temperature/NOAA_5km/b5km_dhw_",replacement = "")
dhw_very_high$date <- gsub(x = dhw_very_high$date,pattern=".nc",replacement = "")
dhw_very_high$date <- as.POSIXct(dhw_very_high$date,format="%Y%m%d")

dhw_very_high_max <- max(dhw_very_high$dhw)
dhw_very_high[which(dhw_very_high$dhw==max(dhw_very_high$dhw)),]

dhw_dist <- merge(dhw_high,dhw_low, by="date",suffixes = c("_high","_low"))
dhw_dist <- merge(dhw_dist, dhw_medium, by="date")
colnames(dhw_dist)[4] <- "dhw_medium"
dhw_dist <- merge(dhw_dist, dhw_very_high, by="date")
colnames(dhw_dist)[5] <- "dhw_very_high"
dhw_dist <- merge(dhw_dist, dhw_very_low, by="date")
colnames(dhw_dist)[6] <- "dhw_very_low"


save.image(file="data/temperature/NOAA_5km.RData")

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Look at the netcdf file metadata
# print(nc)
# # Choose a lat/lon pair to extract data from
# #LonIdx <- which( nc$dim$lon$vals < -157.42) # Data is weird in this netcdf and can't subset this way. Doing it manually for the moment...
# #nc$dim$lon$vals[452]
# LonIdx <- 452
# #LatIdx <- which( nc$dim$lat$vals == 2.025)
# LatIdx <- 1841
# TimeIdx <- nc$dim$time$vals
# TimeIdx <- 1
# # DayIdx <- which( nc$dim$day$vals > 10 & nc$dim$day$vals < 20)
# dhw <- ncvar_get( nc, "CRW_DHW")[LonIdx, LatIdx]
# 
# # Close the netcdf file --!!IMPORTANT!! otherwise you might corrupt your netcfd file
# nc_close(nc)
# 
# 
# 
# 
# dhw_full <- ncvar_get( nc, "CRW_DHW")
# # get longitude and latitude
# lon <- ncvar_get(nc,"lon")
# nlon <- dim(lon)
# head(lon)
# lat <- ncvar_get(nc,"lat")
# nlat <- dim(lat)
# head(lat)
# nc_close(nc)
# 
# dhw_slice <- dhw_full[452,1841]
# 
