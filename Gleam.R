#install.packages("input the library you need")
library(tidyverse)
# library(tidyr)
# library(dplyr)
library(ncdf4)
library(terra)
library(sp)
library(raster)
library(ggplot2)
library(fields)
library(maps)
library(abind)
library(sf)
library(stars)

Mywd <- readline(prompt = "Enter the route you put the result folder:") #D:/Master3_TU/TA_karst
setwd(Mywd)

##################### Download the GLEAM data (fail)----
# library(httr)
# url <- "sftp://hydras.ugent.be:2225/data/README_GLEAM_v3.6.pdf"
# output_file <- "D:/Master3_TU/TA_karst/GLEAM/README_GLEAM_v3.6.csv"
# GET(url, authenticate('gleamuser', 'v36_GLEAM2022#aw'), 
#     write_disk(output_file))
# # Create a matrix with 5 rows and 3 columns 
# mat = matrix(data = sample.int(100, 30), nrow = 5, ncol = 3) 


##################### Extract GLEAM data----
# whichvar <- readline(prompt = "Enter the variable: ") #e.g. E, Ep...
# data = paste0("D:/Master3_TU/TA_karst/GLEAM/v3.6a/daily/2019/",whichvar,"_2019_GLEAM_v3.6a.nc")
#e.g. filename_GLEAM_E = "D:/Master3_TU/TA_karst/GLEAM/v3.6a/daily/2019/E_2019_GLEAM_v3.6a.nc"
#for yealy data
data <- "./GLEAM/v3.6a/yearly/Ep_1980-2021_GLEAM_v3.6a_YR.nc"
ncin <- nc_open(data)
# print(ncin)
# attributes(ncin$var) #show: $names
dat <- ncvar_get(ncin, attributes(ncin$var)$names[1])
# dim(dat)
# dat[1440,720,365]
lat = ncvar_get(ncin, "lat")
lat_reverse <- sort(lat)
lon = ncvar_get(ncin, "lon")
tm =  ncvar_get(ncin, "time")
tm_date = as.Date(tm, origin = "1980-01-01", tz = "UTC") #the date has to change according to the file
# all entries in that grid(1,1)
# AET_in1and1 = ncvar_get(ncin, "E", start = c(1,1,1), count = c(1,1,365))
# AET here is obtained by the time (so we have to change the time)
AET_day1 = ncvar_get(ncin, "E", start = c(1,1,1), count = c(-1,-1,1))  #-1(from first to the last) now only the first layer(day)(the 3rd value can be changed 1-365)
# dim(AET_day1) #1440 720
# image.plot(lon,lat_reverse,AET_day1) 
# AET_day1_REV <- AET_day1[, rev(seq_len(ncol(AET_day1)))] #cause in the original file, the latitude is from big to samll, the map is upside down
image.plot(lon,lat_reverse,AET_day1_REV) 
# Add countries boundaries 
map(add=T)   


##################### Shapefile----
data_shp <- "./Milandre_Catchment_ShapeFiles/MILANDRE_crs.shp" #shp has to do find crs on GIS first
shp_forCropping <- st_read("./Milandre_Catchment_ShapeFiles/MILANDRE_crs.shp") #the shapefile to a data frame
shp <- shp_forCropping$geometry[[1]][[1]] 
class(shp)
shp_df <- as.data.frame(shp)
print(shp_df)
colnames(shp_df) <- c("lon","lat")
## Write the data frame to a CSV file
# write.csv(shp_df, file = "./Milandre_Catchment_ShapeFiles/MILANDRE_shp.csv", row.names = FALSE)

##################### Crop GLEAM data by shapefile (so far can just show the grids include the shp.)----
##### 1. convert GLEAM NetCDF to raster(brick)  
#better for plotting
# reads the netcdf file without specifying variable
brick_data <- brick(data,  varname="E")
## 1 means the first layer (first day in this year, range:1-366)
plot(brick_data[[1]]) 
## plots a range of variable layers
# plot(b[[13:24]])
###### 2. Crop the raster brick object to the extent of the shapefile
brick_crop <- crop(brick_data, extent(shp_forCropping),  snap = "out")
# Extract the values from the raster layer
brick_crop_value <- values(brick_crop)
# Convert the values to a numeric vector (only the first day here)
brick_crop_value_num <- as.numeric(brick_crop_value[,1]) 
plot(brick_crop[[1]], main = paste("Value:", as.character(round(brick_crop_value_num, 4))))
plot(shp_forCropping, add = TRUE)

## OR Plot the raster using ggplot
# Convert the raster to a data frame
# df_cropped <- as.data.frame(brick_crop[[1]], xy = TRUE)
# ggplot(df_cropped, aes(x, y)) +
#   geom_raster() +
#   geom_polygon(data = shp_df, aes(x = lon, y = lat), color = "red", size = 1)  

### Another way to crop
# # Read the netCDF file
# nc_data <- raster(data) 
# # Read the polygon shapefile
# polygon <- readOGR(data_shp)
# # Crop the netCDF data using the polygon
# nc_data_cropped <- crop(nc_data, extent(polygon), snap = "out")
# # Mask the netCDF data using the polygon (fail)
# nc_data_masked <- mask(nc_data_cropped, polygon) 
# # Write the cropped and masked netCDF data to a new file  (fail)
# writeRaster(nc_data_masked, "./Milandre_Catchment_ShapeFiles/E_Milandre.nc")

     
##################### Find the grid order of the caves by their coordinate (LOOP)----
cave_location <- read.csv("./caves5formap.csv")  
cave_number = length(cave_location$cave_site_name)
 
result_bind <- data.frame()
for (cave_no in 1:cave_number){ 
  print(c("NO.",cave_no))
  cave_name = cave_location$cave_site_name[cave_no]
  print(c("Now we are searching for data in", cave_name))
  name_col <- rep(cave_name, length(tm_date))
  need_lat = cave_location$latitude[cave_no]
  lat_range = c(need_lat-0.125,need_lat+0.12499) #if the point is on the boarder of the grid, we can only choose one grid
  lat_index = which(lat >=lat_range[1] & lat <= lat_range[2]) 
  print(lat_index)
  need_lon = cave_location$longitude[cave_no]
  lon_range = c(need_lon-0.125,need_lon+0.12499)
  lon_index = which(lon >=lon_range[1] & lon <= lon_range[2]) 
  print(lon_index)
  
  #find the AET in that grid
  E_ts = ncvar_get(ncin, "E", start = c(lon_index[1],lat_index[1],1), count = c(length(lon_index),length(lat_index),-1)) #-1 means ALL
  print("ok")
  result <- data.frame(tm_date, E_ts, name_col)
  print("ok")
  result_bind <- rbind(result,result_bind)
  }
write.csv(result_bind,file='D:/Master3_TU/TA_karst/GLEAM/Ep_allcave_year.csv', row.names=FALSE)
 
##################### Plot Time series ----
p <- ggplot(result_bind, aes(x=tm_date, y=E_ts, color=name_col)) +
  geom_line() + 
  xlab("") +
  labs(x ="Time", y = "AET (mm/day)")+
  theme_classic()+ 
  theme(
  axis.title.x = element_text(size=12, face="bold"),
  axis.title.y = element_text(size=12, face="bold"),
  panel.border=element_rect(linetype=1,fill=NA)
  )
p

##################### PLOT the grid for the cave ----
# seems to be unable output only one grid, so +1 on the length
E_ts_4grid = ncvar_get(ncin, "E", start = c(lon_index[1],lat_index[1],1), count = c(length(lon_index)+1,length(lat_index)+1,-1)) #-1 means ALL
dim(E_ts_4grid)
image.plot(E_ts[,,1]) 
?image.plot

#AET_area_df <- as.data.frame(E_ts[[1]],xy = T)
AET_time_df <- as.data.frame(E_ts[[1]],xy = T)

flatten_array(E_ts , margin = 2, varsAsFactors = FALSE)
#average the 4 boxes and make average
# 3: the 3rd dim is time. We have to keep the time and make average spatially
E_ts_avg = apply(E_ts, 3, mean, na.rm = T)
?apply
dim(E_ts_avg)
length(E_ts_avg)
plot(E_ts_avg, type ='l')

library(terra)
r <-rast(filename_GLEAM_E)
r.df<-terra::as.data.frame(r, xy=T)

write.csv(unique_site,file='D:/Master3_TU/TA_karst/IAEA/site_India_Ban.csv', row.names=FALSE)


##################### Close the netCDF file ----
nc_close(nc)
