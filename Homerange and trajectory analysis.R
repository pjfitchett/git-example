# Home Range and Trajectory Analysis 

# February 2021

# Setting up libraries
wants <-c("adehabitatHR", "adehabitatHS", "lubridate", "here", "rgeos","dplyr", "ggplot2", "ggmap", "sf", "scales", "rgdal", "tmaptools")
has   <- wants%in% rownames(installed.packages())
if(any(!has))install.packages(wants[!has])

library(adehabitatHR)
library(adehabitatHS)
library(lubridate)
library(here)
library(ggplot2)
library(dplyr)
library(ggmap)
library(sf)
library(scales)
library(rgdal)
library(tmaptools)

# Read in jackal data
jackal<-read.csv(here::here("Data", "black-backed-jackal-Namibia-10.csv"))

# Check the format of the data
head(jackal)
# Check the structure of the data 
str(jackal) # timestamp has been read in as character data

# Use the lubridata function to convert from a factor to a time variable
jackal <- jackal%>%
  transform(timestamp =dmy_hm(timestamp)) 
# dmy_hm because this is the form of the data, could also be ymd_hm etc 

# Check how many fixes are rerecorded for each individual
fixes <- table(jackal$local_identifier)
fixes

# See this data on a map ####
# Preparing the map for output later by set the boundary
jackals_bbox <-make_bbox(lon = location_long, lat = location_lat, data = jackal, f = 0.1)

# Collect the map background (from open street map)
jackals_map <-ggmap(get_stamenmap(jackals_bbox), zoom = 11)

# Plot the map 
plot(jackals_map) +
  geom_point(data=jackal,aes(x=location_long, y=location_lat)) +
  ggtitle("Ten Jackals") # Difficult to see

# Colour code the variables by the local_identifier variable
plot(jackals_map)+ 
  geom_point(data=jackal,aes(x=location_long, y=location_lat,
                             color=local_identifier), alpha = 0.2)
# Separates the data points by animal

# Change a dataframe into a SpatialPoints Dataframe (spdf) by assigning the spatial coordinates
jackal.sp<- jackal %>%
  dplyr::select(location_lat, location_long,local_identifier) %>%
  as.data.frame()

coordinates(jackal.sp)<-~location_long+location_lat

# jackal.sp is a spatial object and should be handled appropriately
str(jackal.sp)

# Assign the correct Coordinate Reference System (CRS) to allow them to be correctly interpreted
# Lat long was used
crs.ll <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# the proj4string function assigns this to the spatial points data frame
proj4string(jackal.sp) <- crs.ll



# Convex polygon method ####

# Calculate the Minimum Convex Polygon (MCP) for the animals
# This is a measure of the smallest area in which the animals were recorded
# Gives a measure of the home range

# First project the data so distance can be measured in metres - the area is calculated in hectares

# spTransform converts between the lat long data and the projected data
jackal.sp_m<-spTransform(jackal.sp,CRS("+init=epsg:29375"))

# The mcp function allows you to specify units in and out 

# Enter the data in metres and have the output in hectares
jackal_mcp <-mcp(jackal.sp_m, percent=100, unin="m", unout = "h")

jackal_mcp.data<-as.data.frame(jackal_mcp)
jackal_mcp.data
# CM33 has the smallest home range, CM70 has the largest

# Add on the number of fixes to the data
jackal_mcp.data$relocs<- fixes

# Calculate the mean mcp area of all jackals
mean(jackal_mcp.data$area) # 33798.91

# Standard deviation
sd(jackal_mcp.data$area) # 42755.49

# Minimum value
min(jackal_mcp.data$area) # 2980.862

# Maximum value
max(jackal_mcp.data$area) # 135264.5

# Display the data as a boxplot
boxplot(jackal_mcp.data$area) # Shows two outliers (high values)

# Does the size of the home range vary with the number of fixes?
ggplot(jackal_mcp.data,aes(area, relocs))+geom_point()

# Plot the home ranges
plot(jackal_mcp)

plot(jackal.sp_m, col =as.numeric(jackal.sp_m$local_identifier), pch = 16)
plot(jackal_mcp, col =alpha(1:22, 0.3), add = TRUE)

jackal_mcp.sf<-st_as_sf(jackal_mcp)
jackal_mcpgeo <-spTransform(jackal_mcp,CRS("+proj=longlat"))

plot(jackals_map)+
  geom_polygon(data =fortify(jackal_mcpgeo,region = "id"),
               aes(long, lat, colour = id, fill = id, group=group),alpha = 0.3)+
  geom_point(data=jackal,aes(x=location_long, y=location_lat,
                             color=local_identifier), alpha = 0.2)

# Kernel Estimation - Utilisation distribution (UD) ####

# This is an estimation of how animals use space
# Takes account of the distribution of relocation points in space
# Measures how much time an animal spends, and applies weighting 
# Reduces the overestimation of space use 

# Estimation of UD for the 10 jackals
ud <-kernelUD(jackal.sp, grid = 100)

ver95 <-getverticeshr(ud, 95)

# Plot the data
plot(ver95, col=rainbow(10))

# Change the proportion of area represented
ver50 <-getverticeshr(ud, 50)
ver75 <-getverticeshr(ud, 75)

# Plot
plot(ver95, col=rainbow(10))
plot(ver75, add =TRUE, col=rainbow(10))
plot(ver50, add = TRUE, col=rainbow(10))

# Extract the area estimates for different %UD
hruds<-kernel.area(ud, percent=seq(50, 95, by=5))
hruds

# Trajectory analysis ####

# Look at the sequence of spatial points through time to give an idea of movement patterns and behaviour

# Remove any duplicate timestamps
jackal.spdf<- jackal %>%
  dplyr::distinct(local_identifier, timestamp, .keep_all = TRUE) %>%
  as.data.frame()
coordinates(jackal.spdf)<-~location_long+location_lat
proj4string(jackal.spdf) <- crs.ll
jackal.sp_m<-spTransform(jackal.spdf,CRS("+init=epsg:29375"))

jackal.ltraj <-as.ltraj(coordinates(jackal.sp_m),
                        date = jackal.sp_m$timestamp, id =jackal.sp_m$local_identifier)

# The ltraj object shows the summary
jackal.ltraj

# Plot all 10 trajectories
plot(jackal.ltraj)

# Or plot a single trajectory - change the number
plot(jackal.ltraj[8])

# Create a dataframe to hold all the contents of paths with a column for id
total.path.df <-data.frame(jackal.ltraj[[1]], id =attr(jackal.ltraj[[1]], "id"))

# Use a 'for' loop to fill the larger dataframe with the rest of the trajectories
for(i in 2: length(jackal.ltraj)) {
  total.path.df <- rbind(total.path.df, 
                         data.frame(jackal.ltraj[[i]], id = attr(jackal.ltraj[[i]], "id")))
}

# Calculate the mean distance travelled per day and the total distance by each animal
jackal.path<-total.path.df%>%
  group_by(id)%>%
  summarise(path.total=sum(dist, na.rm=TRUE)/1000,
            time.total=sum(dt,na.rm=TRUE))%>%
  mutate(dailydist= path.total/(time.total/60/60/24))%>%
  data.frame
# Converts time in seconds to days and divides the total path by number of days

# Summary dataframe
jackal.path

# Merge this dataframe with the data on the MCP and number of fixes/relocations
jackal.data<-left_join(jackal_mcp.data, jackal.path, by="id")

# Visualise the data to see if there is a relationship between a total home range area and distance travelled
dist.area.plot <-ggplot(data =  jackal.data,aes(x = area/1000 , y = path.total,
                                                colour = id)) +
  geom_point(size = 3) +
  labs(x = "MCP area (km2)",
       y = "Total distance travelled (km)" ) +
  theme_classic()
dist.area.plot

