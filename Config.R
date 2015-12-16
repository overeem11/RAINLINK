## R script 'config.R'.
## Contains the parameter values, coordinate systems, visualisation options, and folder names for rainfall
## estimation and mapping using microwave links. This R script needs to be loaded when the functions from
## the rainfall retrieval and visualisation algorithm are employed (R package "RAINLINK").
## Described in paper:
## Aart Overeem, Hidde Leijnse, Remko Uijlenhoet, 2015. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network. Atmos. Meas. Tech. Discuss., revised version.  

## Copyright (C) 2015 Aart Overeem
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program. If not, see <http://www.gnu.org/licenses/>.

####################
# Load R libraries.#
####################

# Give path of R libraries ("yes") instead of using default path location ("no"):
GivePathLib <- "yes"
# Location of R libraries (needs to be specified in case GivePathLib is "yes")
pathlib <- "/usr/people/overeem/Rlibraries"
# .libPaths() can be used to search for location of R libraries.

if (GivePathLib=="yes")
{
	# Use library(PACKAGENAME,lib.loc=pathlib) if R libraries cannot be found and are installed in a 
	# specific path.
	library(sp,lib.loc=pathlib)		
	library(gstat,lib.loc=pathlib)		
	library(ggplot2,lib.loc=pathlib)	
	library(ggmap,lib.loc=pathlib)		
	library(maps,lib.loc=pathlib)		
	library(mapproj,lib.loc=pathlib)	
	library(labeling,lib.loc=pathlib)
	library(rgdal,lib.loc=pathlib)
	#  May require installation of nc-config outside R	
	library(ncdf4,lib.loc=pathlib)
}
if (GivePathLib=="no")
{
	library(sp)		
	library(gstat)		
	library(ggplot2)	
	library(ggmap)	
	library(maps)		
	library(mapproj)		
	library(labeling)
	library(rgdal)
	library(ncdf4)	
}
if (GivePathLib!="no"&GivePathLib!="yes")
{
	print("Please specify whether path of R libraries is given by you!")
	stop()
}



#############################################################
# 1. Time interval of observation; end time of daily period.#
#############################################################

# 1.1 Time interval:
TIMESTEP <- 15		# minutes


# 1.2 Select daily time interval, i.e., "0800" implies 0800 UTC previous day - 
# 0800 UTC present day. 
# Do not use "0000" for 0000 - 0000 UTC, but "2400"!
PERIOD <- "0800"	# time in UTC (hours and minutes)


# 1.3 Time zone
TimeZone <- "UTC"



#############################################
# 2. Folder names and files to be processed.#
#############################################

# 2.1 Folder names:
# Location of original link data:
FolderStart = "LinkDataOriginal"	

# Location of output data from function PreprocessedMinMax:
FolderPreprocessed = "LinkDataPreprocessed"	

# Location of output data from function WetDryNearbyLinkApMinMax or NoWetDryMinMax:
FolderCorrected = "LinkDataCorrected"

# Location of output data from function RainRetrievalMinMax.R:
FolderRainEstimates = paste("LinkPathRainDepths",TIMESTEP,"min",sep="")

# Location of output data from function Interpolation.R:
FolderRainMaps = paste("RainMapsLinks",TIMESTEP,"min",sep="")

# Location of produced figures:
FolderFigures = "Figures"


# 2.2 Which files are to be processed? Give their numbers:
# Function PreprocessingMinMax:
FileNrStartPreproc <- 2		# Folder FolderStart
FileNrEndPreproc <- 2		# Folder FolderStart
# Function WetDryNearbyLinkApMinMax / NoWetDryMinMax:
FileNrStartWetDry <- 1		# Folder FolderPreprocessed
FileNrEndWetDry <- 1		# Folder FolderPreprocessed
# Function RainRetrievalMinMax:
FileNrStartRainRetr <- 1	# Folder FolderCorrected
FileNrEndRainRetr <- 1  	# Folder FolderCorrected
# Function Interpolation:
FileNrStartInterp <- 1		# Folder FolderEstimates
FileNrEndInterp <- 96		# Folder FolderEstimates
# Function RainMapsLinksTimeStep:
FileNrStartRainMaps <- 51	# Folder FolderRainMaps
FileNrEndRainMaps <- 51	# Folder FolderRainMaps



############################
# 3. Microwave frequencies.#
############################

# 3.1 Minimum and maximum allowed microwave frequency:
MinFrequency <- 12.5	# GHz
MaxFrequency <- 40.5	# GHz



#########################
# 4. Coordinate systems.#
#########################

# 4.1 Define coordinate system of input data (e.g. WGS84): 
CoorSystemInputData <- "+init=epsg:4326"	# WGS84


# 4.2 Define center of Azimuthal Equidistant Cartesian coordinate system.
# XMiddle becomes center of Azimuthal Equidistant Cartesian coordinate system. 
# YMiddle becomes center of Azimuthal Equidistant Cartesian coordinate system. 
# We chose 52.155223°N 5.387242°E as the middle of the Netherlands ('The Tower 
# of Our Lady' is a church tower in Amersfoort and used to be the middle point 
# of the Dutch grid reference system).
XMiddle <- 5.387242	# longitude in degrees (WGS84)
YMiddle <- 52.155223	# latitude in degrees (WGS84)
# These coordinates should be provided in the coordinate system CoorSystemInputData.


# 4.3 Projection string to convert to Azimuthal Equidistant Cartesian coordinate system.
# Unit of Azimuthal Equidistant Cartesian coordinate system has to be kilometer, because climatological
# variogram of ordinary kriging algorithm uses kilometers. 
projstring <- paste("+proj=aeqd +a=6378.137 +b=6356.752 +R_A +lat_0=",YMiddle," +lon_0=",XMiddle,
" +x_0=0 +y_0=0",sep="")



############################################
# 5. WET-DRY CLASSIFICATION "LINK APPROACH"#
############################################

# 5.1 Select all links for which both end points are within 15 km from either end of the already selected link:
DistanceLimit <- 15	# km


# 5.2 Minimum number of hours in the previous 24 hours needed for computing max(P_min):
MinHoursPmin <- 6


# 5.3 Only use data if number of available links is at least larger than the threshold below for present and 
# previous day for those time steps that the selected link is available. Selected links is also counted.
ThresholdNumberLinks <- 3


# 5.4 Threshold values link approach:
ThresholdMedianL <- -0.7 	# dB km^-1
ThresholdMedian <- -1.4	# dB


# 5.5 If threshold is exceeded for a given time interval that is classified as wet, the previous two 
# time intervals and the next time interval are classified as wet for the selected link.
ThresholdWetDry <- 2	# dB


# 5.6 File with values of coefficients in R-k relationship (power-law between path-averaged rainfall intensity
# and path-averaged specific attenuation):
FileRainRetr <- "ab_values_vertical.txt"
# "ab_values_vertical.txt": Values of coefficients in k-R relationship from Leijnse, H., 2007: Hydrometeorological application of microwave links - Measurement of evaporation and
# precipitation. PhD thesis, Wageningen University, Wageningen. See page 65.
# Provided for frequencies from 1 - 100 GHz.



#################################
# 6. COMPUTATION REFERENCE LEVEL#
#################################

# 6.1 Minimum number of hours that should be dry in preceding 24 hours for computing reference level:
HoursRefLevel <- 2.5	# h



#####################
# 7. Outlier filter.#
#####################

# 7.1 Threshold outlier filter:
OUTLIERFILTER <- -32.5	# dB km^-1 h

OutlierFilter <- "yes" # Choose "yes" to apply an outlier filter, choose "no" to not apply an outlier filter.



#############################################################
# 8. Wet antenna attenuation correction & sampling strategy.#
#############################################################

# 8.1 Wet antenna attenuation correction:
Aa <- 2.3	# dB


# 8.2 Sampling strategy (conversion from minimum and maximum to mean path-averaged rainfall intensity):
alpha <- 0.33	



####################
# 9. Interpolation.#
####################

# 9.1 File with interpolation grid:
FileGrid <- "InterpolationGrid.dat"	# WGS84 (longitude, latitude (degrees))


# 9.2 Conversion factor from rainfall depth to intensity (mm/h):
MinutesHour <- 60
ConversionDepthToIntensity <- MinutesHour/TIMESTEP


# 9.3 Choose interpolation method and parameters:
# Use "OK" for ordinary kriging using a climatological variogram.
# Use "IDW" for inverse distance weighting on the real microwave link rainfall estimates.
IntpMethod <- "OK"
#IntpMethod <- "IDW"

# For "OK":
nmax <- 50	# for local kriging: the number of nearest observations that should be used for a 
		# kriging prediction or simulation, 
		# where nearest is defined in terms of the space of the spatial locations. By default, 
		# all observations are used


# Ordinary kriging: which variogram to use?
# Use "ClimvdBeek" for climatological spherical variogram model.
# Use "Manual" for spherical variogram model with nugget, sill, and range values manually specified below.
Variogram <- "ClimVar"
#Variogram <- "Manual"
#NUGGET <- 0.37	# mm^2 h^-2
#SILL <- 3.7	# mm^2 h^-2
#RANGE <- 18.7	# km


# Inverse Distance Weighting parameter:
idp <- 2.0	# idp: numeric; specify the inverse distance weighting power



########################
# 10. Rainfall mapping.#
########################

# 10.1 Conversion from rainfall intensity (mm/h) to depth (mm):
ConversionIntensityToDepth <- TIMESTEP/MinutesHour


# 10.2 Name of file with polygons of radar and interpolation grid:
FilePolygonsGrid <- "PolygonsGrid.dat"


# 10.3 Google Maps or OpenStreetMap as background?
MapBackground <- "Google"
# Use "Google" for Google Maps and "OSM" for OpenStreetMap.
# Google Maps will only plot on a square figure.


# 10.4 Color or black-and-white background map?
colortype <- "bw"
# Use "color" for colour and "bw" for black-and-white background map.


# 10.5 Figure width and height:
FigWidth <- 2200	# Pixels
FigHeight <- 2000	# Pixels
# 1280 x 1280 pixels seems maximum graphical resolution for downloaded Google Maps. 
# Because also axes and legend are plotted, it is advised to use e.g. 1450 x 1450 pixels. Then the Google Map
# will remain approximately 1280 x 1280 pixels. Using higher values is not a problem (e.g. 2000). 
# In this way it is tried to get the highest possible resolution.
# For OpenStreetMap the maps may reach resolutions of 1500 - 2000 pixels. Hence, using FigWidth and FigHeight
# of 2000 pixels or higher is advised.
# The OpenStreetMap itself is stored in file "ggmapTemp.png". From this file the resolution of the background
# map can be obtained. This can be useful for determining an appropriate FigWidth and FigHeight above.



# GOOGLE MAPS: SPECIFIC SETTINGS
##################################################
# 10.6 Use specified location as middle of map? 
# And provide location name or longitude and latitude (degrees).
# If LocNameSpecified <- "yes" then the specified location name is used.
# If LocDegSpecified <- "yes" then the specified location in degrees is used.
# Function will stop if both are specified "yes".
# If both are not "yes": Bounding box is determined from interpolation grid. 
LocNameSpecified <- "yes"
LocName <- "De Eemhof"
LocDegSpecified <- "no"
LocLon <- 5.1096649
LocLat <- 52.0901422
#LocName <- "Rotterdam"


# 10.7 In case of Google Maps: which map type should be used?
MapType <- "roadmap"
# Available map types: "terrain", "terrain-background", "satellite", "roadmap", and "hybrid" (google maps).


# 10.8 In case of Google Maps: which zoom level to use?
zoomlevel <- 8
# 14 nice zoomlevel for satellite map Amsterdam.
# 13 nice zoom level for satellite map Rotterdam
# 8 for whole Netherlands if "Eemhof" is center.
##################################################



# OPENSTREETMAP: SPECIFIC SETTINGS
##################################################
# 10.9 Area for which rainfall depths are to be plotted (for OpenStreetMap only):
# Amsterdam region (for OpenStreetMap only):
left <- 4.84		# Longitude in degrees (WGS84)
bottom <- 52.336	# Latitude in degrees (WGS84)
right <- 4.95		# Longitude in degrees (WGS84)
top <- 52.404		# Latitude in degrees (WGS84)

# Rotterdam region (for OpenStreetMap only):
#left <- 4.41		# Longitude in degrees (WGS84)
#bottom <- 51.9		# Latitude in degrees (WGS84)
#right <- 4.52		# Longitude in degrees (WGS84)
#top <- 51.97		# Latitude in degrees (WGS84)

# Utrecht region (for OpenStreetMap only):
#left <- 5.0900		# Longitude in degrees (WGS84)
#bottom <- 52.0700	# Latitude in degrees (WGS84)
#right <- 5.1350		# Longitude in degrees (WGS84)
#top <- 52.0950		# Latitude in degrees (WGS84)

#http://www.openstreetmap.org/export can be used to select area and find minimum scale in order to obtain maximum graphical resolution.


# 10.10 In case of OpenStreetMap: compute bounding box from input data or used
# bounding box defined above?
# Use "yes" if bounding box is to be computed from interpolation grid.
BBoxOSMauto <- "no"


# 10.11 In case of OpenStreetMap: Give value of scale.
# A proper choice of the scale parameter in get_openstreetmap is difficult. 
# It cannot be computed automatically. Hence, a scale parameter value should
# be provided below. The scale parameter should be as small as possible to get the highest 
# graphical resolution. However, a too low value may result in a map not being downloaded. 
# Hence, the user should manually supply get_openstreetmap with a scale. 
# The file "ggmapTemp.png" is written to disk when a OpenStreetMap is loaded.
# The highest possible resolution for a square area is about 2000 x 2000 pixels.
# Scale <- 8600 Utrecht
Scale <- 24000  # Scale for Amsterdam.
#works good if BBoxOSMauto = "no" and with values 
# for left, right, top, bottom as defined above for Amsterdam.
# For Rotterdam (as defined above): Scale <- 22500
# It may require some repeating to find the appropriate value for scale.
##################################################



# LEGENDS AND POLYGONS
##################################################
# 10.12 Transparency of legend and polygons:
alpha_scale <- 0.2  	# alpha_scale = 0.5 does not give any transparency.
alpha_polygon <- 0.6
alpha_links_timestep <- 0.4	# The Netherlands + Amsterdam
#alpha_links_timestep <- 0.6	# Rotterdam
alpha_links_daily <- 1


# 10.13 Number of classes in legend:
colors_number <- 5


# 10.14 Title of legend:
LegendTitleLinksTimeStep <- paste(TIMESTEP,"-min\nrainfall\ndepth\n(mm)\n",sep="")
LegendTitleRadarsTimeStep <- paste(TIMESTEP,"-min\nrainfall\ndepth\n(mm)\n",sep="")
LegendTitleLinksDaily <- paste("Daily\nrainfall\ndepth\n(mm)\n",sep="")
LegendTitleRadarsDaily <- paste("Daily\nrainfall\ndepth\n(mm)\n",sep="")


# 10.15 Label name of horizontal axis:
LabelAxisLonOSM <- "Longitude (º)\n©OpenStreetMap contributors; openstreetmap.org"
LabelAxisLonGoogle <- "Longitude (º)\n©Google Maps"


# 10.16 Plot grid lines for polygons below threshold scale_bottom?
plot_below_scale_bottom <- "no"
# "yes" for plotting.


# 10.17 Colour of pixel borders:
# Use NA (without quotes) to not plot pixel borders.
# If the pixels are relatively small with respect to the plotted region, the graphical quality of the pixel borders deteriorates due to low number of pixels (low resolution).
PixelBorderCol <- NA
#"gray55"


# 10.18 Size of pixel borders:
SizePixelBorder <- 0.8


# 10.19 Lowest class starts at this threshold (minimum rainfall accumulation (mm) to be plotted):
scale_bottom_timestep <- 0.1	# mm. 
scale_bottom_daily <- 1	# mm.
# Using a value clearly above 0 mm can save a lot of computation time if the polygons 
# belonging to values below the threshold are not plotted.


# 10.20 Highest color class ends here (maximum rainfall accumulation (mm) to be plotted):
scale_top_timestep <- 7.6 	# mm
scale_top_daily <- 26		# mm
# Sometimes the legend is not correctly plotted. In that case try other values for scale_top and/or scale_bottom. 
# For instance, if the highest class (> x mm) is plotted below instead of above the other classes.
# Or the number of classes does not match the number of chosen classes.
# Another way to prevent this is to manually give the legend breaks:
ManualScale <- "no"	# If "no" manual scale below is not used, and interval breaks are determined automatically. 
# For not equal to "no", interval breaks are determined manually:
scale_low <- c(scale_bottom_daily,6,11,16,21)
scale_high <- c(6,11,16,21,26)
scale_top_daily <- max(scale_high)
# Please note that in case of x values in scale_low colors_number should also be x.


# 10.21 Let R automatically define highest value of legend (choose "yes"):
AutDefineLegendTop <- "no"
# In case of "yes" the highest class is not plotted anymore.
# Color of highest class:
ColourHighestClass <- "slateblue4"


# 10.22 Choose (RColorBrewer) palette:
palette <- "YlGnBu" 
#"Blues", "YlGnBu", "GnBu": sequential
#"Spectral", "RdYlBu", "BrBG": diverging
# Unfortunately, it seems not possible to select those colours from the full range of the palette. 
# For 6 or less classes we get a reasonable set of colours though.
##################################################



# TITLE OF PLOT AND OUTPUT FILE NAMES
##################################################
# 10.23 Part of title of plot: 
TitleLinks <- "Links, nearby link approach, outlier filter"
#TitleLinks <- "Links, no wet-dry classification, no outlier filter"
TitleRadars <- "Radars + Gauges"
#ExtraText <- "Amsterdam"		# For instance location of map (e.g. a city or country).
ExtraText <- "The Netherlands"


# 10.24 Part of figure name for links for chosen TIMESTEP:
FigFileLinksTimeStep <- paste("Links",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
FigFileRadarsTimeStep <- paste("Radars",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
FigFileLinksDaily <- paste("LinksDaily",gsub(" ","",ExtraText),sep="")
FigFileRadarsDaily <- paste("RadarsDaily",gsub(" ","",ExtraText),sep="")


# LINK LOCATIONS
# 10.25 Plot locations of links in plot?
PlotLocLinks <- "yes"
# If "yes" than locations of links are plotted in plot. Note that full-duplex links are plotted twice.


# 10.23 Color and size of plotted link paths:
ColorLinks <- "black" # "black" for country-wide maps and Amsterdam.
SizeLinks <- 3 # For Netherlands
#SizeLinks <- 7 # For Amsterdam 
#SizeLinks <- 9 # For Rotterdam
##################################################



# NAME OF RADAR INPUT FILES
##################################################
# NETCDF4 file for daily rainfall depths:
# Download from Climate4Impact (works at least for Linux):
# path0 = "http://opendap.knmi.nl/knmi/thredds/fileServer/radarprecipclim/RAD_NL25_RAC_MFBS_24H_NC/2011/09/RAD_NL25_RAC_MFBS_24H_201109110800.nc"
# download.file(path0,"RAD_NL25_RAC_MFBS_24H_201109110800.nc",method="wget",quiet=F,mode="wb",cacheOK=T)
FileNameRadarDaily <- "RAD_NL25_RAC_MFBS_24H_201109110800.nc"
# ASCII file for 15-min rainfall depths:
FileNameRadarTimeStep <- "Radar_201109102045_15min.dat"
##################################################







