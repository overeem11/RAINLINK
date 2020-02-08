## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.14
## Copyright (C) 2019 Aart Overeem
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

## R program 'Config.R'.
## Contains the parameter values, coordinate system, visualisation options, and folder names for rainfall
## estimation and mapping using microwave links. This R script can be loaded when the functions from
## the rainfall retrieval and visualisation algorithm are employed (R package "RAINLINK").
## source("Config.R")
## Run this script by pasting the line above (source(...)).


#######################
# 0. Load R libraries.#
#######################

# Give path of R libraries ("yes") instead of using default path location ("no"):
GivePathLib <- "no"
# Location of R libraries (needs to be specified in case GivePathLib is "yes")
pathlib <- "/usr/people/overeem/Rlibraries"
# .libPaths() can be used to search for location of R libraries.

if (GivePathLib=="yes")
{
	# Use library(PACKAGENAME,lib.loc=pathlib) if R libraries cannot be found and are installed in a 
	# specific path.

	# Load RAINLINK package:
	library(RAINLINK,lib.loc=pathlib)

	# Load other packages:
        library(backports,lib.loc=pathlib)
        library(vctrs,lib.loc=pathlib)	
	library(curl,lib.loc=pathlib)
	library(sp,lib.loc=pathlib)		
	library(gstat,lib.loc=pathlib)	
	library(crayon,lib.loc=pathlib)	
	library(withr,lib.loc=pathlib)	
	library(ggplot2,lib.loc=pathlib)	
	library(ggmap,lib.loc=pathlib)		
	library(maps,lib.loc=pathlib)		
	library(mapproj,lib.loc=pathlib)	
	library(labeling,lib.loc=pathlib)
	library(rgdal,lib.loc=pathlib)
	library(digest,lib.loc=pathlib)
	library(farver,lib.loc=pathlib)
	#  May require installation of nc-config outside R	
	library(ncdf4,lib.loc=pathlib)
}
if (GivePathLib=="no")
{
	# Load RAINLINK package:
	library(RAINLINK)

	# Load other packages:
        library(backports)
        library(vctrs)
	library(curl)	
	library(sp)		
	library(gstat)	
	library(crayon)	
	library(withr)	
	library(ggplot2)	
	library(ggmap)	
	library(maps)		
	library(mapproj)		
	library(labeling)
	library(rgdal)
	library(digest)
	library(farver)
	library(ncdf4)	
}
if (GivePathLib!="no"&GivePathLib!="yes")
{
	print("Please specify whether path of R libraries is given by you!")
	stop()
}


# Needed for WetDryNearbyLinkApMinMaxRSL, Interpolation and visualisation functions:
# Define center of Azimuthal Equidistant Cartesian coordinate system.
# XMiddle becomes center of Azimuthal Equidistant Cartesian coordinate system. 
# YMiddle becomes center of Azimuthal Equidistant Cartesian coordinate system. 
XMiddle <- 5.387242	# longitude in degrees (WGS84)
YMiddle <- 52.155223	# latitude in degrees (WGS84)
# These coordinates should be provided in the coordinate system CoorSystemInputData.
# We chose 52.155223°N 5.387242°E as the middle of the Netherlands ('The Tower 
# of Our Lady' is a church tower in Amersfoort and used to be the middle point 
# of the Dutch grid reference system).



############################
# 1. PreprocessingMinMaxRSL#
############################

# Maximum and minimum allowed microwave frequency:
MaxFrequency <- 40.5	# GHz 
MinFrequency <- 12.5	# GHz




############################################
# 2. WetDryNearbyLinkApMinMaxRSL (OPTIONAL)#
############################################


# Select all links for which both end points are within Radius km 
# from either end of the already selected link:
Radius <- 15	# km

# Minimum number of hours in the previous 24 hours needed for computing max(P_min):
MinHoursPmin <- 6

# Number of previous hours over which max(Pmin) is to be computed:
PeriodHoursPmin <- 24	# Also determines period over which cumulative difference F of outlier filter is computed.
# Note that if less than PeriodHoursPmin hours are available, the wet-dry classification is still performed
# if at least MinHoursPmin hours are available. E.g. at the start of the period with microwave link data, we need
# at least MinHoursPmin hours of data before the wet-dry classification will start.

# If threshold ThresholdWetDry is exceeded for a given time interval that is classified as wet, the previous two 
# time intervals and the next time interval are classified as wet for the selected link. This is only done
# if Step8 is "yes".
Step8 <- TRUE

# Threshold values:
ThresholdMedian <- -1.4	# dB
ThresholdMedianL <- -0.7 	# dB km^-1

# Only use data if number of available links is at least larger than this threshold for the time interval 
# under consideration. The selected link is also counted.
ThresholdNumberLinks <- 3

# If threshold is exceeded for a given time interval that is classified as wet, the previous two 
# time intervals and the next time interval are classified as wet for the selected link.
# This is only done when Step7 is "yes".
ThresholdWetDry <- 2	# dB




#######################
# 3. RefLevelMinMaxRSL#
#######################

# Minimum number of hours that should be dry in preceding PeriodHoursRefLevel hours for computing reference level:
HoursRefLevel <- 2.5	# h

# Period over which reference level is to be determined:
PeriodHoursRefLevel <- 24	# h




####################################
# 4. OutlierFilterMinMax (OPTIONAL)#
####################################

# Outlier filter threshold:
FilterThreshold <- -32.5	# dB km^-1 h



############################
# 6. RainRetrievalMinMaxRSL#
############################

# Wet antenna attenuation correction:
Aa <- 2.3	# dB

# Sampling strategy (conversion from minimum and maximum to mean path-averaged rainfall intensity):
alpha <- 0.33	

# Name of file with values of coefficients in R-k relationship (power-law between path-averaged rainfall intensity
# and path-averaged specific attenuation) for vertically polarized signals:
FileRainRetrVertical <- "ab_values_vertical.txt"
# "ab_values_vertical.txt": Values of coefficients in k-R relationship from Leijnse, H., 2007: Hydrometeorological 
# application of microwave links - Measurement of evaporation and precipitation. PhD thesis, Wageningen University, 
# Wageningen. See page 65. Provided for frequencies from 1 - 100 GHz.

# Name of file with values of coefficients in R-k relationship (power-law between path-averaged rainfall intensity
# and path-averaged specific attenuation) for horizontally polarized signals:
FileRainRetrHorizontal <- "ab_values_horizontal.txt"
# "ab_values_horizontal.txt": Leijnse, H., R. Uijlenhoet, and A. Berne, 2010: Errors and uncertainties in microwave 
# link rainfall estimation explored using drop size measurements and high-resolution radar data. J. Hydrometeorol., 
# 11 (6), 1330–1344, doi:https://doi.org/10.1175/2010JHM1243.1.



###################
# 7. Interpolation#
###################

# File with interpolation grid in same coordinate system as CoorSystemInputData:
FileGrid <- "InterpolationGrid.dat"	# WGS84 (longitude, latitude (degrees))

# For "IDW":
# Specify the inverse distance weighting power:
idp <- 2	

# Choose interpolation method and parameters:
# Use "OK" for ordinary kriging using a climatological variogram.
# Use "IDW" for inverse distance weighting on the real microwave link rainfall estimates.
IntpMethod <- "OK"

# For "OK":
nmax <- 50	# for local kriging: the number of nearest observations that should be used for a 
		# kriging prediction or simulation, 
		# where nearest is defined in terms of the space of the spatial locations. By default, 
		# all observations are used

# For "OK": which variogram to use?
# Use "ClimVar" for climatological spherical variogram model.
# Use "Manual" for spherical variogram model with nugget, sill, and range values manually specified below.
Variogram <- "ClimVar"
NUGGET <- 0.37	# mm^2 
SILL <- 3.7	# mm^2 
RANGE <- 18.7	# km



###################
# 8. Visualisation#
###################

###################
# GENERAL SETTINGS#
###################

# Define coordinate system of input data:
CoorSystemInputData <- "+init=epsg:4326"	# WGS84


# Select daily time interval, i.e., "0800" implies 0800 UTC previous day - 
# 0800 UTC present day. Do not use "0000" for 0000 - 0000 UTC, but "2400".
PERIOD <- "0800"	# hours and minutes


# Time zone:
TimeZone <- "UTC"
Sys.setenv <- (TZ="TimeZone")


# To reduce computational time, it is automatically determined which grid cells fall within the plotted region. 
# To also plot grid cell values which partly fall outside the plotted region, a positive number for ExtraDeg
# should be specified (degrees). This should typically be at least the size of one grid cell in degrees.
ExtraDeg <- 0.05  


# Name of file with polygons of interpolation grid in same coordinate system as CoorSystemInputData:
FilePolygonsGrid <- "PolygonsGrid.dat"	# WGS84 (longitude, latitude (degrees))


# Folder name of figures:
FolderFigures <- "Figures"


# Google Maps or OpenStreetMap as background?
MapBackground <- "Stamen"
# Use "Google" for Google Maps and "OSM" for OpenStreetMap and "Stamen" for Stamen Map (based on OpenStreetMap data).
# Google Maps will only plot on a square figure.


# Colour or black-and-white background map?
ColourType <- "bw"
# Use "color" for colour and "bw" for black-and-white background map.


# Figure width and height:
FigWidth <- 2200	# Pixels
FigHeight <- 2000	# Pixels
# 1280 times 1280 pixels seems maximum graphical resolution for downloaded Google Maps. 
# Because also axes and legend are plotted, it is advised to use e.g. 1450 times 1450 pixels. Then the Google Map
# will remain approximately 1280 x 1280 pixels. Using higher values is not a problem (e.g. 2000). 
# In this way it is tried to get the highest possible resolution.
# For OpenStreetMap the maps may reach resolutions of 1500 - 2000 pixels. Hence, using FigWidth and FigHeight
# of 2000 pixels or higher is advised.
# The OpenStreetMap itself is stored in file "ggmapTemp.png". From this file the resolution of the background
# map can be obtained. This can be useful for determining an appropriate FigWidth and FigHeight above.


# Specify font family of text in figures:
FontFamily <- "Times"
# To select the default font use "". Using "Times" may give warnings when executing the visualisation. 
# In that case the font is not installed on the computer. This can be solved by using the default font ("").


# Size of plot title:
SizePlotTitle <- 65


# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15		


# Location of interpolated link data:
FolderRainMaps <- paste("RainMapsLinks",TIMESTEP,"min",sep="")


# Location of link path data:
FolderRainEstimates <- paste("LinkPathRainDepths",TIMESTEP,"min",sep="")


# Conversion factor from rainfall depth (mm) to intensity (mm/h):
MinutesHour <- 60
ConversionDepthToIntensity <- MinutesHour/TIMESTEP


# File with interpolation grid in same coordinate system as CoorSystemInputData:
FileGrid <- "InterpolationGrid.dat"	# WGS84 (longitude, latitude (degrees))



#################################
# GOOGLE MAPS: SPECIFIC SETTINGS#
#################################
# Use specified location as centre of map? 
# And provide location name or longitude and latitude (degrees).
# If GoogleLocNameSpecified <- "yes" then the specified location name is used.
# If GoogleLocDegSpecified <- "yes" then the specified location in degrees is used.
# Function will stop if both are specified "yes".
# If both are not "yes": Bounding box is determined from interpolation grid, i.e. FilePolygonsGrid. 
GoogleLocNameSpecified <- "yes"
GoogleLocName <- "De Eemhof, Zeewolde"
GoogleLocDegSpecified <- "no"
GoogleLocLon <- 5.1096649
GoogleLocLat <- 52.0901422
#GoogleLocName <- "Amsterdam"

# In case of Google Maps: which map type should be used?
GoogleMapType <- "terrain"
# Available map types: "terrain", "satellite", "roadmap", and "hybrid" (google maps).


# Which zoom level to use for the Google Maps?
GoogleZoomlevel <- 8
# 14 nice zoom level for satellite map Amsterdam.
# 13 nice zoom level for satellite map Rotterdam
# 8 nice zoom level for whole Netherlands if "Eemhof" is center.
#################################



##################################################
# OPENSTREETMAP and Stamen Map: SPECIFIC SETTINGS#
##################################################
# Area for which rainfall depths are to be plotted (for OpenStreetMap and Stamen Map only):
# Amsterdam region (for OpenStreetMap and Stamen Map only):
OSMLeft <- 4.84			# Longitude in degrees (WGS84) for left side of the area for which rainfall depths are to be plotted (for OpenStreetMap only). 
OSMBottom <- 52.336		# Latitude in degrees (WGS84) for bottom side of the area for which rainfall depths are to be plotted (for OpenStreetMap only).
OSMRight <- 4.95		# Longitude in degrees (WGS84) for right side of the area for which rainfall depths are to be plotted (for OpenStreetMap only). 
OSMTop <- 52.404		# Latitude in degrees (WGS84) for top side of the area for which rainfall depths are to be plotted (for OpenStreetMap only).

# Rotterdam region (for OpenStreetMap and Stamen Map only):
#left <- 4.41		# Longitude in degrees (WGS84)
#bottom <- 51.9		# Latitude in degrees (WGS84)
#right <- 4.52		# Longitude in degrees (WGS84)
#top <- 51.97		# Latitude in degrees (WGS84)

# Utrecht region (for OpenStreetMap and Stamen Map only):
#left <- 5.0900		# Longitude in degrees (WGS84)
#bottom <- 52.0700	# Latitude in degrees (WGS84)
#right <- 5.1350		# Longitude in degrees (WGS84)
#top <- 52.0950		# Latitude in degrees (WGS84)

#http://www.openstreetmap.org/export can be used to select area and find minimum scale in order to obtain maximum graphical resolution.


# Compute bounding box from input data or use bounding box defined above? (for OpenStreetMap and Stamen Map only):
# Use "yes" if bounding box is to be computed from interpolation grid.
BBoxOSMauto <- "yes"


# Give value of scale (for OpenStreetMap only, so not for Stamen Map):
# A proper choice of the scale parameter in get_openstreetmap is difficult. 
# It cannot be computed automatically. Hence, a scale parameter value should
# be provided below. The scale parameter should be as small as possible to get the highest 
# graphical resolution. However, a too low value may result in a map not being downloaded. 
# Hence, the user should manually supply get_openstreetmap with a scale. 
# It may require some iterations to find the appropriate value for scale.
# The file "ggmapTemp.png" is written to disk when an OpenStreetMap is loaded.
# The highest possible resolution for a square area is about 2000 x 2000 pixels.
# Scale <- 8600 Utrecht
OSMScale <- 24000  # Scale for Amsterdam.
# works well if BBoxOSMauto <- "no" and with values 
# for left, right, top, bottom as defined above for Amsterdam.
# For Rotterdam (as defined above): Scale <- 22500


# Which zoom level to use for the Stamen Maps? This determines the level of detail. Large values take more time. It does not determine the domain of the area which is plotted.
StamenZoomlevel <- 8
# 15 nice zoom level for map Amsterdam.
# 8 or 9 nice zoom level for whole Netherlands.


# In case of Stamen Maps: which map type should be used?
StamenMapType <- "toner-lite"
# Available map types which seem most useful and work: "toner-hybrid" &, recommended: "toner-lite", "terrain" & "watercolor". 
# It is not possible to make a coloured map black-and-white or vice versa. Hence, it does not make sense to add this option.
###################################



##############################
# LEGENDS, AXES, AND POLYGONS#
##############################
# Transparency of legend, polygons, and link paths:
AlphaPolygon <- 0.6
AlphaLinksTimeStep <- 0.4	# The Netherlands + Amsterdam
AlphaLinksDaily <- 0.4
AlphaLinkLocations <- 0.3


# Number of colour classes in legend:
ColoursNumber <- 5


# Title of legend:
LegendTitleLinksTimeStep <- paste(TIMESTEP,"-min\nrainfall\ndepth\n(mm)\n",sep="")
LegendTitleRadarsTimeStep <- paste(TIMESTEP,"-min\nrainfall\ndepth\n(mm)\n",sep="")
LegendTitleLinksDaily <- paste("Daily\nrainfall\ndepth\n(mm)\n",sep="")
LegendTitleRadarsDaily <- paste("Daily\nrainfall\ndepth\n(mm)\n",sep="")


# Size of legend:
LegendSize <- 75


# Label name of vertical and horizontal axis:
LabelAxisLat <- bquote("Latitude "*(degree))
LabelAxisLonOSM <- bquote(atop("Longitude "*(degree), "\uA9OpenStreetMap contributors; openstreetmap.org")) 
LabelAxisLonGoogle <- bquote(atop("Longitude "*(degree), "\uA9Google Maps")) 
LabelAxisLonStamen <- bquote(atop("Longitude "*(degree), "\uA9Map tiles by Stamen Design. Data by OpenStreetMap.")) 


# Plot grid lines for polygons below threshold ScaleBottomTimeStep or ScaleBottomDaily?
PlotBelowScaleBottom <- "no"
# "yes" for plotting.


# Colour of pixel borders:
# Use NA (without quotes) to not plot pixel borders.
# If the pixels are relatively small with respect to the plotted region, the graphical quality of the pixel borders deteriorates due to low number of pixels (low resolution).
PixelBorderCol <- NA
# "gray55" for gray pixel borders.


# Size of pixel borders:
SizePixelBorder <- 0.8	# 0.2 works well for scale of the Netherlands.


# Lowest class starts at this threshold (minimum rainfall accumulation (mm) to be plotted):
ScaleBottomTimeStep <- 0.1	# mm. 
ScaleBottomDaily <- 1	# mm.
# Using a value clearly above 0 mm can save a lot of computation time if the polygons 
# belonging to values below the threshold are not plotted.


# Highest colour class ends here (maximum rainfall accumulation (mm) to be plotted):
ScaleTopTimeStep <- 7.6 	# mm
ScaleTopDaily <- 26		# mm
# Sometimes the legend is not correctly plotted. In that case try other values for ScaleTop and/or ScaleBottom. 
# Or if the number of classes does not match the number of chosen classes.
# Another way to prevent this is to manually provide the legend breaks:
ManualScale <- "no"	# If "no", manual scale below is not used, and interval breaks are determined automatically. 
# For not equal to "no", interval breaks are determined manually:
ScaleLow <- c(ScaleBottomDaily,6,11,16,21)	# or ScaleBottomTimeStep instead of ScaleBottomDaily.
ScaleHigh <- c(6,11,16,21,ScaleTopDaily)	# or ScaleTopTimeStep instead of ScaleTopDaily.
# Please note that in case of x values in ColoursNumber, ScaleLow and ScaleHigh should also contain x values!
# So, in case you change the number of values in ScaleLow and ScaleHigh: do not forget to change the number for ColoursNumber above accordingly,
# and be aware that the number of colors in ColourScheme below, when given as a list of hexadecimal codes or names, should also change accordingly.
# Example for time steps:
#ScaleBottomTimeStep <- 0.1	# mm. 
#ScaleTopTimeStep <- 10.1	# mm.
#ScaleLow <- c(ScaleBottomTimeStep,2.1,4.1,6.1,8.1)	
#ScaleHigh <- c(2.1,4.1,6.1,8.1,ScaleTopTimeStep)



# Let R automatically define highest value of legend (choose "yes"):
AutDefineLegendTop <- "no"
# In case of "yes" the highest class is not plotted anymore. This is also the case if ManualScale is not equal to "no", although
# the higest value of legend is then determined by ScaleTopDaily.



# Choose colour scheme by using hexadecimal codes or names:
ColourScheme <- rainbow(ColoursNumber+1)
ColourScheme <- c("#DBEED3","#9CD5C4","#71B5C7","#858AC1","#A2569C","#96344E")
ColourScheme <- c("red","orange","lightgreen","darkgreen","blue","purple")
ColourScheme <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494","#a133c8")
# The last entry is the colour of the highest class.
# This website allows you to make suitable color schemes: http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3



######################################
# TITLE OF PLOT AND OUTPUT FILE NAMES#
######################################
# Part of title of plot: 
TitleLinks <- "Links, nearby link approach, outlier filter"
TitleRadars <- "Radars + Gauges"
ExtraText <- "Amsterdam"		# For instance location of map (e.g. a city or country).
ExtraText <- "The Netherlands"


# Part of figure name for links or radars for chosen TIMESTEP or day:
FigFileLinksTimeStep <- paste("Links",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
FigFileRadarsTimeStep <- paste("Radars",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
FigFileLinksDaily <- paste("LinksDaily",gsub(" ","",ExtraText),sep="")
FigFileRadarsDaily <- paste("RadarsDaily",gsub(" ","",ExtraText),sep="")


# Choose output file type of image: jpeg, png or tiff.
OutputFileType <- "jpeg"
######################################



#################
# LINK LOCATIONS#
#################
# Plot locations of links in plot? 
PlotLocLinks <- "yes"
# If "yes" than locations of links are plotted in plot. Note that full-duplex links are plotted twice.


# Colour and size of plotted link paths:
ColourLinks <- "black" # "black" for country-wide maps and Amsterdam.
SizeLinks <- 3 # For Netherlands
#SizeLinks <- 7 # For Amsterdam 
#SizeLinks <- 9 # For Rotterdam
#################



##########################################
# PLOT RAINFALL DEPTH FOR LOCATION ON MAP#
##########################################

# The user can choose to plot a symbol including the corresponding rainfall depth for a specified location on the map.
# A location is plotted on map if PlotLocation is "yes":
PlotLocation <- "no"
# Note that the name of the location is only plotted if the Google API key has been obtained. Otherwise, an error message will be provided and the name will not be plotted.
# However, the rainfall depth and the symbol will be plotted.
# Latitude of location on map (degrees):
LatLocation <- 52.3702165
# Latitude of text (rainfall depth) of location on map (degrees):
LatText <- 52.3702165
# Longitude of location on map (degrees):
LonLocation <- 4.8951685
# Longitude of text (rainfall depth) of location on map (degrees):
LonText <- 4.8951685
# Transparency of plotted symbol for specified location on map:
AlphaPlotLocation <- 0.2
# Colour of plotted symbol for specified location on map:
ColourPlotLocation <- "red"
# Colour of plotted rainfall depth for specified location on map:
ColourPlotLocationText <- "black"
# Size of symbol and and accompanied text for specified location on map:
SizePlotLocation <- 50
# Symbol to be plotted for specified location on map:
SymbolPlotLocation <- "+"



#########################################
# OPTIONS FOR FUNCTION PlotLinkLocations#
#########################################
TitleLinkLocations <- "Locations of microwave link paths"
ExtraTextLinkLocations <- "The Netherlands"
FigFileLinkLocations <- "LinkLocationsTheNetherlands"



###############################################################
# To reproduce Figure 7 from AMT paper: Amsterdam zoomed plot:#
###############################################################
#BBoxOSMauto <- "no"
#MapBackground <- "OSM"
#ColourType <- "color"
#AlphaLinksTimeStep <- 0.4	
#ScaleBottomTimeStep <- 0.2	# mm. 
#ScaleTopTimeStep <- 3.7	# mm
#ExtraText <- "Amsterdam"
#SizeLinks <- 7  
#PixelBorderCol <- "gray55"
#PlotLocation <- "yes"
# Latitude of location on map (degrees):
#LatLocation <- 52.39
# Latitude of text (rainfall depth) of location on map (degrees):
#LatText <- 52.39
# Longitude of location on map (degrees):
#LonLocation <- 4.85
# Longitude of text (rainfall depth) of location on map (degrees):
#LonText <- 4.85
#FigFileLinksTimeStep <- paste("Links",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
#OpenStreetMap can often fail to download, because of problems accessing the server:
#Error: map grabbing failed - see details in ?get_openstreetmap.
#In addition: Warning message:
#In download.file(url, destfile = destfile, quiet = !messaging, mode = "wb") :
#  cannot open URL 'http://tile.openstreetmap.org/cgi-bin/export?bbox=4.84,52.336,4.95,52.404&scale=24000&format=png': HTTP status was '400 #Bad Request'

# TO REPRODUCE Figure 7 WITH GOOGLE MAPS USE:
#BBoxOSMauto <- "no"
#MapBackground <- "Google"
#AlphaLinksTimeStep <- 0.4	
#ScaleBottomTimeStep <- 0.2	# mm. 
#ScaleTopTimeStep <- 3.7	# mm
#ExtraText <- "Amsterdam"
#SizeLinks <- 7  
#PixelBorderCol <- "gray55"
#FigFileLinksTimeStep <- paste("Links",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
#GoogleZoomlevel <- 13
#GoogleLocName <- "Amsterdam"
# To obtain satellite map as background use this:
#GoogleMapType <- "satellite"
#ColourType <- "bw"

# TO REPRODUCE Figure 7 WITH STAMEN MAPS USE:
#BBoxOSMauto <- "no"
#MapBackground <- "Stamen"
#AlphaLinksTimeStep <- 0.4	
#ScaleBottomTimeStep <- 0.2	# mm. 
#ScaleTopTimeStep <- 3.7	# mm
#ExtraText <- "Amsterdam"
#SizeLinks <- 7  
#PixelBorderCol <- "gray55"
#FigFileLinksTimeStep <- paste("Links",gsub(" ","",ExtraText),TIMESTEP,"min",sep="")
#StamenMapType <- "terrain"
#StamenZoomlevel <- 15



