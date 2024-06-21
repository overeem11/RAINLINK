## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.31
## Copyright (C) 2024 Aart Overeem
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
## Contains the parameter values, coordinate systems, and folder names for rainfall
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
	library(sf,lib.loc=pathlib)
	library(s2,lib.loc=pathlib)
	library(gstat,lib.loc=pathlib)	
	library(hexbin,lib.loc=pathlib)
	library(sfheaders,lib.loc=pathlib)
	library(abind,lib.loc=pathlib)
        library(stars,lib.loc=pathlib)
}
if (GivePathLib=="no")
{
	# Load RAINLINK package:
	library(RAINLINK)
	
	# Load other packages:
	library(sf)
	library(s2)
	library(gstat)	
	library(hexbin)
	library(sfheaders)
	library(abind)
        library(stars)
}
if (GivePathLib!="no"&GivePathLib!="yes")
{
	print("Please specify whether path of R libraries is given by you!")
	stop()
}




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

# At https://epsg.io/ you can find suitable EPSG codes for your region.
# Define EPSG code for input coordinate system (e.g., 4326L for WGS84 in degrees):
InputCoorSystem <- 4326L
# crs = 4326L is EPSG:4326. WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS.

# Define EPSG code for (local) Cartesian coordinate system (meters):
LocalCartesianCoorSystem <- 28992
# LocalCartesianCoorSystem <- 4978 # Worldwide cartesian EPSG (meters), which can be used over the entire globe (https://epsg.io/4978), 
# but is likely less accurate than local Cartesian coordinate systems. Quite some differences were found for the Netherlands.




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

# InputCoorSystem & LocalCartesianCoorSystem are already defined below 2. WetDryNearbyLinkApMinMaxRSL.

# File with interpolation grid in same coordinate system as InputCoorSystem:
FileGrid <- "InterpolationGrid.dat"	# Example file is WGS84 (longitude, latitude (decimal degrees)), EPSG code 4326L.

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

# Time zone:
TimeZone <- "UTC"
Sys.setenv <- (TZ=TimeZone)




######################
# 8. ReadRainLocation#
######################

# InputCoorSystem & LocalCartesianCoorSystem are already defined below 2. WetDryNearbyLinkApMinMaxRSL.




####################
# 10. Plot Topology#
####################

# InputCoorSystem & LocalCartesianCoorSystem are already defined below 2. WetDryNearbyLinkApMinMaxRSL.

# Folder name of figures:
FolderFigures <- "Figures"

# Directories and names of output files:
FigNameBarplotAngle=paste(FolderFigures,"/Barplot_Orientation.pdf",sep="")
FigNameBarplotFrequency=paste(FolderFigures,"/Barplot_Frequency.pdf",sep="")
FigNameBarplotPathLength=paste(FolderFigures,"/Barplot_PathLength.pdf",sep="")
FigNameFrequencyVsPathLength=paste(FolderFigures,"/Frequency_vs_PathLength.pdf",sep="")
FigNameScatterdensityplotFrequencyVsPathLength=paste(FolderFigures,"/ScatterdensityPlot_Frequency_vs_PathLength.pdf",sep="")
# Maximum percentage on scale for bar plot of microwave frequency:
MaxPercFrequency=40
# Maximum percentage on scale for bar plot of orientation:
MaxPercOrientation=7
# Maximum percentage on scale for bar plot of path length:
MaxPercPathLength=30
# Maximum microwave frequency to be plotted in bar plot (GHz). This is the value where the last bin class ends:
Maxf=40
# Minimum microwave frequency to be plotted in bar plot (GHz). This is the value where the first bin class ends:
Minf=7
# Maximum link path length to be plotted in bar plot (km). This is the value where the last bin class ends:
MaxL=22
# Minimum link path length to be plotted in bar plot (km). This is the value where the first bin class ends:
MinL=1
# Title of plots (e.g., which CML vendor or period):
PlotTitleTopology <- "Topology"
# Stepf Bin size of microwave frequency classes for bar plot in GHz:
Stepf=1
# StepL Bin size of link path length classes for bar plot in km:
StepL=1




#############################
# 11. Plot data availability#
#############################

# TimeZone is already defined below 7. Interpolation.
# FolderFigures is already defined below 10. Plot Topology.

# Size of axis annotation:
cex.axis=0.9
# Size of x and y labels:
cex.lab=1.15
# Directories and names of output files
FigNameBarplotAvailabilityLinks=paste(FolderFigures,"/Barplot_Availability_Links.pdf",sep="")
FigNameBarplotAvailabilityLinkPaths=paste(FolderFigures,"/Barplot_Availability_LinkPaths.pdf",sep="")
FigNameTimeseriesAvailability=paste(FolderFigures,"/TimeseriesAvailability.pdf",sep="")
# Location of legend of figure with time series of number of sub-links and link paths. Choose from "bottomright", "bottomright", "bottom", 
# "bottomleft", "left", "topleft", "top", "topright", "right" and "center":
LocationLegendTimeseriesAvailability <- "bottomright"
# Maximum percentage on scale for bar plot of link path availability:
MaxPercLinkPaths <- 100
# Maximum percentage on scale for bar plot of sub-link availability:
MaxPercSubLinks <- 100
# Title of plots (e.g., which CML vendor or period):
PlotTitleDataAvailability <- "Timeseries of data availability"
# The point size of text:
ps=18




##########################
# 12. Compute path length#
##########################

# InputCoorSystem is already defined below 2. WetDryNearbyLinkApMinMaxRSL.







