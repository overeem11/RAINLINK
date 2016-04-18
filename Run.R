## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.1
## Copyright (C) 2016 Aart Overeem
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

## R program 'Run.R'.
## SCRIPT FOR RAINFALL ESTIMATION USING MICROWAVE LINKS.
## source("Run.R")
## Run this script by pasting the line above (source(...)) or by pasting parts of the script into the R shell.


# Note that it is not necessarily a problem if a function argument is not supplied to the function. If the
# function argument is not used, then there is no problem. Only be aware that you should use e.g.
# MaxFrequency=MaxFrequency. I.e. if you only supply MaxFrequency and the function argument before
# MaxFrequency is missing, than the function will not execute properly.


#############################################################
# 0. Load R libraries, parameter values, and other settings.#
# This also loads the RAINLINK package.                     #
#############################################################

source("Config.R") 



############################
# 1. PreprocessingMinMaxRSL#
############################

# Load example data:
data(Linkdata)


# Run R function:
StartTime <- proc.time()

DataPreprocessed <- PreprocessingMinMaxRSL(Data=Linkdata,MaxFrequency=MaxFrequency,MinFrequency=MinFrequency,verbose=TRUE)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))




############################################
# 2. WetDryNearbyLinkApMinMaxRSL (OPTIONAL)#
############################################

# Run R function:	
StartTime <- proc.time()

WetDry <- WetDryNearbyLinkApMinMaxRSL(Data=DataPreprocessed,CoorSystemInputData=NULL, 
MinHoursPmin=MinHoursPmin,PeriodHoursPmin=PeriodHoursPmin,Radius=Radius,Step8=Step8, 
ThresholdMedian=ThresholdMedian,ThresholdMedianL=ThresholdMedianL,ThresholdNumberLinks=ThresholdNumberLinks, 
ThresholdWetDry=ThresholdWetDry)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))




#######################
# 3. RefLevelMinMaxRSL#
#######################

# Run R function:
StartTime <- proc.time()

Pref <- RefLevelMinMaxRSL(Data=DataPreprocessed,Dry=WetDry$Dry,HoursRefLevel=HoursRefLevel,PeriodHoursRefLevel=PeriodHoursRefLevel)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))


# If wet-dry classification (function WetDryNearbyLinkApMinMaxRSL) has not been applied, run the R function as follows:
StartTime <- proc.time()

Pref <- RefLevelMinMaxRSL(Data=DataPreprocessed,HoursRefLevel=HoursRefLevel,PeriodHoursRefLevel=PeriodHoursRefLevel)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))




#############################################################################################################
# 4. OutlierFilterMinMax (OPTIONAL) - Can only be applied when WetDryNearbyLinkApMinMaxRSL has been applied.#
#############################################################################################################

# Run R function:
DataOutlierFiltered <- OutlierFilterMinMaxRSL(Data=DataPreprocessed,F=WetDry$F,FilterThreshold=FilterThreshold)




######################
# 5. CorrectMinMaxRSL#
######################

# Run R function:
Pcor <- CorrectMinMaxRSL(Data=DataOutlierFiltered,Dry=WetDry$Dry,Pref=Pref)


# If wet-dry classification (function WetDryNearbyLinkApMinMaxRSL) has not been applied, run the R function as follows:
Pcor <- CorrectMinMaxRSL(Data=DataPreprocessed,Pref=Pref)



############################
# 6. RainRetrievalMinMaxRSL#
############################

kRPowerLawData <- read.table(FileRainRetr)
colnames(kRPowerLawData) <- c("f", "a", "b")

# Run R function:
StartTime <- proc.time()

Rmean <- RainRetrievalMinMaxRSL(Aa=Aa,alpha=alpha,Data=DataOutlierFiltered,kRPowerLawData=kRPowerLawData,PmaxCor=Pcor$PmaxCor,PminCor=Pcor$PminCor,Pref=Pref)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))


# If wet-dry classification (function WetDryNearbyLinkApMinMaxRSL) has not been applied, run the R function as follows:
StartTime <- proc.time()

Rmean <- RainRetrievalMinMaxRSL(Aa=Aa,alpha=alpha,Data=DataPreprocessed,kRPowerLawData=kRPowerLawData,PmaxCor=Pcor$PmaxCor,PminCor=Pcor$PminCor,Pref=Pref)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))


# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15	
	
# Location of output link data:
FolderRainEstimates <- paste("LinkPathRainDepths",TIMESTEP,"min",sep="")
ToFile = TRUE
if (ToFile)
{	
	# Create directory for output files:
	dir.create(FolderRainEstimates)
	# Write output to file
	ID <- unique(DataPreprocessed$ID)
	t <- sort(unique(DataPreprocessed$DateTime))
	t_sec <- as.numeric(as.POSIXct(as.character(t), format = "%Y%m%d%H%M"))
	dt <- min(diff(t_sec))
	
	for (i in 1 : length(t))
	{
		ind <- which(DataPreprocessed$DateTime == t[i])
		int_data <- data.frame(ID = DataPreprocessed$ID[ind], RainfallDepthPath = Rmean[ind] * dt / 3600, 
		PathLength = DataPreprocessed$PathLength[ind], XStart = DataPreprocessed$XStart[ind], 
		YStart = DataPreprocessed$YStart[ind], XEnd = DataPreprocessed$XEnd[ind], 
		YEnd = DataPreprocessed$YEnd[ind], IntervalNumber = rep(i, length(ind)), 
		Frequency = DataPreprocessed$Frequency[ind])
		
		Filename <- paste(FolderRainEstimates, "/linkdata_", t[i], ".dat", sep="")
		write.table(int_data, Filename, row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE)
	}
}




###################
# 7. Interpolation#
###################

# Read grid onto which data are interpolated
RainGrid <- read.table(FileGrid, header = TRUE, sep=",")

# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15		

# Location of output link data:
FolderRainMaps <- paste("RainMapsLinks",TIMESTEP,"min",sep="")

# Run R function:
StartTime <- proc.time()

Interpolation(Data=DataPreprocessed,CoorSystemInputData=NULL,idp=idp,IntpMethod=IntpMethod,nmax=nmax,
NUGGET=NUGGET,RANGE=RANGE,RainGrid=RainGrid,Rmean=Rmean,SILL=SILL,Variogram=Variogram,OutputDir=FolderRainMaps)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))




###################
# 8. Visualisation#
###################

############################
# 8.1 RainMapsLinksTimeStep#
############################

# Date and time at which rainfall mapping starts:
DateTimeStartRainMaps <- "201109102030"
# Date and time at which rainfall mapping ends:
DateTimeEndRainMaps <- "201109102045"

# Run function RainMapsLinksTimeStep:
RainMapsLinksTimeStep(AlphaLinksTimeStep=AlphaLinksTimeStep,AlphaPlotLocation=AlphaPlotLocation,AlphaPolygon=AlphaPolygon,
AlphaScale=AlphaScale,AutDefineLegendTop=AutDefineLegendTop,BBoxOSMauto=BBoxOSMauto,ColourLinks=ColourLinks,ColoursNumber=ColoursNumber,ColourPlotLocation=ColourPlotLocation,
ColourPlotLocationText=ColourPlotLocationText,ColourType=ColourType,ColourHighestClass=ColourHighestClass,ConversionDepthToIntensity=ConversionDepthToIntensity,
CoorSystemInputData=CoorSystemInputData,DateTimeEndRainMaps=DateTimeEndRainMaps,DateTimeStartRainMaps=DateTimeStartRainMaps,ExtraDeg=ExtraDeg,ExtraText=ExtraText,
FigFileLinksTimeStep=FigFileLinksTimeStep,FigHeight=FigHeight,FigWidth=FigWidth,FileGrid=FileGrid,FilePolygonsGrid=FilePolygonsGrid,FolderFigures=FolderFigures,
FolderRainMaps=FolderRainMaps,FolderRainEstimates=FolderRainEstimates,FontFamily=FontFamily,GoogleLocDegSpecified=GoogleLocDegSpecified,GoogleLocLat=GoogleLocLat,
GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,GoogleLocNameSpecified=GoogleLocNameSpecified,GoogleMapType=GoogleMapType,GoogleZoomlevel=GoogleZoomlevel,
LabelAxisLat=LabelAxisLat,LabelAxisLonGoogle=LabelAxisLonGoogle,LabelAxisLonOSM=LabelAxisLonOSM,LatLocation=LatLocation,LatText=LatText,
LegendTitleLinksTimeStep=LegendTitleLinksTimeStep,LonLocation=LonLocation,LonText=LonText,ManualScale=ManualScale,MapBackground=MapBackground,OSMBottom=OSMBottom,
OSMLeft=OSMLeft,OSMRight=OSMRight,OSMScale=OSMScale,OSMTop=OSMTop,Palette=Palette,PlotLocation=PlotLocation,PixelBorderCol=PixelBorderCol,
PlotBelowScaleBottom=PlotBelowScaleBottom,PlotLocLinks=PlotLocLinks,ScaleBottomTimeStep=ScaleBottomTimeStep,ScaleHigh=ScaleHigh,ScaleLow=ScaleLow,
ScaleTopTimeStep=ScaleTopTimeStep,SizeLinks=SizeLinks,SizePixelBorder=SizePixelBorder,SizePlotLocation=SizePlotLocation,SizePlotTitle=SizePlotTitle,
SymbolPlotLocation=SymbolPlotLocation,TitleLinks=TitleLinks,XMiddle=XMiddle,YMiddle=YMiddle)




#########################
# 8.2 RainMapsLinksDaily#
#########################

# Date and time at which rainfall mapping starts:
DateTimeStartRainMaps <- "201109100800"
# Date and time at which rainfall mapping ends:
DateTimeEndRainMaps <- "201109110800"

# Run function RainMapsLinksDaily:
RainMapsLinksDaily(AlphaLinksDaily=AlphaLinksDaily,AlphaPlotLocation=AlphaPlotLocation,AlphaPolygon=AlphaPolygon,AlphaScale=AlphaScale,
AutDefineLegendTop=AutDefineLegendTop,BBoxOSMauto=BBoxOSMauto,ColourLinks=ColourLinks,ColoursNumber=ColoursNumber,ColourPlotLocation=ColourPlotLocation,
ColourPlotLocationText=ColourPlotLocationText,ColourType=ColourType,ColourHighestClass=ColourHighestClass,ConversionDepthToIntensity=ConversionDepthToIntensity,
CoorSystemInputData=CoorSystemInputData,DateTimeEndRainMaps=DateTimeEndRainMaps,DateTimeStartRainMaps=DateTimeStartRainMaps,ExtraDeg=ExtraDeg,
ExtraText=ExtraText,FigFileLinksDaily=FigFileLinksDaily,FigHeight=FigHeight,FigWidth=FigWidth,FileGrid=FileGrid,FilePolygonsGrid=FilePolygonsGrid,
FolderFigures=FolderFigures,FolderRainMaps=FolderRainMaps,FolderRainEstimates=FolderRainEstimates,FontFamily=FontFamily,
GoogleLocDegSpecified=GoogleLocDegSpecified,GoogleLocLat=GoogleLocLat,GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,
GoogleLocNameSpecified=GoogleLocNameSpecified,GoogleMapType=GoogleMapType,GoogleZoomlevel=GoogleZoomlevel,LabelAxisLat=LabelAxisLat,
LabelAxisLonGoogle=LabelAxisLonGoogle,LabelAxisLonOSM=LabelAxisLonOSM,LatLocation=LatLocation,LatText=LatText,
LegendTitleLinksDaily=LegendTitleLinksDaily,LonLocation=LonLocation,LonText=LonText,ManualScale=ManualScale,MapBackground=MapBackground,
OSMBottom=OSMBottom,OSMLeft=OSMLeft,OSMRight=OSMRight,OSMScale=OSMScale,OSMTop=OSMTop,Palette=Palette,PERIOD=PERIOD,PlotLocation=PlotLocation,
PixelBorderCol=PixelBorderCol,PlotBelowScaleBottom=PlotBelowScaleBottom,PlotLocLinks=PlotLocLinks,ScaleBottomDaily=ScaleBottomDaily,
ScaleHigh=ScaleHigh,ScaleLow=ScaleLow,ScaleTopDaily=ScaleTopDaily,SizeLinks=SizeLinks,SizePixelBorder=SizePixelBorder,SizePlotLocation=SizePlotLocation,
SizePlotTitle=SizePlotTitle,SymbolPlotLocation=SymbolPlotLocation,TIMESTEP=TIMESTEP,TitleLinks=TitleLinks,XMiddle=XMiddle,YMiddle=YMiddle)




#############################
# 8.3 RainMapsRadarsTimeStep#
#############################

# Name and path of daily radar input file:
# NetCDF4 file for daily rainfall depths:
# Download from Climate4Impact (works at least for Linux):
# path <- "http://opendap.knmi.nl/knmi/thredds/fileServer/radarprecipclim/RAD_NL25_RAC_MFBS_24H_NC/2011/09/RAD_NL25_RAC_MFBS_24H_201109110800.nc"
# download.file(path0,"RAD_NL25_RAC_MFBS_24H_201109110800.nc",method="wget",quiet=F,mode="wb",cacheOK=T)
FileNameRadarDaily <- "RAD_NL25_RAC_MFBS_24H_201109110800.nc"
# Path in NetCDF4 file with radar data:
PathRadarRainfallDepth <- "image1_image_data"
# Name of folder which contains 5-min radar rainfall files
FolderRadarRainMapsTimeStep <- "Radar5min"

# Run function RainMapsRadarsTimeStep:
RainMapsRadarsTimeStep(AlphaPlotLocation=AlphaPlotLocation,AlphaPolygon=AlphaPolygon,AlphaScale=AlphaScale,
AutDefineLegendTop=AutDefineLegendTop,BBoxOSMauto=BBoxOSMauto,ColoursNumber=ColoursNumber,ColourPlotLocation=ColourPlotLocation,
ColourPlotLocationText=ColourPlotLocationText,ColourType=ColourType,ColourHighestClass=ColourHighestClass,
CoorSystemInputData=CoorSystemInputData,ExtraDeg=ExtraDeg,ExtraText=ExtraText,FigFileRadarsTimeStep=FigFileRadarsTimeStep,FigHeight=FigHeight,
FigWidth=FigWidth,FileGrid=FileGrid,FilePolygonsGrid=FilePolygonsGrid,FolderFigures=FolderFigures,FolderRadarRainMapsTimeStep=FolderRadarRainMapsTimeStep,
FontFamily=FontFamily,GoogleLocDegSpecified=GoogleLocDegSpecified,GoogleLocLat=GoogleLocLat,
GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,GoogleLocNameSpecified=GoogleLocNameSpecified,GoogleMapType=GoogleMapType,GoogleZoomlevel=GoogleZoomlevel,
LabelAxisLat=LabelAxisLat,LabelAxisLonGoogle=LabelAxisLonGoogle,LabelAxisLonOSM=LabelAxisLonOSM,LatLocation=LatLocation,LatText=LatText,
LegendTitleRadarsTimeStep=LegendTitleRadarsTimeStep,LonLocation=LonLocation,LonText=LonText,ManualScale=ManualScale,MapBackground=MapBackground,
OSMBottom=OSMBottom,OSMLeft=OSMLeft,OSMRight=OSMRight,OSMScale=OSMScale,OSMTop=OSMTop,Palette=Palette,PathRadarRainfallDepth=PathRadarRainfallDepth,
PERIOD=PERIOD,PlotLocation=PlotLocation,PixelBorderCol=PixelBorderCol,PlotBelowScaleBottom=PlotBelowScaleBottom,
ScaleBottomTimeStep=ScaleBottomTimeStep,ScaleHigh=ScaleHigh,ScaleLow=ScaleLow,ScaleTopTimeStep=ScaleTopTimeStep,SizePixelBorder=SizePixelBorder,
SizePlotLocation=SizePlotLocation,SizePlotTitle=SizePlotTitle,SymbolPlotLocation=SymbolPlotLocation,TIMESTEP=TIMESTEP,TimeZone=TimeZone,TitleRadars=TitleRadars,XMiddle=XMiddle,
YMiddle=YMiddle)




##########################
# 8.4 RainMapsRadarsDaily#
##########################

DateMap <- "20110911"
# Name and path of daily radar input file:
# NetCDF4 file for daily rainfall depths:
# Download from Climate4Impact (works at least for Linux):
# path <- "http://opendap.knmi.nl/knmi/thredds/fileServer/radarprecipclim/RAD_NL25_RAC_MFBS_24H_NC/2011/09/RAD_NL25_RAC_MFBS_24H_201109110800.nc"
# download.file(path0,"RAD_NL25_RAC_MFBS_24H_201109110800.nc",method="wget",quiet=F,mode="wb",cacheOK=T)
FileNameRadarDaily <- "RAD_NL25_RAC_MFBS_24H_201109110800.nc"
# Path in NetCDF4 file with radar data:
PathRadarRainfallDepth <- "image1_image_data"
# Name of folder which contains daily radar rainfall files
FolderRadarRainMapsDaily <- "Radar24H"

# Run function RainMapsRadarsDaily:
RainMapsRadarsDaily(AlphaPlotLocation=AlphaPlotLocation,AlphaPolygon=AlphaPolygon,AlphaScale=AlphaScale,
AutDefineLegendTop=AutDefineLegendTop,BBoxOSMauto=BBoxOSMauto,ColoursNumber=ColoursNumber,ColourPlotLocation=ColourPlotLocation,
ColourPlotLocationText=ColourPlotLocationText,ColourType=ColourType,ColourHighestClass=ColourHighestClass,
CoorSystemInputData=CoorSystemInputData,DateMap=DateMap,ExtraDeg=ExtraDeg,ExtraText=ExtraText,FigFileRadarsDaily=FigFileRadarsDaily,FigHeight=FigHeight,
FigWidth=FigWidth,FileGrid=FileGrid,FileNameRadarDaily=FileNameRadarDaily,FilePolygonsGrid=FilePolygonsGrid,FolderFigures=FolderFigures,FolderRadarRainMapsDaily=FolderRadarRainMapsDaily,
FontFamily=FontFamily,GoogleLocDegSpecified=GoogleLocDegSpecified,GoogleLocLat=GoogleLocLat,
GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,GoogleLocNameSpecified=GoogleLocNameSpecified,GoogleMapType=GoogleMapType,GoogleZoomlevel=GoogleZoomlevel,
LabelAxisLat=LabelAxisLat,LabelAxisLonGoogle=LabelAxisLonGoogle,LabelAxisLonOSM=LabelAxisLonOSM,LatLocation=LatLocation,LatText=LatText,
LegendTitleRadarsDaily=LegendTitleRadarsDaily,LonLocation=LonLocation,LonText=LonText,ManualScale=ManualScale,MapBackground=MapBackground,
OSMBottom=OSMBottom,OSMLeft=OSMLeft,OSMRight=OSMRight,OSMScale=OSMScale,OSMTop=OSMTop,Palette=Palette,PathRadarRainfallDepth=PathRadarRainfallDepth,
PERIOD=PERIOD,PlotLocation=PlotLocation,PixelBorderCol=PixelBorderCol,PlotBelowScaleBottom=PlotBelowScaleBottom,
ScaleBottomDaily=ScaleBottomDaily,ScaleHigh=ScaleHigh,ScaleLow=ScaleLow,ScaleTopDaily=ScaleTopDaily,SizePixelBorder=SizePixelBorder,
SizePlotLocation=SizePlotLocation,SizePlotTitle=SizePlotTitle,SymbolPlotLocation=SymbolPlotLocation,TitleRadars=TitleRadars,XMiddle=XMiddle,
YMiddle=YMiddle)




#######################
# 8.5 ReadRainLocation#
#######################

# Load (daily) radar rainfall data:
ncFILE <- nc_open(paste(FolderRadarRainMapsDaily,"/",FileNameRadarDaily,sep=""),verbose=F)
dataf <- c(ncvar_get(ncFILE,varid="image1_image_data"))
dataf <- dataf[!is.na(dataf)]
nc_close(ncFILE)

# File with interpolation grid in same coordinate system as CoorSystemInputData:
FileGrid <- "InterpolationGrid.dat"	# WGS84 (longitude, latitude (degrees))

# OR load e.g. 15-min link data:
# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15	
# Conversion factor from rainfall intensity (mm/h) to depth (mm):
MinutesHour <- 60
ConversionIntensityToDepth <- TIMESTEP/MinutesHour
# Location of link data:
FolderRainMaps <- paste("RainMapsLinks",TIMESTEP,"min",sep="")
# Make list of input files: 
Files <- list.files(path = FolderRainMaps, all.files=FALSE,
full.names=TRUE, recursive=FALSE, pattern="linkmap")
# Select date and time for which links are to be plotted:
DateTime <- "201109110200"
condTime <- grep(DateTime,Files)
# Select file:
Filename <- Files[condTime]
# Read data from input file:
dataf <- read.table(Filename,header=TRUE)
dataf <- ConversionIntensityToDepth * dataf[,1]

# Location for which rainfall depth is to be extracted:
Lon = 5.1214201
Lat = 52.0907374

# Run function ReadRainLocation:
ReadRainLocation(CoorSystemInputData=CoorSystemInputData,dataf=dataf,FileGrid=FileGrid,Lat=Lat,Lon=Lon,XMiddle=XMiddle,YMiddle=YMiddle)

# R provides tools to extract the street name and municipality for a location:
revgeocode(c(Lon,Lat))

# If you would like to know the rainfall depth for a street name and municipality, R can provide you with the location in degrees:
# Give latitude and longitude for location known by street name and municipality:
geocode("Domplein 1, Utrecht")
    



#######################
# 9. PlotLinkLocations#
#######################

# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15	
# Location of link data:
FolderRainEstimates <- paste("LinkPathRainDepths",TIMESTEP,"min",sep="")
# Make list of input files: 
Files <- list.files(path = FolderRainEstimates, all.files=FALSE,
full.names=TRUE, recursive=FALSE, pattern="linkdata")
# Select date and time for which links are to be plotted:
DateTime <- "201109110200"
condTime <- grep(DateTime,Files)
# Select file:
Filename <- Files[condTime]
# Read data from input file:
dataf <- read.table(Filename,header=TRUE)

# Plot link locations on a map:
PlotLinkLocations(AlphaLinkLocations=AlphaLinkLocations,BBoxOSMauto=BBoxOSMauto,OSMBottom=OSMBottom,ColourLinks=ColourLinks,
ColourType=ColourType,dataf=dataf,DateTime=DateTime,ExtraTextLinkLocations=ExtraTextLinkLocations,FigFileLinkLocations=FigFileLinkLocations,
FigHeight=FigHeight,FigWidth=FigWidth,FolderFigures=FolderFigures,FontFamily=FontFamily,GoogleLocDegSpecified=GoogleLocDegSpecified,
GoogleLocLat=GoogleLocLat,GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,GoogleLocNameSpecified=GoogleLocNameSpecified,
GoogleMapType=GoogleMapType,GoogleZoomlevel=GoogleZoomlevel,LabelAxisLat=LabelAxisLat,LabelAxisLonGoogle=LabelAxisLonGoogle,
LabelAxisLonOSM=LabelAxisLonOSM,OSMLeft=OSMLeft,MapBackground=MapBackground,OSMRight=OSMRight,OSMScale=OSMScale,
SizeLinks=SizeLinks,SizePlotTitle=SizePlotTitle,TitleLinkLocations=TitleLinkLocations,OSMTop=OSMTop)



