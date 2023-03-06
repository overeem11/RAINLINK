## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.3
## Copyright (C) 2022 Aart Overeem
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

# If the time zone of the employed microwave link dataset is not the same as the (local) time zone used by R on your computer, set the time zone of the microwave link dataset:
# (this is important for functions RefLevelMinMaxRSL, WetDryNearbyLinkApMinMaxRSL and Interpolation):
Sys.setenv(TZ='UTC')
# Otherwise RAINLINK can derive a wrong time interval length due to going to or from daylight saving time (DST). Timing of DST may be different between time zones, or one time zone may not have a change to/from DST.




############################
# 1. PreprocessingMinMaxRSL#
############################

# Load example data:
data(Linkdata)

# For each unique link identifier a time interval is removed if it contains more than one record. This is done by the function "PreprocessingMinMaxRSL".
# In case the records from a certain time interval and unique link identifier are the same, this would throw away data from the entire interval for this identifier,
# whereas the information would be useful. To check how often this occurs:
# length(Linkdata[,1])
# length(unique(Linkdata)[,1])
# To avoid this:
Linkdata <- unique(Linkdata)

# Add column with polarization if this column is not supplied in the link data:
if ("Polarization" %in% names(Linkdata)==FALSE)
{
   Linkdata$Polarization <- rep(NA,nrow(Linkdata))
}
# When no information on polarization is provided, the above code creates a column of NA for Polarization. In the function "RainRetrievalMinMaxRSL.R" links with
# NA values for polarization are processed with a & b values determined for vertically polarized signals.
# If information on polarization of links is available, use H for horizontally polarized & V for vertically polarized in Linkdata$Polarization.
# H, V & NA may occur in the same Linkdata file.

# Run R function:
StartTime <- proc.time()

DataPreprocessed <- PreprocessingMinMaxRSL(Data=Linkdata,MaxFrequency=MaxFrequency,MinFrequency=MinFrequency,verbose=TRUE)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))




############################################
# 2. WetDryNearbyLinkApMinMaxRSL (OPTIONAL)#
############################################

# Run R function:	
StartTime <- proc.time()
 
WetDry <- WetDryNearbyLinkApMinMaxRSL(Data=DataPreprocessed,InputCoorSystem=InputCoorSystem,LocalCartesianCoorSystem=LocalCartesianCoorSystem,MinHoursPmin=MinHoursPmin,PeriodHoursPmin=PeriodHoursPmin,
Radius=Radius,Step8=Step8,ThresholdMedian=ThresholdMedian,ThresholdMedianL=ThresholdMedianL,
ThresholdNumberLinks=ThresholdNumberLinks,ThresholdWetDry=ThresholdWetDry)

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

Pref <- RefLevelMinMaxRSL(Data=DataPreprocessed,Dry=NULL,HoursRefLevel=HoursRefLevel,PeriodHoursRefLevel=PeriodHoursRefLevel)

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
Pcor <- CorrectMinMaxRSL(Data=DataPreprocessed,Dry=NULL,Pref=Pref)




############################
# 6. RainRetrievalMinMaxRSL#
############################

kRPowerLawDataH <- read.table(FileRainRetrHorizontal)
colnames(kRPowerLawDataH) <- c("f", "a", "b")

kRPowerLawDataV <- read.table(FileRainRetrVertical)
colnames(kRPowerLawDataV) <- c("f", "a", "b")


# Run R function:
StartTime <- proc.time()

Rmean <- RainRetrievalMinMaxRSL(Aa=Aa,alpha=alpha,Data=DataOutlierFiltered,kRPowerLawDataH=kRPowerLawDataH,kRPowerLawDataV=kRPowerLawDataV,PmaxCor=Pcor$PmaxCor,PminCor=Pcor$PminCor,Pref=Pref)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))


# If wet-dry classification (function WetDryNearbyLinkApMinMaxRSL) has not been applied, run the R function as follows:
StartTime <- proc.time()

Rmean <- RainRetrievalMinMaxRSL(Aa=Aa,alpha=alpha,Data=DataPreprocessed,kRPowerLawDataH=kRPowerLawDataH,kRPowerLawDataV=kRPowerLawDataV,PmaxCor=Pcor$PmaxCor,PminCor=Pcor$PminCor,Pref=Pref)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))


# Write path-average rainfall data to files:
# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15	
	
# Location of output link data:
FolderRainEstimates <- paste("LinkPathRainDepths",TIMESTEP,"min",sep="")
ToFile = TRUE
if (ToFile)
{	
	# Create directory for output files:
	if(!dir.exists(FolderRainEstimates)){ dir.create(FolderRainEstimates) }
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
		# Using na.omit removes all rows with at least 1 NA:
		write.table(na.omit(int_data), Filename, row.names = FALSE, col.names = TRUE, append = FALSE, quote = FALSE)
	}
}
# Note that the output files contain rainfall depths (mm). If these data are to be used for the interpolation, they must first be read ("Interpolation.R" does not read these files).
# Using the data for "Interpolation.R" requires a conversion from rainfall depth (mm) to rainfall intensity (mm/h).




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

Interpolation(Data=DataPreprocessed,idp=idp,InputCoorSystem,IntpMethod=IntpMethod,LocalCartesianCoorSystem=LocalCartesianCoorSystem,
nmax=nmax,NUGGET=NUGGET,RANGE=RANGE,RainGrid=RainGrid,Rmean=Rmean,SILL=SILL,TimeZone=TimeZone,Variogram=Variogram,OutputDir=FolderRainMaps)

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))




#######################################################################################################################
# 8. Code to accumulate CML rainfall maps and to read the accumulated rainfall for a given location - ReadRainLocation#
#######################################################################################################################

# All visualisation functions have been removed as of RAINLINK version 1.3. Python software MapRAINLINK is now publicly available to 
# visualize rain gauge, radar, and commercial microwave link (CML) locations and their rainfall estimates on a map 
# (https://github.com/overeem11/MapRAINLINK). Hence, large parts of 8. and 9. entirely have been removed.
# Note that the plotting of data availability and topology have been kept.


# Code to obtain rainfall accumulation for a period. E.g., just obtain 15-min rainfall or accumulated 24-h rainfall:#
#####################################################################################################################
# Select date and time for which interpolated rainfall needs to be extracted / accumulated:
# Date and time at which rainfall mapping starts:
DateTimeStartRainMaps <- "201109100800"
# Date and time at which rainfall mapping ends:
DateTimeEndRainMaps <- "201109110800"
# Do not use "0000" for 0000 UTC, but "2400".

# Location of output link data:
FolderRainMaps <- paste("RainMapsLinks",TIMESTEP,"min",sep="")

# Duration of time interval of sampling strategy (min):
TIMESTEP <- 15
# Conversion factor from rainfall intensity (mm/h) to depth (mm):
MinutesHour <- 60
ConversionIntensityToDepth <- TIMESTEP/MinutesHour
# Make list of input files: 
Files <- list.files(path = FolderRainMaps, all.files=FALSE,
full.names=TRUE, recursive=FALSE, pattern="linkmap")
FilesNames <- list.files(path = FolderRainMaps, all.files=FALSE,
full.names=FALSE, recursive=FALSE, pattern="linkmap")
DateFiles <- substr(FilesNames,9,20)
# Only select files for supplied period:
Files <- Files[which(DateFiles>DateTimeStartRainMaps &DateFiles<=DateTimeEndRainMaps )]
Files <- Files[which(file.info(Files)$size>0)]
if (length(Files)==0)
{
	print("No files with data! Function stops.")
	stop()
}

# Compute data availability (percentage of available files):
MinutesDay <- 1440
NrStepsDay <- MinutesDay/TIMESTEP
DataAvail <- 100 * length(Files)/NrStepsDay

# Compute rainfall depths over period from mean rainfall intensities:
Rperiod <- c(NA)
Grid <- read.table(FileGrid,header=TRUE,sep=",")	
NrGridPoints <- length(Grid[,1])
Rperiod[1:NrGridPoints] <- 0
for (FileNr in 1:length(Files))
{
	# Read interpolated link rainfall intensities:
	Data <- read.table(Files[FileNr],header=TRUE)
	Rperiod[1:NrGridPoints] <- Rperiod[1:NrGridPoints] + Data$RainIntensity[1:NrGridPoints]
}
# Convert rainfall intensities to rainfall depths:
Rperiod <- Rperiod * ConversionIntensityToDepth


# Obtain rainfall at location with coordinates Lon, Lat:
########################################################
# Location for which rainfall depth is to be extracted (here provided in WGS84, degrees, EPSG code 4326L:
Lon <- 6.1214201
Lat <- 52

# Run function ReadRainLocation to read (accumulated) rainfall:
ReadRainLocation(dataf=Rperiod,FileGrid=FileGrid,InputCoorSystem=InputCoorSystem,Lat=Lat,LocalCartesianCoorSystem=LocalCartesianCoorSystem,Lon=Lon)




####################
# 10. Plot topology#
####################

Topology(Data=DataPreprocessed,FigNameBarplotAngle=FigNameBarplotAngle,FigNameBarplotFrequency=FigNameBarplotFrequency,
FigNameBarplotPathLength=FigNameBarplotPathLength,FigNameFrequencyVsPathLength=FigNameFrequencyVsPathLength,
FigNameScatterdensityplotFrequencyVsPathLength=FigNameScatterdensityplotFrequencyVsPathLength,InputCoorSystem=InputCoorSystem,
LocalCartesianCoorSystem=LocalCartesianCoorSystem,Maxf=Maxf,Minf=Minf,MaxL=MaxL,MinL=MinL,Rmean=Rmean,Stepf=Stepf,StepL=StepL)

# Note that Data object must be preprocessed if Rmean is provided.




#############################
# 11. Plot data availability#
#############################

DataAvailability(Data=DataPreprocessed,cex.axis=cex.axis,cex.lab=cex.lab,FigNameBarplotAvailabilityLinks=FigNameBarplotAvailabilityLinks,
FigNameBarplotAvailabilityLinkPaths=FigNameBarplotAvailabilityLinkPaths,
FigNameTimeseriesAvailability=FigNameTimeseriesAvailability,ps=ps,Rmean=Rmean,TimeZone=TimeZone)

# Note that Data must be preprocessed, because Rmean is used.
# Another remark concerns the function "DataAvailability". In the figure showing the time series of data availability, the first period reveals 0 availability. 
# This is due to the spin-up time of RAINLINK. This period is also taken into account in the computations of data availability. To prevent this, the first period
# should be removed from the Data and Rmean object as provided to function "DataAvailability". 




########################################
# 12. Compute path length in kilometers#
########################################

PathLength(InputCoorSystem=InputCoorSystem,XStart=Linkdata$XStart,XEnd=Linkdata$XEnd,YStart=Linkdata$YStart,YEnd=Linkdata$YEnd)




