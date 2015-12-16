## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
## 
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

#' Function for preprocessing of microwave link data.
#' @description Function for preprocessing of microwave link data. Works for sampling strategy where minimum and maximum received powers are provided. This function performs the following tasks:  
#' \enumerate{
#'   \item Time interval numbers are computed.
#'   \item Link data are selected for microwave frequencies within chosen range.
#'   \item Data selection criteria are applied.
#'   \item Link coordinates are converted to Cartesian coordinate system. 
#'   \item Data from previous and present 24-h period are combined into one file for each day for which rainfall maps need to be obtained. 
#' }
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item TIMESTEP: Duration of time interval of sampling strategy (min).
#'   \item FolderStart: Folder name of input data.
#'   \item FolderPreprocessed: Folder name of output data.
#'   \item TimeZone: Time zone of data (e.g. "UTC").
#'   \item PERIOD: Select daily time interval, i.e., "0800" implies 0800 UTC previous day - 0800 UTC present day (use 2400 for 0000 UTC).
#'   \item MinFrequency: Minimum allowed microwave frequency of link in output (GHz).
#'   \item MaxFrequency: Maximum allowed microwave frequency of link in output (GHz).
#'   \item CoorSystemInputData: Coordinate system of input data (e.g. "+init=epsg:4326"	for WGS84 in degrees).
#'   \item projstring: Proj4string of an Azimuthal Equidistant Cartesian output coordinate system.
#' }
#' @export PreprocessingMinMax
#' @examples
#' -
#' 

PreprocessingMinMax <- function() 
{

	StartTime <- proc.time()

	# Create directory for output files:
	dir.create(FolderPreprocessed)

	# Compute number of time intervals:
	NrTimeStepsHour <- 60/TIMESTEP
	MinutesDay <- 1440
	NrStepsDay <- MinutesDay/TIMESTEP
	NrStepsTwoDays <- 2 * NrStepsDay


	# Make list of input files: 
	Files <- list.files(path = paste(FolderStart, sep=""), all.files=FALSE, full.names=TRUE, 
	recursive=FALSE, pattern=".dat")
	Files <- Files[which(file.info(Files)$size>0)]
	if (length(Files)==0)
	{
		print("No files with data! Function stops.")
		stop()
	}

	# Construct header of output file:	
	Header <- c("Frequency DateTime IntervalNumber Pmin Pmax PathLength XStart YStart XEnd YEnd ID")

	for (FileNr in FileNrStartPreproc:FileNrEndPreproc)
	{

		# Construct output file name:
		Filename <- paste(FolderPreprocessed,substr(Files[FileNr],(nchar(FolderStart)+1),
		nchar(Files[FileNr])),sep="")
		# Remove output file if it already exists:
		unlink(Filename)
	
		# Write header of output file to output file:
		writeLines(Header, Filename)

		# Date of present day:
		DateStr <- substr(Files[FileNr],(nchar(FolderStart)+10),(nchar(Files[FileNr])-4))
  
		# Load link data from present day:
		DataToday <- try(read.table(Files[FileNr],sep=" ",header=TRUE),silent=TRUE)
		# Load link data from previous day:
		Temp <- strptime(DateStr,"%Y%m%d",tz=TimeZone) - 86400
		Temp <- as.Date(Temp)
		DateYesterday <- as.numeric(paste(substr(Temp,1,4),substr(Temp,6,7),
		substr(Temp,9,10),sep=""))
		FilenameYesterday <- paste(substr(Files[FileNr],1,(nchar(Files[FileNr])-12)),DateYesterday,
		substr(Files[FileNr],(nchar(Files[FileNr])-3),nchar(Files[FileNr])),sep="")
		DataYesterday <- try(read.table(FilenameYesterday,sep=" ",header=TRUE),silent=TRUE)

		# Only proceed when file for present and previous day can be opened:
		if (class(DataToday)=='data.frame' & class(DataYesterday)=='data.frame')
		{

			# Determine time interval number:
			StartTimeToday <- strptime(paste(DateStr,PERIOD,sep=""),"%Y%m%d%H%M",tz=TimeZone) - 86400
			TimeStepToday <- as.numeric((strptime(DataToday$DateTime,"%Y%m%d%H%M",tz=TimeZone) -
 			StartTimeToday),units="hours") * NrTimeStepsHour + NrStepsDay
			StartTimeYesterday <- strptime(paste(substr(FilenameYesterday,(nchar(FolderStart)+10),
			(nchar(Files[FileNr])-4)),PERIOD,sep=""),"%Y%m%d%H%M",tz=TimeZone) - 86400
			TimeStepYesterday <- as.numeric((strptime(DataYesterday$DateTime,"%Y%m%d%H%M",tz="UTC") - 
			StartTimeYesterday),units="hours") * NrTimeStepsHour 
			TimeStep <- c(TimeStepYesterday,TimeStepToday)

			# Combine data from previous day and present day:
			Data <- rbind(DataYesterday,DataToday)

			# Only select data within chosen microwave frequencies:
			CondFreq <- Data$Frequency >= MinFrequency & Data$Frequency <= MaxFrequency
			Data <- Data[CondFreq,]
			TimeStep <- TimeStep[CondFreq]

		 	# ID is the link identifier.
			ID <- as.character(Data$ID)
			LinkID <- as.character(unique(DataToday$ID[DataToday$Frequency >= MinFrequency & 
			DataToday$Frequency <= MaxFrequency]))

			if ( length(LinkID)>0 )
			{

				for (i in 1:length(LinkID))
		      		{
	
					# Select data belonging to chosen link identifier (LinkID):            
		    			SelID <- which(ID==LinkID[i])
		    			SelTimeStep <- TimeStep[SelID]


					# CONSISTENCY CHECKS.
					# 1. Remove a time interval if > 1 time interval number, i.e., 
					# remove those intervals for 
					# which more than 1 observation is available. 
					Temp2 <- rle(sort(SelTimeStep))
					Temp <- Temp2$values[which(Temp2$lengths>1)]
					if ( length(Temp)>0 )
					{
						SelTimeStep[which(match(SelTimeStep,Temp)>0)] <- NA
						SelID[is.na(SelTimeStep)] <- NA
						SelTimeStep <- SelTimeStep[!is.na(SelTimeStep)]
						SelID <- SelID[!is.na(SelID)]
					}
	
					# Do not proceed if no link data are available for the selected LinkID.
					if ( length(SelID)==0 | length(SelTimeStep)==0 )
					{
						next
					}
	
	
					# 2. If for a given LinkID frequency, link coordinates, or link length differ during the day: 
					# remove the link for the entire day.
					if ( length(unique(Data$Frequency[SelID])) > 1 | 
					length(unique(Data$PathLength[SelID])) > 1 |
					length(unique(Data$XStart[SelID])) > 1 | 
					length(unique(Data$XEnd[SelID])) > 1 | 
					length(unique(Data$YStart[SelID])) > 1 | 
					length(unique(Data$YEnd[SelID])) > 1 )
					{
						next
					}
	
	
					# Apply selection:
					Dataf <- array(NA,c(NrStepsTwoDays,11))     
	              			Dataf[SelTimeStep,1] <- Data$Frequency[SelID]		  
	               			Dataf[SelTimeStep,2] <- Data$DateTime[SelID]		  
	               			Dataf[SelTimeStep,3] <- TimeStep[SelID]
		       			Dataf[SelTimeStep,4] <- Data$Pmin[SelID]
	               			Dataf[SelTimeStep,5] <- Data$Pmax[SelID]		  
	               			Dataf[SelTimeStep,6] <- Data$PathLength[SelID]		  
		       			Dataf[SelTimeStep,11] <- LinkID[i]
		
	
					# Convert to a Cartesian coordinate system 
					# (easting and northing of start of link, 
					# easting and northing of end of link, respectively; km). 
					Coor <- data.frame(lon=Data$XStart[SelID],lat=Data$YStart[SelID])
					coordinates(Coor) <- c("lon", "lat")
					proj4string(Coor) <- CRS(CoorSystemInputData) 
					CRS.cart <- CRS(projstring)
					Coor.cart <- spTransform(Coor, CRS.cart)
					Dataf[SelTimeStep,7] <- Coor.cart$lon  # Easting (in km)
					Dataf[SelTimeStep,8] <- Coor.cart$lat  # Northing (in km)
	
					Coor <- data.frame(lon=Data$XEnd[SelID],lat=Data$YEnd[SelID])
					coordinates(Coor) <- c("lon", "lat")
					proj4string(Coor) <- CRS(CoorSystemInputData) 
					CRS.cart <- CRS(projstring)
					Coor.cart <- spTransform(Coor, CRS.cart)
					Dataf[SelTimeStep,9] <- Coor.cart$lon  # Easting (in km)
					Dataf[SelTimeStep,10] <- Coor.cart$lat # Northing (in km)
	
	       
					# Write data to file (remove rows with one or more missing values):
		       			write.table(na.omit(Dataf),Filename,append=TRUE,
					row.names=FALSE,col.names=FALSE,quote=FALSE) 	
		
	  			}
	      		}
	   	}

		temp <- read.table(Filename,header=TRUE)
		print(paste("File number ",FileNr,"; Date: ",DateStr,"; Number of links: ",length(unique(temp$ID)),sep=""))
	      
	  	# Print warnings per day:
	   	print(warnings())
	   	# Remove warnings:
	   	assign("last.warning", NULL, envir = baseenv())


	}

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))
}




