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

#' Function for path-averaged rainfall estimation using microwave links.
#' @description Function for path-averaged rainfall estimation using microwave links. Works for sampling strategy where minimum and maximum received powers are provided. Maximum and minimum path-averaged rainfall intensites are computed, where a fixed correction factor is applied to remove wet antenna attenuation (subfunction ``MinMaxToMean''). Subfunction ``OutlierFilter'' can be called to apply an outlier filter.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item TIMESTEP: Duration of time interval of sampling strategy (min).
#'   \item FolderCorrected: Folder name of input data.
#'   \item FolderRainEstimates: Folder name of output data.
#'   \item OutlierFilter: is "no" when no outlier filter is to be applied; is "yes" when an outlier filter is to be applied (will only work when wet-dry classification according to nearby link approach has been performed).
#' }
#' @export RainRetrievalMinMax
#' @examples
#' -
#' 

RainRetrievalMinMax <- function()
{

	StartTime <- proc.time()

	# Construct header of output file:
   	Header <- c(c("ID RainfallDepthPath PathLength XStart YStart XEnd YEnd IntervalNumber Frequency"))

	# Fraction time interval (used to convert from rainfall intensity (mm/h) to rainfall depth (mm)):
	FractionTimeInterval <- TIMESTEP/60

	# Create directory for output files:
	dir.create(FolderRainEstimates)

	MinutesDay <- 1440
	NrStepsDay <- MinutesDay/TIMESTEP

	# Make list of input files: 
	Files <- list.files(path = paste(FolderCorrected, sep=""), all.files=FALSE, full.names=TRUE, 
	recursive=FALSE, pattern="linkdata")
	Files <- Files[which(file.info(Files)$size>0)]
	if (length(Files)==0)
	{
		print("No files with data! Function stops.")
		stop()
	}	

	for (FileNr in FileNrStartRainRetr:FileNrEndRainRetr)
	{
   	
		# Date:
   		DateStr <- substr(Files[FileNr],(nchar(FolderCorrected)+10),(nchar(Files[FileNr])-4))

		# Read link data:
   		Data <- read.table(Files[FileNr],header=TRUE)

		if (OutlierFilter=="yes")
		{
			# Apply filter to remove outliers
			OutlierFilterMinMax(Data)
			Method <- "# Outlier filter."	
		}

		if (OutlierFilter=="no")
		{
			ID <- a <- b <- Amax <- Amin <- PathLength <- IntervalNumber <- XStart <- 
			YStart <- XEnd <- YEnd <- 
			Frequency <- c(NA)    
			ID <- Data$ID
			a <- Data$a
			b <- Data$b
			Amax <- Data$Amax
			Amin <- Data$Amin
			PathLength <- Data$PathLength
			IntervalNumber <- Data$IntervalNumber
			XStart <- Data$XStart
			YStart <- Data$YStart
			XEnd <- Data$XEnd
			YEnd <- Data$YEnd
			Frequency <- Data$Frequency

			assign("ID",ID,envir = .GlobalEnv)
			assign("a",a,envir = .GlobalEnv)
			assign("b",b,envir = .GlobalEnv)
			assign("Amax",Amax,envir = .GlobalEnv)
			assign("Amin",Amin,envir = .GlobalEnv)
			assign("PathLength",PathLength,envir = .GlobalEnv)
			assign("IntervalNumber",IntervalNumber,envir = .GlobalEnv)
			assign("XStart",XStart,envir = .GlobalEnv)
			assign("YStart",YStart,envir = .GlobalEnv)
			assign("XEnd",XEnd,envir = .GlobalEnv)
			assign("YEnd",YEnd,envir = .GlobalEnv)
			assign("Frequency",Frequency,envir = .GlobalEnv)

			Method <- "# No outlier filter."	
		}

		if (OutlierFilter!="yes"&OutlierFilter!="no")
		{
			print("Please specify whether outlier filter should be applied or not!")
			stop()
		}


		# Convert minimum and maximum path-averaged attenuation to mean path-averaged rainfall intensity:
		MinMaxToMean()


		# Write data to file:
		Dataf <- data.frame(cbind(ID,Rmean*FractionTimeInterval,PathLength,XStart,YStart,XEnd,YEnd,
		IntervalNumber,Frequency))
		for (s in 1:NrStepsDay)
		{      
	   		Temp <- c(NA)
	   		Temp <- Dataf[IntervalNumber==s,]
			sFile <- s
			sFile[s<10] <- paste("0",as.character(sFile),sep="")
			Filename <- paste(FolderRainEstimates,substr(Files[FileNr],(nchar(FolderCorrected)+1),
			nchar(Files[FileNr])-4),"_",sFile,".dat",sep="")

			if ( nrow(Temp)>0 )
			{
				# Write header of output file to output file:
		   		writeLines(Header, Filename)
				# Write name of method to output file:
		       		write.table(Method,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
				# Write data to output file:
	   			write.table(Temp,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
			}
		}

		print(paste("File number ",FileNr,"; Date: ",DateStr,"; Number of links: ",
		length(unique(ID)),sep=""))
	      
	  	# Print warnings per day:
	   	print(warnings())
	   	# Remove warnings:
	   	assign("last.warning", NULL, envir = baseenv())

	}

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))	
}



