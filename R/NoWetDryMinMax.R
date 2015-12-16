## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.## R function 'WetDryLinkApproach.R'.
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


################################################################################
# THIS FUNCTION CALCULATES THE MINIMUM AND MAXIMUM ATTENUATION OVER A LINK PATH#
# NO WET-DRY CLASSIFICATION IS APPLIED            			       #
# RECEIVED POWERS ARE CORRECTED USING THE CALCULATED REFERENCE LEVEL.          #
################################################################################


#' Function for determination of reference signal level, and computing corrected received powers.
#' @description Function for determination of reference signal level (subfunction RefLevelMinMax), and computing corrected received powers, and corrected received powers (this function and subfunction RefLevelMinMax). No wet-dry classification of time intervals is applied. Further, the minimum and maximum attenuation over a link path are computed.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item TIMESTEP: Duration of time interval of sampling strategy (min).
#'   \item FolderPreprocessed: Folder name of input data.
#'   \item FolderCorrected: Folder name of output data.
#'   \item FileRainRetr: Filename with values of coefficients in relationship between specific attenuation and rainfall intensity.
#' }
#' @export NoWetDryMinMax
#' @examples
#' -
#' 

NoWetDryMinMax <- function() 
{

	StartTime <- proc.time()

	MinutesHour <- 60
	MinutesDay <- 1440
	NrStepsDayMinus1 <- MinutesDay/TIMESTEP - 1
	NrStepsDay <- MinutesDay/TIMESTEP
	NrStepsDayPlus1 <- MinutesDay/TIMESTEP + 1
	NrStepsTwoDays <- 2 * NrStepsDay
	NrStepsTwoDaysPlus1 <- NrStepsTwoDays + 1
	NrStepsMaxP <- MinHoursPmin * MinutesHour / TIMESTEP # Number of time steps in MinHoursPmin hours
	NrStepsDryPeriodsRefLevel <- HoursRefLevel * MinutesHour / TIMESTEP  # Number of time steps in HoursRefLevel hours


	# Read file with values of exponents power-law relationship between rainfall intensity and specific attenuation:
	coef_list <- read.table(FileRainRetr)

	# Create directory for output files:
	dir.create(FolderCorrected)


	# Make list of input files: 
	Files <- list.files(path = paste(FolderPreprocessed, sep=""), all.files=FALSE, full.names=TRUE, recursive=FALSE, pattern=".dat")
	Files <- Files[which(file.info(Files)$size>0)]
	if (length(Files)==0)
	{
		print("No files with data! Program stops.")
		stop()
	}

	# Construct header of output file:
	Header <- c(paste("ID a b Amax Amin PathLength IntervalNumber XStart YStart XEnd YEnd Frequency Pmin Pmax ",
	"PminCor PmaxCor RefLevel DateTime",sep=""))


	for (FileNr in FileNrStartWetDry:FileNrEndWetDry)
	{

		# Obtain date:
   		DateStr <- substr(Files[FileNr],(nchar(FolderPreprocessed)+10),(nchar(Files[FileNr])-4))

		# Construct output filename:   
   		Filename <- paste(FolderCorrected,"/linkdata",DateStr,".dat",sep="")    
   		unlink(Filename)
		# Write header of output file to output file:
   		writeLines(Header, Filename)
		# Write name of method to output file:
		Method <- "# No Wet-dry classification."
        	write.table(Method,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)	
	       
   		# Read microwave link data:
   		Data <- read.table(Files[FileNr],header=TRUE)



  		# Make lists with the coordinates of all the links:
		Data$ID <- as.character(Data$ID)
	   	UniqueID <- unique(Data$ID)
	   	lengte_reeks <- length(UniqueID)

	
		if ( lengte_reeks==0 )
		{
			next
		}

	
     
   		for (i in 1:lengte_reeks)
   		{
	
			IDSel <- c(NA)
			IDSel <- UniqueID[i]	
	
  
			# Read data from selected link 
			Frequency <- Date <- Pmin <- Pmax <- PathLength <- XStart <- YStart <- XEnd <- YEnd <- c(NA)    
        		Frequency[1:NrStepsTwoDays] <- Date[1:NrStepsTwoDays]  <- Pmin[1:NrStepsTwoDays] <- Pmax[1:NrStepsTwoDays] <- NA
			PathLength[1:NrStepsTwoDays] <- XStart[1:NrStepsTwoDays] <- YStart[1:NrStepsTwoDays] <- XEnd[1:NrStepsTwoDays] <- 
			YEnd[1:NrStepsTwoDays] <- NA

			# Select data for selected link:
			Cond <- Data$ID==IDSel  
			# Find interval numbers for selected link:
			Selection <- Data$IntervalNumber[Cond]

			# Select data for the selected link and the available interval numbers:
        		Frequency[Selection] <- Data$Frequency[Cond]            
        		Date[Selection] <- Data$DateTime[Cond]	
        		Pmin[Selection] <- Data$Pmin[Cond] 
       			Pmax[Selection] <- Data$Pmax[Cond]         
        		PathLength[Selection] <- Data$PathLength[Cond]  
        		XStart[Selection] <- Data$XStart[Cond] 	
        		YStart[Selection] <- Data$YStart[Cond] 	
        		XEnd[Selection] <- Data$XEnd[Cond] 	
        		YEnd[Selection] <- Data$YEnd[Cond] 

			# Coordinates of selected link
			XStartSel <- YStartSel <- c(NA)
			XStartSel <- unique(XStart)
			YStartSel <- unique(YStart)
			XEndSel <- YendSel <- c(NA)
			XEndSel <- unique(XEnd)
			YEndSel <- unique(YEnd)
			XStartSel <- XStartSel[!is.na(XStartSel)]
			YStartSel <- YStartSel[!is.na(YStartSel)]	
			XEndSel <- XEndSel[!is.na(XEndSel)]	
			YEndSel <- YEndSel[!is.na(YEndSel)]
	
		

        		# Criterion is used that demands that the selected link is at least available during 1 time step of present day. 
			# Only for present day rainfall will be estimated.
			if (length(which(!is.na(Pmin[NrStepsDayPlus1:NrStepsTwoDays]))) > 0)
			{
	   
        	   		Pmin_dry <- c(NA)
        	   		Pmin_dry[1:NrStepsTwoDays] <- 999
		   		PminCor <- Pmin_dry
	      

				# Determine reference level:
				WetDry <- "no"
				RefLevelMinMax(Pmin,PminCor,Pmin_dry,Pmax,NrStepsDay,NrStepsTwoDays,NrStepsDryPeriodsRefLevel,NrStepsDayMinus1,WetDry)


		   		#######################################################################################                   
		   		# CALCULATE MINIMUM AND MAXIMUM ATTENUATION OVER LINK PATH AND WRITE LINK DATA TO FILE#
		   		#######################################################################################
		   		# NB: Only NrStepsDayPlus1:NrStepsTwoDays are suited for computing rainfall intensities.
        	   		# Convert received powers to attenuation over the link path
        	   		# Only proceed if Pmin has a value and only for the present day.
   				a <- b <- c(NA)
		   		FreqSel <- unique(Frequency[!is.na(Frequency)])
       				b <- coef_list[,3][which(abs(coef_list[,1]-FreqSel) == min(abs(coef_list[,1]-FreqSel)))[1]] 
       				a <- coef_list[,2][which(abs(coef_list[,1]-FreqSel) == min(abs(coef_list[,1]-FreqSel)))[1]]  
       				#[1] has been added because it could happen that 2 frequencies from the list are both the closest 
				# to the link frequency.

        	  		Cond <- which(!is.na(Pmin[NrStepsDayPlus1:NrStepsTwoDays])&!is.na(RefLevel[NrStepsDayPlus1:NrStepsTwoDays]))

        	   		if ( length(Cond)>0 )
				{

		       			# Attenuation related to maximum rainfall intensity, link approach:
		       			Amax <- RefLevel[Cond+NrStepsDay] - PminCorRL[Cond+NrStepsDay] 

		       			# Attenuation related to minimum rainfall intensity, link approach:
		       			Amin <- RefLevel[Cond+NrStepsDay] - PmaxCorRL[Cond+NrStepsDay]
		     
		       			data_fit_linkap <- data.frame(cbind(IDSel,a,b,Amax,Amin,PathLength[Cond+NrStepsDay],Cond,XStartSel,YStartSel,
					XEndSel,YEndSel,FreqSel,Pmin[Cond+NrStepsDay],Pmax[Cond+NrStepsDay],PminCorRL[Cond+NrStepsDay],
					PmaxCorRL[Cond+NrStepsDay],RefLevel[Cond+NrStepsDay],Date[Cond+NrStepsDay]))	

					# Write data to output file:
        	       			write.table(data_fit_linkap,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)	
				}
          		

 
      			}      
   		}

		temp <- read.table(Filename,header=TRUE)
		print(paste("File number ",FileNr,"; Date: ",DateStr,"; Number of links: ",
		length(unique(temp$ID)),sep=""))

   		# Print warnings per day:
   		print(warnings())
   		# Remove warnings:
   		assign("last.warning", NULL, envir = baseenv())
	}

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))
}
