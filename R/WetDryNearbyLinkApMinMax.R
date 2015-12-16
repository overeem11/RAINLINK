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

#' Function for wet-dry classification according to nearby link approach, determination of reference signal level, and corrected received powers.
#' @description The received signal powers often decrease during non-rainy periods, resulting in non-zero rainfall estimates, e.g., caused by reflection of the beam or dew formation on the antennas. To prevent this rainfall overestimation a reliable classification of wet and dry periods is needed. This is also beneficial for determining an appropriate reference signal level, representative for dry weather. In order to define wet and dry periods, we assume that rain is correlated in space, and hence that several links in a given area should experience a decrease in received signal level in the case of rain. A time interval is labeled as wet if at least half of the links in the vicinity (for chosen radius) of the selected link experience such a decrease. This so called nearby link approach is applied in this function. It also determines the reference signal level (subfunction RefLevelMinMax), and corrected received powers (this function and subfunction RefLevelMinMax). Further, the minimum and maximum attenuation over a link path are computed. Finally, malfunctioning link antennas can cause outliers in rainfall retrievals (especially for daily accumulations). These outliers can be removed by using a filter that is based on the assumption that rainfall is correlated in space. The value of this outlier filter is calculated, but the filter is not applied in this function.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item TIMESTEP: Duration of time interval of sampling strategy (min).
#'   \item FolderPreprocessed: Folder name of input data.
#'   \item FolderCorrected: Folder name of output data.
#'   \item DistanceLimit: Radius in wet-dry classification (km).
#'   \item MinHoursPmin: Minimum number of hours in the previous 24 hours needed for computing max(P_min) (h).
#'   \item ThresholdNumberLinks: Only use data if number of available links is at least larger than this threshold for present and previous day for those time steps that the selected link is available. Selected link is also counted.
#'   \item ThresholdMedianL: Threshold value (dB km^-1).
#'   \item ThresholdMedian: Threshold value (dB).
#'   \item FileRainRetr: Filename with values of coefficients in relationship between specific attenuation and rainfall intensity.
#' }
#' @export WetDryNearbyLinkApMinMax
#' @examples
#' -
#' 

WetDryNearbyLinkApMinMax <- function() 
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
	Header <- c(paste("ID a b Amax Amin PathLength IntervalNumber XStart YStart XEnd YEnd MeanNrSurLinks F Frequency Pmin Pmax ",
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
		Method <- "# Wet-dry classification with nearby link approach."
		# Write data to output file:
        	write.table(Method,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)	
	       
   		# Read microwave link data:
   		Data <- read.table(Files[FileNr],header=TRUE)



  		# Make lists with the coordinates of all the links:
	   	XStartLink <- YStartLink <- XEndLink <- YEndLink <- LengthLink <- IDLink <- c(NA)
		Data$ID <- as.character(Data$ID)
	   	UniqueID <- unique(Data$ID)
	   	lengte_reeks <- length(UniqueID)
		PminLink <- PminLink_max <- array(NA,c(NrStepsTwoDays,lengte_reeks))

		if ( lengte_reeks>0 )
		{

			# Store data from the surrounding links, as well as data from the selected link, in PminLink.
		   	for (p in 1:lengte_reeks)
		   	{	
				IDLink[p] <- UniqueID[p] 
				Cond <- which(Data$ID==IDLink[p])
				XStartLink[p] <- unique(Data$XStart[Cond])
	        		YStartLink[p] <- unique(Data$YStart[Cond]) 	
	        		XEndLink[p] <- unique(Data$XEnd[Cond]) 	
	        		YEndLink[p] <- unique(Data$YEnd[Cond]) 
				LengthLink[p] <- unique(Data$PathLength[Cond]) 
	
				# Store data from the considered link:
	       			PminLink[Data$IntervalNumber[which(Data$ID==IDLink[p])],p] <- Data$Pmin[which(Data$ID==IDLink[p])]
	
	       			if (length(which(!is.na(PminLink[(1:NrStepsDay),p])) >= NrStepsMaxP))
	     			{		     
	        			# From previous day we just take the maximum value over the whole previous day. 
					# Is only calculated in case of a minimum of MinHoursPmin hours of data for the considered link.
	        			PminLink_max[(1:NrStepsDay),p] <- max(PminLink[(1:NrStepsDay),p],na.rm=T)
	     			}
	     			# For present day maximum values can vary for different time steps.
	     			for (y in 1:NrStepsDay)
	     			{
	        			# For a chosen time step max(P_min) can only be computed in case of a minimum of MinHoursPmin hours of data 
					# in the previous 24 hours:
					if (length(which(!is.na(PminLink[(y:(y+NrStepsDayMinus1)),p])) >= NrStepsMaxP))
					{
			   			PminLink_max[(y+NrStepsDay),p] <- max(PminLink[(y:(y+NrStepsDayMinus1)),p],na.rm=T)
					}
	     			}	
	   		}

		}	



	
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
	
		

			#################################
			# CALCULATIONS FOR LINK APPROACH#
			#################################
	
			# Make a list of links for which the distance to the selected link is smaller than DistanceLimit km.
			q <- 0
			DataTemp <- DataTempDistw <- array(NA,c(lengte_reeks,NrStepsTwoDays))
		   
        		XStart_fixed <- rep(XStartSel,length(XStartLink))
        		XEnd_fixed <- rep(XEndSel,length(XEndLink))
        		YStart_fixed <- rep(YStartSel,length(YStartLink))
        		YEnd_fixed <- rep(YEndSel,length(YEndLink))	   	   
	   
			# Compute distance:
			Distance1 <- sqrt( (XStart_fixed-XStartLink)^2 + (YStart_fixed-YStartLink)^2 )
     			Distance2 <- sqrt( (XEnd_fixed-XStartLink)^2 + (YEnd_fixed-YStartLink)^2 ) 	
			Distance3 <- sqrt( (XStart_fixed-XEndLink)^2 + (YStart_fixed-YEndLink)^2 )
     			Distance4 <- sqrt( (XEnd_fixed-XEndLink)^2 + (YEnd_fixed-YEndLink)^2 ) 
			SelectDist <- which(Distance1 < DistanceLimit & Distance2 < DistanceLimit & Distance3 < DistanceLimit & 
			Distance4 < DistanceLimit )	  
	
			SelectDist <- c(SelectDist,i)
			SelectDist <- unique(SelectDist) 
	

        		Distw <- c(NA)
			Distw[1:NrStepsTwoDays] <- NA 
			for (p in SelectDist)
			{
	      
		     		# p==i: also the selected link itself is taken into account.
	
		     		# If the selected link, i.e. the one for which we want to estimate rainfall, is not available then the data from
				# the surroundings links should not be used. So these are made NA.
        	     		# So only time steps which have data for the selected link do get data.
        	     		PminLink_temp <- PminLink_max_temp <- c(NA)
		     		PminLink_temp[1:NrStepsTwoDays] <- PminLink_max_temp[1:NrStepsTwoDays] <- NA
		     		PminLink_temp[which(!is.na(Pmin))] <- PminLink[which(!is.na(Pmin)),p]
        	     		PminLink_max_temp[which(!is.na(Pmin))] <- PminLink_max[which(!is.na(Pmin)),p]
             
		     		# Only take into account a surrounding link if it has at least data for 1 time step on the present day. 
				# Note that due to the selection just above, this is not necessarily the case.
        	     		# If it is only available on the previous day, such a link could still be useful, because then it helpes to
				# determine whether a time step is wet or dry, and this would be helpful to establish the reference level.
				# Moreover, time step nr NrStepsDay can determine whether time step NrStepsDay + 1 is wet or dry. 
				# Nevertheless, we kept the original selection criterion.
		     		if ( length( which(!is.na(PminLink_temp[NrStepsDayPlus1:NrStepsTwoDays]-PminLink_max_temp[NrStepsDayPlus1:NrStepsTwoDays]))) > 0 )
		     		{
		        		q <- q + 1
		        		DataTemp[q,] <- (PminLink_temp-PminLink_max_temp)
		        		DataTempDistw[q,] <- (PminLink_temp-PminLink_max_temp)/LengthLink[p]
		        		if (p == i)
		        		{
			   			Distw <- DataTempDistw[q,]
		           			Pmin_max <- PminLink_max_temp
        	        		}
		     		}	      
			}
 

	      
			Minval <- 99999
			Cumval <- 0
			NumberSteps <- 0
			NumberLength <- c(NA)
			for (k in 1:NrStepsTwoDays)
			{
		   		NumberLength <- length(which(!is.na(DataTemp[,k]))) 
		   		# If a time step has 0 values, then apparently the selected link is also not available. This is not an error, 
				# but we have to anticipate:
		   		if (NumberLength == 0)
		   		{
		      			next
		   		}
		   		if (NumberLength < Minval)
		   		{
		      			Minval <- NumberLength
		   		}
		   		Cumval <- NumberLength + Cumval	   
		   		NumberSteps <- NumberSteps + 1
			}
			MeanNrSurLinks <- Cumval/NumberSteps
        		# Minval: Minimum number of surroundings links (including the selected link), which is available during the 
			# previous and present day on the time steps that the selected link is available.
	
	
        		# Criterion is used that demands that the selected link is at least available during 1 time step of present day. 
			# Only for present day rainfall will be estimated.
        		# Only use data if at least 4 links are minimal available for present and previous day (including the selected link), 
			# for those time steps that the selected link is available (the selected link is at least available at 1 time step 
			# on previous or present day).
			# Minval!=99999 is an additional check, which should be redundant.
			if (Minval > ThresholdNumberLinks & length(which(!is.na(Pmin[NrStepsDayPlus1:NrStepsTwoDays]))) > 0 & Minval!=99999)
			{


		   		####################################
		   		# PROCESSING DATA FOR LINK APPROACH#
		   		####################################
        	   		# q is here the number of different surrounding links that are used in total. Such a surrounding link is only used 
				# if the selected link has data for the chosen time step.
	   
		   		DataLinks <- DataLinks_distw <- array(NA,c(q,NrStepsTwoDays))
      				DataLinks[1:q,] <- DataTemp[1:q,]
      				DataLinks_distw[1:q,] <- DataTempDistw[1:q,]
	   
		   		# Make table with the average Pmin below the PminLink_max for each time step
		   		Median <- MedianDistw <- c(NA)
		   		Median[1:NrStepsTwoDays] <- MedianDistw[1:NrStepsTwoDays] <- NA
		   		Numbers <- c(NA)
		   		Numbers <- which(Pmin!="NA")
        	   		for (j in Numbers)
		   		{
		      			Median[j] <- median(DataLinks[,j],na.rm=T)
		      			MedianDistw[j] <- median(DataLinks_distw[,j],na.rm=T)	      
		   		}
	   
	   
         	  		######################################################################
	   			# APPLICATION OF LINK APPROACH, CORRECTION OF MINIMUM RECEIVED POWERS#
	           		######################################################################
        	   		Pmin_dry <- c(NA)
        	   		Pmin_dry[1:NrStepsTwoDays] <- 999
		   		PminCor <- c(NA)
		   		PminCor[1:NrStepsTwoDays] <- Pmin_dry 	   
		   		PminCor[which(MedianDistw < ThresholdMedianL & Median < ThresholdMedian )] <- 
		   		Pmin[which(MedianDistw < ThresholdMedianL & Median < ThresholdMedian  )]           
	      
	   
		   		Pmin_temp <- c(NA)
		   		Pmin_temp <- PminCor
	   
		   		Group <- c(NA)
		   		Group <- which(Pmin_temp!=Pmin_dry)-1	      
		   		Group[Group==0] <- NA
		   		Group <- Group[!is.na(Group)]
        	   		Overlap <- c(NA)
		   		Overlap <- Group[match(which(Pmin_temp<(Pmin_max-ThresholdWetDry))-1,Group)]
		   		Overlap <- Overlap[!is.na(Overlap)]
		   		if ( length(Overlap)>0 )
		   		{
		      			PminCor[Overlap] <- Pmin[Overlap]		      
        	   		} 
	   	      
		   		Group <- c(NA)
		   		Group <- which(Pmin_temp!=Pmin_dry)-2	      
		   		Group[Group==0] <- NA
		   		Group[Group==-1] <- NA
		   		Group <- Group[!is.na(Group)]
        	   		Overlap <- c(NA)
		   		Overlap <- Group[match(which(Pmin_temp<(Pmin_max-ThresholdWetDry))-2,Group)]
		   		Overlap <- Overlap[!is.na(Overlap)]	      
		   		if ( length(Overlap)>0 )
		   		{
		      			PminCor[Overlap] <- Pmin[Overlap]		      
        	   		}  	    
	      
		   		Group <- c(NA)
		   		Group <- which(Pmin_temp!=Pmin_dry)+1	      
		   		Group[Group==NrStepsTwoDaysPlus1] <- NA
		   		Group <- Group[!is.na(Group)]
        	   		Overlap <- c(NA)
		   		Overlap <- Group[match(which(Pmin_temp<(Pmin_max-ThresholdWetDry))+1,Group)]
		   		Overlap <- Overlap[!is.na(Overlap)]	      
		   		if ( length(Overlap)>0 )
		   		{
		      			PminCor[Overlap] <- Pmin[Overlap]		      
        	   		}  	      
	      

				# Determine reference level:
				WetDry <- "yes"
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

        	  		Cond <- which(!is.na(Pmin[NrStepsDayPlus1:NrStepsTwoDays])&!is.na(RefLevel[NrStepsDayPlus1:NrStepsTwoDays])&!
				is.na(Distw[NrStepsDayPlus1:NrStepsTwoDays])&!is.na(MedianDistw[NrStepsDayPlus1:NrStepsTwoDays]))

        	   		if ( length(Cond)>0 )
				{

		       			# Attenuation related to maximum rainfall intensity, link approach:
		       			Amax <- RefLevel[Cond+NrStepsDay] - PminCorRL[Cond+NrStepsDay] 

		       			# Attenuation related to minimum rainfall intensity, link approach:
		       			Amin <- RefLevel[Cond+NrStepsDay] - PmaxCorRL[Cond+NrStepsDay]
		     
					F <- c(NA)
					for (s in Cond)
					{	       			
						# Filter. Calculate value of $\Sigma ( \Delta P_L - \mbox{median} ( \Delta P_L) )
        	       				F[s] <- sum(Distw[(s+1):(s+NrStepsDay)] - MedianDistw[(s+1):(s+NrStepsDay)],na.rm=T) * 
						TIMESTEP/MinutesHour
        	       				# Surrounding links exist, selected link has Pmin data for this time step, so Distw and
						# MedianDistw must both be not equal to NA.
					}
					F <- F[!is.na(F)]
	
		       			data_fit_linkap <- data.frame(cbind(IDSel,a,b,Amax,Amin,PathLength[Cond+NrStepsDay],Cond,XStartSel,YStartSel,
					XEndSel,YEndSel,MeanNrSurLinks,F,FreqSel,Pmin[Cond+NrStepsDay],Pmax[Cond+NrStepsDay],PminCorRL[Cond+NrStepsDay],
					PmaxCorRL[Cond+NrStepsDay],RefLevel[Cond+NrStepsDay],Date[Cond+NrStepsDay]))	
	
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
