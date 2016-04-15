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

#' Function for preprocessing of microwave link data.
#' @description Function for preprocessing of microwave link data. This function performs the following tasks:  
#' \enumerate{
#'   \item Link data are selected for microwave frequencies within chosen range.
#'   \item Data selection criteria are applied.
#' }
#'
#' Works for a sampling strategy where minimum and maximum received signal powers
#' are provided, and the transmitted power levels are constant.
#'
#' @param Data Data frame with microwave link data (use data(Linkdata) to load example data)
#' @param MaxFrequency Maximum allowed microwave frequency of link in output (GHz; default infinite)
#' @param MinFrequency Minimum allowed microwave frequency of link in output (GHz; default 0)
#' @return Data frame with microwave link data
#' @export PreprocessingMinMaxRSL
#' @examples
#' data(Linkdata)
#' PreprocessingMinMaxRSL(Data=Linkdata,MaxFrequency=40.5,MinFrequency=12.5)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R. (2016): Retrieval algorithm for rainfall mapping from
#' microwave links in a cellular communication network, Atmospheric Measurement Techniques, under review.


PreprocessingMinMaxRSL <- function(Data,MaxFrequency=Inf,MinFrequency=0,verbose=TRUE)
{

			# Only select data within chosen microwave frequencies:
			CondFreq <- Data$Frequency >= MinFrequency & Data$Frequency <= MaxFrequency
			Data <- Data[CondFreq,]

		 	# ID is the link identifier.
			ID <- as.character(Data$ID)
			LinkID <- as.character(unique(Data$ID))

			if ( length(LinkID)>0 )
			{
				for (i in 1:length(LinkID))
		      		{
					# Select data belonging to chosen link identifier (LinkID):            
		    			SelID <- which(ID==LinkID[i])

					# CONSISTENCY CHECKS.

					# 1. Remove a time interval if > 1 time interval number, i.e., 
					# remove those intervals for 
					# which more than 1 observation is available. 
					dt <- diff(Data$DateTime[SelID])
					ind.nodiff <- which(dt == 0)
					if (length(ind.nodiff) > 0)
					{
						SelID[ind.nodiff] <- NA
						SelID[ind.nodiff + 1] <- NA
						SelID <- SelID[!is.na(SelID)]
					}
	
					# Do not proceed if no link data are available for the selected LinkID.
					if ( length(SelID)==0 )
					{
						next
					}
	
	
					# 2. If for a given LinkID, frequency, link coordinates, or link length differ during the day: 
					# remove the link for the entire day.
					if ( length(unique(Data$Frequency[SelID])) > 1 | 
					length(unique(Data$PathLength[SelID])) > 1 |
					length(unique(Data$XStart[SelID])) > 1 | 
					length(unique(Data$XEnd[SelID])) > 1 | 
					length(unique(Data$YStart[SelID])) > 1 | 
					length(unique(Data$YEnd[SelID])) > 1 )
					{
						if (verbose)
						{
							print(paste("Warning: removing link with ID ", LinkID[i], " because of conflicting link characteristics", sep = ""))
						}
						
						# Set ID variable to NA for this link so that it will be removed later
						Data$ID[SelID] <- NA
					}
	   	}
	}

	# Remove all rows for which one variable is NA, and return resulting data frame
	return(na.omit(Data))

}




