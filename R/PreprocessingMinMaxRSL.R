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
#' Also works for a sampling strategy where instantaneous transmitted and received signal levels are obtained.
#' In case of instantaneous signal levels, it does not matter whether transmitted power levels vary or are constant.
#' The only requirement is that the input data for RAINLINK needs some preprocessing. See ''ManualRAINLINK.pdf''
#' for instructions. 
#'
#' Also works for a sampling strategy where average transmitted and received signal levels are obtained.
#' In case of average signal levels, it does not matter whether transmitted power levels vary or are constant.
#' The only requirement is that the input data for RAINLINK needs some preprocessing. See ''ManualRAINLINK.pdf''
#' for instructions. 
#'
#' The input microwave link data do not have to be sorted chronologically.
#'
#' It is strongly advised to use the same unique link identifier (ID) for a link during
#' the entire processed period(s). First of all, time series of sufficient length
#' are needed in order to compute e.g. a reference signal level. Moreover, utilizing
#' the same ID allows for plotting (continuous) time series from the same link.
#'
#' @param Data Data frame with microwave link data (use data(Linkdata) to load example data).
#' @param MaxFrequency Maximum allowed microwave frequency of link in output (GHz; default infinite).
#' @param MinFrequency Minimum allowed microwave frequency of link in output (GHz; default 0).
#' @return Data frame with microwave link data.
#' @export PreprocessingMinMaxRSL
#' @examples
#' data(Linkdata)
#' PreprocessingMinMaxRSL(Data=Linkdata,MaxFrequency=40.5,MinFrequency=12.5)
#' @author Aart Overeem & Hidde Leijnse & Lotte de Vos
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


PreprocessingMinMaxRSL <- function(Data,MaxFrequency=Inf,MinFrequency=0,verbose=TRUE)
{


			# It is required that Data is chronological for the first consistency check:
			Data <- Data[order(Data$DateTime),]	
			
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
					# which more than 1 observation is available from the same link. 
					# To prevent problems when Data$DateTime is read as a factor, rewrite it with as.numeric(as.character(...)) into numerical values.
					# dt does not correspond with a time difference in minutes, as the format is YYYYMMDDHHmm. This is not a problem as dt will only be zero for
					# identical time stamps in all cases.
					dt <- diff(as.numeric(as.character(Data$DateTime[SelID])))
					ind.nodiff <- which(dt == 0)
					if (length(ind.nodiff) > 0)
					{
						# Not SelID should become NA, but the corresponding ID value in Data. This is now correct. 
						# At the end of this function these rows will be excluded.
						Data$ID[ SelID[ind.nodiff] ] <- NA
						Data$ID[ SelID[ind.nodiff + 1] ] <- NA
					}
	
					# Do not proceed if no link data are available for the selected LinkID.
					# length(SelID)==0 is no indication that there are no available link data for the selected LinkID. Use the following line instead.
					if ( length(which(is.na(Data$ID[SelID])==F))==0 )
					{
						next
					}
	
	
					# 2. If for a given LinkID, frequency, link coordinates, or link length differ during the considered period: 
					# remove the link for the entire considered period.
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
						
						# Set ID variable to NA for this link so that it will be removed later:
						Data$ID[SelID] <- NA
					}
	   	}
	}

	# Remove all rows for which one variable is NA (except Polarization), and return resulting data frame:
	rows_with_missing_data <- which(is.na(Data$Frequency) | is.na(Data$DateTime) | is.na(Data$Pmin) | is.na(Data$Pmax) | is.na(Data$PathLength) | 
	is.na(Data$XStart) | is.na(Data$YStart) | is.na(Data$XEnd) | is.na(Data$YEnd) | is.na(Data$ID))

	if(length(rows_with_missing_data)>0){Data <- Data[-rows_with_missing_data,]}
	return(Data)

}


	


						

