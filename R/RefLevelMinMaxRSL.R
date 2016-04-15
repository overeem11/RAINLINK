## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.

## 
## Described in paper:
## Aart Overeem, Hidde Leijnse, Remko Uijlenhoet, 2015. Retrieval algorithm for rainfall mapping 
## from microwave links in a cellular communication network. Atmos. Meas. Tech. Discuss., revised version.    

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

#' Function for determination of reference signal level, which is representative of dry weather.
#' @description Function for determination of reference signal level, which is representative 
#' of dry weather.
#'
#' Works for a sampling strategy where minimum and maximum received signal powers
#' are provided, and the transmitted power levels are constant.
#'
#' @param Data Data frame with microwave link data.
#' @param Dry Data frame: Should interval be considered dry for reference level 
#' determination? (0 = wet; 1 = dry)
#' @param HoursRefLevel Minimum number of hours that should be dry in preceding 
#' PeriodHoursRefLevel hours for computing reference level (h)
#' @param PeriodHoursRefLevel Period over which reference level is to be determined
#' (h)
#' @return Reference level (dB)
#' @export RefLevelMinMaxRSL
#' @examples RefLevelMinMaxRSL(Data=DataPreprocessed,Dry=NULL,HoursRefLevel=2.5,
#' PeriodHoursRefLevel=24)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R. (2016): Retrieval algorithm for rainfall mapping from
#' microwave links in a cellular communication network, Atmospheric Measurement Techniques, under review.


RefLevelMinMaxRSL <- function(Data,Dry=NULL,HoursRefLevel=2.5,PeriodHoursRefLevel=24)
{

	# In case no wet-dry classification has been performed:
	if (is.null(Dry))
	{
		Dry <- rep(1, length(Data$DateTime))
	}

	IDLink <- unique(Data$ID)
	N_links <- length(IDLink)
	Data$RefLevel <- rep(NA,length(Data[,1]))
	t <- sort(unique(Data$DateTime))
	N_t <- length(t)
	
	# Make numeric representation of time in seconds from an arbitrary origin
	t_sec <- as.numeric(as.POSIXct(as.character(t), format = "%Y%m%d%H%M"))
	
	# Determine time step length (in seconds)
	dt <- min(diff(t_sec))
	
	#Determine time indices for each entry
	t_ind <- rep(NA, length(Data$DateTime))
	for (i in 1 : N_t)
	{
		ind <- which(Data$DateTime == t[i])
		t_ind[ind] <- i
	}
		
	Pmean = rep(NA, length(Data$DateTime))
	ind_dry = which(Dry == 1)
	Pmean[ind_dry] = (Data$Pmin[ind_dry] + Data$Pmax[ind_dry]) / 2
	
	# Initialize Pref
	Pref <- rep(NA, length(Data$DateTime))
   	for (i in 1 : N_links)
   	{
		Cond <- which(Data$ID == IDLink[i])
		
		ind_PrevPeriod <- 1
		for (j in 2 : N_t)
		{
			ind_PrevPeriod <- min(which(t_sec[ind_PrevPeriod : (j - 1)] > 
			(t_sec[j] - PeriodHoursRefLevel * 3600))) + ind_PrevPeriod - 1
			
			ind_valid <- which(t_ind[Cond] >= ind_PrevPeriod & t_ind[Cond] <= j)
			t_valid <- sum(!is.na(Pmean[Cond[ind_valid]])) * dt
			if (t_valid >= (HoursRefLevel * 3600))
			{
				Pref[Cond[t_ind[Cond] == j]] <- median(Pmean[Cond[ind_valid]], na.rm = TRUE)
			}
		}
	}
	
	# Return Pref
	return(Pref)

}



