## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.2
## Copyright (C) 2020 Aart Overeem
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

#' Function for correcting minimum and maximum received signal powers. 
#' @description Function for correcting minimum (Pmin) and maximum (Pmax) received signal powers.
#' For a rainy time interval the corrected minimum received signal power becomes equal to 
#' the minimum received signal power if this is below the reference signal level (Pref). 
#' Otherwise the corrected minimum received signal power becomes equal to the 
#' reference signal level. The corrected maximum received signal power becomes
#' equal to the maximum received signal power if both the maximum received signal 
#' power and the corrected minimum received signal power are below the reference
#' signal level.
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
#' @param Data Data frame with microwave link data.
#' @param Dry Data frame: Should interval be considered dry for reference level
#' determination? (0 = wet; 1 = dry). Use Dry=NULL if no wet-dry classification
#' has been performed.
#' @param Pref Reference level (dB).
#' @return Data frame with corrected minimum and maximum received powers (dB).
#' @export CorrectMinMaxRSL
#' @examples
#' CorrectMinMaxRSL(Data=DataOutlierFiltered,Dry=WetDry$Dry,Pref=Pref)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


CorrectMinMaxRSL <- function(Data=DataOutlierFiltered,Dry=NULL,Pref=Pref)
{

	# In case no wet-dry classification has been performed:
	if (is.null(Dry))
	{
		Dry <- rep(0, length(Data$DateTime))
	}

	# Compute PminCor, and set all values where Pmin, Dry, or Pref is NA to NA
	PminCor <- Pref
	ind_Pmin <- which(Data$Pmin < Pref & Dry == 0)
	PminCor[ind_Pmin] <- Data$Pmin[ind_Pmin]
	PminCor[is.na(Pref) | is.na(Data$Pmin) | is.na(Dry)] <- NA
	
	# Compute PmaxCor
	PmaxCor <- Pref
	ind_Pmax <- which(PminCor < Pref & Data$Pmax < Pref)
	PmaxCor[ind_Pmax] <- Data$Pmax[ind_Pmax]
	PmaxCor[is.na(PminCor) | is.na(Data$Pmax)] <- NA
	
	# Return data frame with corrected powers
	return_value = data.frame(PminCor = PminCor, PmaxCor = PmaxCor)
	return(return_value)

}
