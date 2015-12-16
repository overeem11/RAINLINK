## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.

## 
## Described in paper:
## Aart Overeem, Hidde Leijnse, Remko Uijlenhoet, 2015. Retrieval algorithm for rainfall mapping 
## from microwave links in a cellular communication network. Atmos. Meas. Tech. Discuss., revised version.    

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

#' Subfunction for determination of reference signal level, which is representative of dry weather, and to obtain corrected received powers.
#' @description Subfunction for determination of reference signal level, which is representative of dry weather, and to obtain corrected received powers.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'    \item HoursRefLevel: Minimum number of hours that should be dry in preceding 24 hours for computing reference level.
#' }
#' @param Pmin minimum received power
#' @param PminCor corrected received power (in case of wet-dry classification corrected for dry periods)
#' @param Pmin_dry minimum received power
#' @param Pmax maximum received power
#' @param NrStepsDay number of time intervals in 24 hours
#' @param NrStepsTwoDays number of time intervals in 48 hours
#' @param NrStepsDryPeriodsRefLevel number of time intervals which falls in minimum number of hours that should be dry in preceding 24 hours for computing reference level
#' @param NrStepsDayMinus1 number of time intervals in 24 ours minus 1
#' @param WetDry is "no" when no wet-dry classification has been applied, is "yes" when a wet-dry classification has been applied.
#' @return RefLevel, PmaxCorRL, PminCorRL
#' @export RefLevelMinMax
#' @examples
#' -
#' 

RefLevelMinMax <- function(Pmin,PminCor,Pmin_dry,Pmax,NrStepsDay,NrStepsTwoDays,NrStepsDryPeriodsRefLevel,NrStepsDayMinus1,WetDry)
{

	##################################################################################################################
	# CALCULATE REFERENCE LEVEL FOR INTERVALS NrStepsDay + 1, NrStepsDay + 2, NrStepsDay + 3, ..., NrStepsTwoDays, AND
	# CORRECT MINIMUM RECEIVED POWERS#
	##################################################################################################################

	if (WetDry=="yes")
	{

	        RefLevel <- c(NA)
		for (q in 1:NrStepsDay)
		{
			# In principal Pmax always exists if Pmin exists and vice versa. Pmin data determine whether a period is wet or dry.
			MeanReflevelTimestep <- (Pmin[q:(q+NrStepsDayMinus1)][PminCor[q:(q+NrStepsDayMinus1)] == 
			Pmin_dry[q:(q+NrStepsDayMinus1)]]+Pmax[q:(q+NrStepsDayMinus1)][PminCor[q:(q+NrStepsDayMinus1)] == 
			Pmin_dry[q:(q+NrStepsDayMinus1)]])/2
	   		# MeanReflevelTimestep can contain one or more NA values. This happens if Pmin is NA for a time step. 
			# If criterion is not met (but Pmin exists), nothing is used in MeanReflevelTimestep of that time step.
			# Then the list MeanReflevelTimestep becomes shorter.
			# The average reference level is computed on the basis of Pmin and Pmax of each time interval (of
			# TIMESTEP min) classified as dry.
			# Subsequently, over a period of 24 hours the median is taken of the average reference signal, 
			# on the basis of those time steps classified as dry.
			if ( length(which(!is.na(MeanReflevelTimestep))) >= NrStepsDryPeriodsRefLevel)
			{
	    			RefLevel[q+NrStepsDay] <- median(MeanReflevelTimestep,na.rm=T)   
			}
			else
		       	{
		        	RefLevel[q+NrStepsDay] <- NA
		       	}
		      	 # If less than HoursRefLevel hours of dry periods: Reference level is not determined.
		}
		PminCor[which(PminCor > RefLevel)] <- RefLevel[which(PminCor > RefLevel)]

		# If RefLevel is NA, then PminCor becomes equal to NA
		PminCor[which(is.na(RefLevel))] <- NA
	
		# Only if RefLevel exists, PminCor may have a value.	   
		Pmin_temp <- c(NA)	   
		Pmin_temp[1:NrStepsTwoDays] <- NA
		Pmin_temp[which(!is.na(Pmin))] <- PminCor[which(!is.na(Pmin))]
		PminCor <- Pmin_temp

		##################################
	 	# CORRECT MAXIMUM RECEIVED POWERS#
		##################################
		PmaxCor <- c(NA)   
		PmaxCor[1:NrStepsTwoDays] <- RefLevel
		Select <- c(NA)
		Select <- which(PminCor<RefLevel)
		# PmaxCor gets original Pmax value only if on the basis of the Pmin signal a period is classified as wet 
		# (when PminCor < RefLevel). In case of no-wet dry classification just the original Pmin value is used.
	 	# Otherwise Pmax is equal to reference level.
		PmaxCor[Select] <- Pmax[Select]	
		# If Pmax is larger than reference level: PmaxCor is set equal to the reference level.
		PmaxCor[which(PmaxCor > RefLevel)] <- RefLevel[which(PmaxCor > RefLevel)]
	

		assign("RefLevel",RefLevel,envir = .GlobalEnv)
		assign("PmaxCorRL",PmaxCor,envir = .GlobalEnv)
		assign("PminCorRL",PminCor,envir = .GlobalEnv)

	}

	   	
	if (WetDry=="no")
	{

	        RefLevel <- c(NA)
		for (q in 1:NrStepsDay)
		{
			# In principal Pmax always exists if Pmin exists and vice versa. 
			MeanReflevelTimestep <- (Pmin[q:(q+NrStepsDayMinus1)]+Pmax[q:(q+NrStepsDayMinus1)])/2
	   		# MeanReflevelTimestep can contain one or more NA values. This happens if Pmin is NA for a time step. 
			# The average reference level is computed on the basis of Pmin and Pmax of each time interval (of
			# TIMESTEP min).
			# Subsequently, over a period of 24 hours the median is taken of the average reference signal. 
			if ( length(which(!is.na(MeanReflevelTimestep))) >= NrStepsDryPeriodsRefLevel)
			{
	    			RefLevel[q+NrStepsDay] <- median(MeanReflevelTimestep,na.rm=T)   
			}
			else
		       	{
		        	RefLevel[q+NrStepsDay] <- NA
		       	}
		      	 # If less than HoursRefLevel hours of dry periods: Reference level is not determined.
		}
		PminCorRL <- Pmin
		PminCorRL[which(Pmin > RefLevel)] <- RefLevel[which(Pmin > RefLevel)]

		# If RefLevel is NA, then PminCorRL becomes equal to NA
		PminCorRL[which(is.na(RefLevel))] <- NA
	
		# Only if RefLevel exists, PminCorRL may have a value.	   
		Pmin_temp <- c(NA)	   
		Pmin_temp[1:NrStepsTwoDays] <- NA
		Pmin_temp[which(!is.na(Pmin))] <- PminCorRL[which(!is.na(Pmin))]
		PminCorRL <- Pmin_temp

		##################################
	 	# CORRECT MAXIMUM RECEIVED POWERS#
		##################################
		PmaxCor <- c(NA)   
		PmaxCor[1:NrStepsTwoDays] <- RefLevel
		Select <- c(NA)
		Select <- which(PminCorRL<RefLevel)
		# PmaxCor gets original Pmax value only if on the basis of the Pmin signal a period is classified as wet 
		# (when PminCorRL < RefLevel). In case of no-wet dry classification just the original Pmin value is used.
	 	# Otherwise Pmax is equal to reference level.
		PmaxCor[Select] <- Pmax[Select]	
		# If Pmax is larger than reference level: PmaxCor is set equal to the reference level.
		PmaxCor[which(PmaxCor > RefLevel)] <- RefLevel[which(PmaxCor > RefLevel)]
	

		assign("RefLevel",RefLevel,envir = .GlobalEnv)
		assign("PmaxCorRL",PmaxCor,envir = .GlobalEnv)
		assign("PminCorRL",PminCorRL,envir = .GlobalEnv)


	}



}



