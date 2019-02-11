## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.12
## Copyright (C) 2019 Aart Overeem
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
#' @description Function for path-averaged rainfall estimation using microwave links. 
#' Maximum and minimum path-averaged rainfall intensites are computed in subfunction 
#' ``MinMaxRSLToMeanR'', where a fixed correction factor is applied to remove wet 
#' antenna attenuation.  
#'
#' Works for a sampling strategy where minimum and maximum received signal powers
#' are provided, and the transmitted power levels are constant.
#'
#' Also works for a sampling strategy where instantaneous transmitted and received signal levels are obtained.
#' In case of instantaneous signal levels, it does not matter whether transmitted power levels vary or are constant.
#' The only requirement is that the input data for RAINLINK needs some preprocessing. See ''ManualRAINLINK.pdf''
#' for instructions. 
#'
#' @param Aa Wet antenna attenuation correction \eqn{A_{\mbox{a}}} (dB).
#' @param alpha Coefficient (\eqn{\alpha}) determining contribution of minimum and 
#' maximum path-averaged rainfall intensity to mean path-averaged rainfall intensity (-).
#' @param Data Data frame with microwave link data.
#' @param kRPowerLawData Values of coefficients a and b employed to convert specific 
#' attenuation to path-averaged rainfall intensity for a range of microwave frequencies.
#' @param PmaxCor Data frame with corrected maximum received powers (dB).
#' @param PminCor Data frame with corrected minimum received powers (dB).
#' @param Pref Reference level (dB).
#' @return Mean path-averaged rainfall intensity (mm h\eqn{^{-1}}).
#' @export RainRetrievalMinMaxRSL
#' @examples
#' RainRetrievalMinMaxRSL(Aa=2.3,alpha=0.33,Data=DataOutlierFiltered,
#' kRPowerLawData=kRPowerLawData,PmaxCor=Pcor$PmaxCor,PminCor=Pcor$PminCor,Pref=Pref)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


RainRetrievalMinMaxRSL <- function(Aa=2.3,alpha=0.33,Data,kRPowerLawData,PmaxCor,PminCor,Pref)
{

		# Find proper values of coefficients in R-k relationship:
		FrequencyLinks <- unique(Data$Frequency)
		a_vec <- approx(x = log(kRPowerLawData$f), y = kRPowerLawData$a, xout = log(FrequencyLinks), method = "linear")$y
		b_vec <- approx(x = log(kRPowerLawData$f), y = kRPowerLawData$b, xout = log(FrequencyLinks), method = "linear")$y
		a <- rep(NA, length(Data$Frequency))
		b <- rep(NA, length(Data$Frequency))
		for (i in 1 : length(FrequencyLinks))
		{
			ind <- which(Data$Frequency == FrequencyLinks[i])
			a[ind] <- a_vec[i]
			b[ind] <- b_vec[i]
		}

		
		# Convert minimum and maximum path-averaged attenuation to mean path-averaged rainfall intensity:
		Rmean <- MinMaxRSLToMeanR(a=a,Aa=Aa,alpha=alpha,b=b,PathLength=Data$PathLength,PmaxCor=PmaxCor,PminCor=PminCor,Pref=Pref)

		return(Rmean)

}



