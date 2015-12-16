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

#' Subfunction to apply filter to remove outliers in path-averaged microwave link attenuations.
#' @description Subfunction to apply filter to remove outliers in link-based rainfall estimates. Malfunctioning link antennas can cause outliers in rainfall retrievals (especially for daily accumulations). These outliers can be removed by using a filter that is based on the assumption that rainfall is correlated in space. The filter discards a time interval of a link for which the cumulative difference between its specific attenuation and that of the surrounding links over the previous 24 h (including the present time interval) becomes lower than -32.5 dB h per km. Works for sampling strategy where minimum and maximum received powers are provided.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item OUTLIERFILTER: Threshold outlier filter (-32.5 dB h per km).
#' }
#' @param Data the microwave link data
#' @return ID, a, b, Amax, Amin, PathLength, IntervalNumber, XStart, YStart, XEnd, YEnd, Frequency
#' @export OutlierFilterMinMax
#' @examples
#' -
#' 

OutlierFilterMinMax <- function(Data) 
{

	Criterium <- c(NA)
	Criterium <- which(Data$F <= OUTLIERFILTER)
	# Filter (criterion) to remove outliers: 
	# $\Sigma ( \Delta P_L - \mbox{median} ( \Delta P_L) ) <$  -130 dB km$^{-1}$.    
   
	Data$ID[Criterium] <- NA
	Data$a[Criterium] <- NA
	Data$b[Criterium] <- NA
	Data$Amax[Criterium] <- NA
	Data$Amin[Criterium] <- NA
	Data$PathLength[Criterium] <- NA
	Data$IntervalNumber[Criterium] <- NA      
	Data$XStart[Criterium] <- NA
	Data$YStart[Criterium] <- NA
	Data$XEnd[Criterium] <- NA      
	Data$YEnd[Criterium] <- NA  
	Data$Frequency[Criterium] <- NA     

	ID <- a <- b <- Amax <- Amin <- PathLength <- IntervalNumber <- XStart <- YStart <- 
	XEnd <- YEnd <- Frequency <- c(NA)    
	ID <- Data$ID[!is.na(Data$ID)]
	a <- Data$a[!is.na(Data$a)] 
	b <- Data$b[!is.na(Data$b)]
	Amax <- Data$Amax[!is.na(Data$Amax)]    
	Amin <- Data$Amin[!is.na(Data$Amin)]
	PathLength <- Data$PathLength[!is.na(Data$PathLength)]   
	IntervalNumber <- Data$IntervalNumber[!is.na(Data$IntervalNumber)]    
	XStart <- Data$XStart[!is.na(Data$XStart)]    
	YStart <- Data$YStart[!is.na(Data$YStart)]
	XEnd <- Data$XEnd[!is.na(Data$XEnd)]   
	YEnd <- Data$YEnd[!is.na(Data$YEnd)]         
	Frequency <- Data$Frequency[!is.na(Data$Frequency)]  

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

}
