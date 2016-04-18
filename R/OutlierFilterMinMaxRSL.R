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

#' Function to apply filter to remove outliers in path-averaged microwave link attenuations.
#' @description Function to apply filter to remove outliers in link-based rainfall estimates. 
#' Malfunctioning link antennas can cause outliers in rainfall retrievals (especially for 
#' daily accumulations). These outliers can be removed by using a filter that is based on the 
#' assumption that rainfall is correlated in space. The filter discards a time interval of a 
#' link for which the cumulative difference between its specific attenuation and that of the 
#' surrounding links over the previous 24 h (including the present time interval) becomes 
#' lower than a threshold value in dB h km\eqn{^{-1}}. 
#'
#' The outlier filter has been tested on minimum received signal powers, i.e.
#' for a sampling strategy where minimum and maximum received signal powers
#' are provided, and the transmitted power levels are constant.
#' This function can also be applied for other sampling strategies, because
#' it does not explicitly require minimum and maximum received signal powers.
#' It just applies the selection on all rows in a data frame.
#' Whether the outlier filter will give good results when applied to link data
#' obtained from other sampling strategies would need to be tested.
#' Hence, ''MinMaxRSL'' is kept in this function name to stress that it
#' has been tested for a sampling strategy where minimum and maximum received 
#' powers are provided.
#'
#' Can only be applied when function WetDryNearbyLinkApMinMaxRSL has been executed.
#' 
#' @param Data Data frame with microwave link data
#' @param F Values for filter to remove outliers (dB km\eqn{^{-1}} h)
#' @param FilterThreshold Outlier filter threshold (dB h km\eqn{^{-1}})
#' @return Data frame with microwave link data
#' @export OutlierFilterMinMaxRSL
#' @examples
#' OutlierFilterMinMaxRSL(Data=DataPreprocessed,F=WetDry$F,FilterThreshold=-32.5)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R. (2016): Retrieval algorithm for rainfall mapping from
#' microwave links in a cellular communication network, Atmospheric Measurement Techniques, under review.


OutlierFilterMinMaxRSL <- function(Data,F,FilterThreshold) 
{

	# Set Pmin variable to NA when F exceeds the threshold
	Data$Pmin[F <= FilterThreshold] <- NA
	
	# Return the modified data frame
	return(Data)

}
