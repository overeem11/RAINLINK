## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.11
## Copyright (C) 2017 Aart Overeem
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

#' Subfunction for obtaining climatological values of sill, range, and nugget of 
#' spherical variogram model for RAINLINK.
#' @description Subfunction for obtaining climatological values of sill, range, 
#' and nugget of spherical variogram model. This is based on a climatological variogram 
#' based on 30-year automatic rain gauge data sets from The Netherlands. Spherical 
#' variograms have been modelled as function of the day number and duration in 
#' Van de Beek et al. (2012). They use durations of 1 - 24 h. In this function the 
#' relationships can be extrapolated to, e.g. 15-min, data. 
#'
#' @param DateStr The end date of the chosen daily period.
#' @param TimeScaleHours Rainfall aggregation interval in hours.
#' @param TimeZone Time zone of data (e.g. "UTC").
#' @return Data frame with values of sill, range and nugget.
#' @export ClimVarParam
#' @examples
#' ClimVarParam(DateStr="20110911",TimeScaleHours=0.25,TimeZone="UTC")
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.
#'
#'  Van de Beek, C. Z., Leijnse, H., Torfs, P. J. J. F., and Uijlenhoet, R., 2012: Seasonal semi-variance of Dutch 
#' rainfall at hourly to daily scales, Adv. Water Resour., 45, 76-85, doi:10.1016/j.advwatres.2012.03.023.


ClimVarParam <- function(DateStr, TimeScaleHours, TimeZone)
{

	# Set frequency:
	f <- 1/365
	
	# Calculate sill, range and nugget of spherical variogram for this particular day:
	t <- strptime(DateStr, "%Y%m%d",tz=TimeZone)$yday+1  # Determines day of year (Julian day number)
	   
	RANGE <- (15.51 * TimeScaleHours^0.09 + 2.06 * TimeScaleHours^-0.12 * cos(2*pi*f * (t - 7.37 * TimeScaleHours^0.22) ) )^4  
	Sill <- (0.84 * TimeScaleHours^-0.25 + 0.20 * TimeScaleHours^-0.37 * cos(2*pi*f * (t - 162 * TimeScaleHours^-0.03) ) )^4
	
	Nugget <- 0.1 * Sill 
	Range <- RANGE/1000 # range (in kilometers)
	
	dataf <- data.frame(cbind(Sill,Range,Nugget))
	return(dataf)

}
