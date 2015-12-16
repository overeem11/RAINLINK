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

#' Subfunction for obtaining climatological values of sill, range, and nugget of spherical variogram model for RAINLINK.
#' @description Subfunction for obtaining climatological values of sill, range, and nugget of spherical variogram model. This is based on a climatological variogram based on 30-year automatic rain gauge data sets from The Netherlands. Spherical variograms have been modelled as function of the day number and duration in Van de Beek et al. (2012). They use durations of 1 - 24 h. In this function the relationships can be extrapolated to, e.g. 15-min, data. For all input data the middle of the link is used as location of the path-averaged measurement. Input data are link-based, e.g. 15-minute, path-averaged rainfall accumulations. These rainfall accumulations are assumed to be representative for the point at the center of the link. Van de Beek, C. Z., Leijnse, H., Torfs, P. J. J. F., and Uijlenhoet, R.: Seasonal semi-variance of Dutch rainfall at hourly to daily scales, Adv. Water Resour., 45, 76-85, doi:10.1016/j.advwatres.2012.03.023, 2012.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item TIMESTEP: Duration of time interval of sampling strategy (min).
#' }
#' @param DateStr the end date of the chosen daily period.
#' @return Sill, Range and Nugget
#' @export ClimVarParam
#' @examples
#' -
#' 

ClimVarParam <- function(DateStr)
{

	# Compute duration in hours:
	MinutesHour <- 60
	D <- TIMESTEP/MinutesHour
	f <- 1/365


	# Calculate sill, range and nugget of spherical variogram for this particular day:
	t <- strptime(DateStr, "%Y%m%d")$yday+1  # Determines day of year (Julian day number)
	   
	RANGE <- (15.51 * D^0.09 + 2.06 * D^-0.12 * cos(2*pi*f * (t - 7.37 * D^0.22) ) )^4  
	Sill <- (0.84 * D^-0.25 + 0.20 * D^-0.37 * cos(2*pi*f * (t - 162 * D^-0.03) ) )^4

	Nugget <- 0.1 * Sill 
	Range <- RANGE/1000 # range (in kilometers)
   
	assign("Sill",Sill,envir = .GlobalEnv)
	assign("Range",Range,envir = .GlobalEnv)
	assign("Nugget",Nugget,envir = .GlobalEnv)

}



