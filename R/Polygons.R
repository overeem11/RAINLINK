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

#' Subfunction which makes dataframe for polygons with rainfall estimates in specific rainfall class.
#' @description Subfunction which makes dataframe for polygons with rainfall estimates in specific rainfall class.
#' @param cond row numbers of dataframe which fall in specific rainfall class
#' @param Selected coordinates of polygons and their assigned rainfall values
#' @return dataf, available
#' @export Polygons
#' @examples
#' -
#' 

Polygons <- function(cond, Selected) 
{
	length_NA = length(cond)/5
	available = "no"	# default is: no values are available for this class
	if ( length_NA > 0 )
	{
		Lon = Lat = value = array(NA,c(length_NA*6,1))
		for (i in 1:length_NA)
		{
			Lon[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = Selected[((i-1)*5+1):((i-1)*5+5),1]
			Lat[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = Selected[((i-1)*5+1):((i-1)*5+5),2]
			value[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = 
			Selected[((i-1)*5+1):((i-1)*5+5),3]			
		}
		dataf <- data.frame(Lon, Lat,value)
		assign("dataf",dataf,envir = .GlobalEnv)
		available = "yes" 	# when values are available for this class
	}
	assign("available",available,envir = .GlobalEnv)
}
