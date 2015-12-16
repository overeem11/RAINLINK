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

#' Subfunction which assignes values of rainfall grid to polygons.
#' @description Subfunction which assignes values of rainfall grid to polygons.
#' @param Data microwave link data
#' @return Value
#' @export ToPolygonsRain
#' @examples
#' -
#' 

ToPolygonsRain <- function(Data) 
{
	length_rainvalues = length(Data)
	length_polygons = length_rainvalues*6
	Value = array(NA,c(length_polygons,1))
	for (i in 1:length_rainvalues)
	{
		Value[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = Data[i]		
	}
	Value <- data.frame(Value)
	assign("Value",Value,envir = .GlobalEnv)
}
