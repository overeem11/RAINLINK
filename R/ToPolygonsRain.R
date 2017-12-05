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

#' Subfunction which assignes values of rainfall grid to polygons.
#' @description Subfunction which assignes values of rainfall grid to polygons.
#'
#' @param Data Field of rainfall depths at the chosen grid.
#' @return Field of rainfall depths for the polygons at the chosen grid.
#' @export ToPolygonsRain
#' @examples
#' ToPolygonsRain(Data=Data)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


ToPolygonsRain <- function(Data) 
{

	LengthRainvalues = length(Data)
	LengthPolygons = LengthRainvalues*6
	Value = array(NA,c(LengthPolygons,1))
	for (i in 1:LengthRainvalues)
	{
		Value[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = Data[i]		
	}
	Value <- data.frame(Value)

	return(Value)

}
