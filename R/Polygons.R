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

#' Subfunction which makes dataframe for polygons with rainfall estimates in specific rainfall class.
#' @description Subfunction which makes dataframe for polygons with rainfall estimates in specific rainfall class.
#'
#' @param cond Row numbers of dataframe which fall in specific rainfall class.
#' @param Selected Coordinates of polygons and their assigned rainfall values.
#' @return Data frame
#' @export Polygons
#' @examples
#' RAINLINK::Polygons(cond=cond,Selected=Selected)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


Polygons <- function(cond, Selected) 
{

	LengthNA = length(cond)/5
	dataf <- data.frame(c(-99999999))
	if ( LengthNA > 0 )
	{
		Lon = Lat = value = array(NA,c(LengthNA*6,1))
		for (i in 1:LengthNA)
		{
			Lon[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = Selected[((i-1)*5+1):((i-1)*5+5),1]
			Lat[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = Selected[((i-1)*5+1):((i-1)*5+5),2]
			value[((i-1)*5+1+(i-1)):((i-1)*5+5+(i-1))] = 
			Selected[((i-1)*5+1):((i-1)*5+5),3]			
		}
		dataf <- data.frame(Lon, Lat,value)
	}

	return(dataf)

}
