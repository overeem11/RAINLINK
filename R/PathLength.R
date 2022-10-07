## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links
## in a cellular communication network.
##
## Version 1.3
## Copyright (C) 2022 Aart Overeem
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

#' Function to compute path length of microwave links from coordinates.
#' @description Function for computing path length of microwave links from coordinates.
#'
#' @param InputCoorSystem Define EPSG code for input coordinate system (e.g., 4326L for WGS84 in degrees).
#' @param XStart List with longitude of start of microwave links.
#' @param XEnd List with longitude of end of microwave links.
#' @param YStart List with latitude of start of microwave links.
#' @param YEnd List with latitude of end of microwave links.
#' @return Path length of microwave links (km).
#' @export PathLength
#' @examples PathLength(InputCoorSystem=4326L,XStart=XStart,XEnd=XEnd,YStart=YStart,YEnd=YEnd)
#' @author Aart Overeem
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


PathLength <- function(InputCoorSystem,XStart,XEnd,YStart,YEnd)
{

	# Compute path length from longitude and latitude.
	pntsX <- 
	  data.frame(
	    lon = XStart,
	    lat = YStart)
	pntsY <- 
	  data.frame(
	    lon = XEnd,
	    lat = YEnd)

    	pnts_sfX <- st_as_sf(pntsX, crs = InputCoorSystem, coords = c("lon", "lat"))
    	pnts_sfY <- st_as_sf(pntsY, crs = InputCoorSystem, coords = c("lon", "lat"))    	
        PathLength <- as.numeric(st_distance(pnts_sfX, pnts_sfY,by_element=TRUE))
	# Convert PathLength from m to km.
	PathLength <- PathLength/1000

	# Return PathLength
	return(PathLength)

}




