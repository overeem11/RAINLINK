## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links
## in a cellular communication network.
##
## Version 1.21
## Copyright (C) 2021 Aart Overeem
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
#' Coordinates should be provided as longitudes and latitudes of start and end of a link.

#' It uses the function ''distVincentyEllipsoid'' from the R package ''geosphere'', which computes the ''Vincenty'' (Ellipsoid) Great Circle Distance.
#' The shortest distance between two points (i.e., the ''great-circle-distance'' or ''as the crow flies''), according to the ''Vincenty (ellipsoid)'' method. 
#' The WGS84 ellipsoid is used.
#'
#' @param XStart List with longitude of start of microwave links (decimal degrees)
#' @param XEnd List with longitude of end of microwave links (decimal degrees)
#' @param YStart List with latitude of start of microwave links (decimal degrees)
#' @param YEnd List with latitude of end of microwave links (decimal degrees)
#' @return Path length of microwave links (km).
#' @export PathLength
#' @examples PathLength(XStart=XStart,XEnd=XEnd,YStart=YStart,YEnd=YEnd)
#' @author Aart Overeem
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


PathLength <- function(XStart,XEnd,YStart,YEnd)
{

	# Compute path length from longitude and latitude.
	PathLength <- c(NA)
	for (nr in 1:length(XStart))
	{
		PathLength[nr] <- distVincentyEllipsoid(c(XStart[nr], YStart[nr]), c(XEnd[nr], YEnd[nr]))
	}
	# Convert PathLength from m to km.
	PathLength <- PathLength/1000

	# Return PathLength
	return(PathLength)

}



