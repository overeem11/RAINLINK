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

#' Subfunction for computing path-averaged rainfall intensities for unique link paths. 
#' A path-averaged rainfall intensity is assigned to a point at the middle of 
#' the link path.
#' @description Subfunction for computing path-averaged rainfall intensities for 
#' unique link paths. The link-based, e.g. a 15-minute path-averaged rainfall 
#' accumulation is converted to a path-averaged rainfall intensity, and subsequently 
#' assigned to a point at the middle of the link path. Path-averaged rainfall 
#' intensities are obtained, so data from full-duplex links are averaged.
#'
#' @param ID Link identifier
#' @param Rmean Data frame with mean path-averaged rainfall intensities (mm h\eqn{^{-1}})
#' @param XEnd Easting of end of links (km)
#' @param XStart Easting of start of links (km)
#' @param YEnd Northing of end of links (km)
#' @param YStart Northing of start of links (km)
#' @return Coordinates of links in Azimuthal Equidistant Cartesian coordinate system
#' (latitude, longitude) and rainfall intensity (mm h\eqn{^{-1}}))
#' @export IntpPathToPoint
#' @examples
#' IntpPathToPoint(ID,Rmean,Xend,XStart,YEnd,YStart)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R. (2016): Retrieval algorithm for rainfall mapping from
#' microwave links in a cellular communication network, Atmospheric Measurement Techniques, under review.


IntpPathToPoint <- function(ID,Rmean,XEnd,XStart,YEnd,YStart)
{
	# Determine coordinates of middle of link:
	X_middle <- (XStart + XEnd) / 2
	Y_middle <- (YStart + YEnd) / 2  

	# Determine unique middle of links:
	Coord_dataf <- data.frame(cbind(X_middle, Y_middle))
	Coord_uniq <- unique(Coord_dataf)
	Rainlink <- array(NA, c(length(Coord_uniq[, 1]), 3))

	# Compute average number of unique link paths:
	NrPaths <- (length(Coord_uniq[, 1]))
	
	# Compute average number of unique links (full-duplex links count twice)
	NrLinks <- length(ID)

	# Compute mean path-averaged rainfall intensities for each unique middle of link. 
	# I.e., rainfall intensities from full-duplex links are averaged.
	Rainlink[, 1] <- Coord_uniq[, 1]
	Rainlink[, 2] <- Coord_uniq[, 2]
	for (i in 1 : NrPaths)
	{
		Rainlink[i, 3] <- mean(Rmean[Rainlink[i, 1] == X_middle & Rainlink[i, 2] == Y_middle], na.rm = TRUE)
	}
	
	Rainlink <- as.data.frame(Rainlink)
	colnames(Rainlink) <- c("X", "Y", "RAIN")
	centres <- cbind(Rainlink$X, Rainlink$Y)
	coordinates(Rainlink) <- centres
	
	return(na.omit(Rainlink))
}
