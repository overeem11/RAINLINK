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

#' Subfunction for computing path-averaged rainfall intensity for unique link paths. Assign the path-averaged intensity to point at middle of link path.
#' @description Subfunction for computing path-averaged rainfall intensity for unique link paths. Assign the path-averaged intensity to point at middle of link path.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item ConversionDepthToIntensity: Conversion factor from rainfall depth to intensity.
#' }
#' @param Data the microwave link data
#' @return Rainlink, NrPaths, NrLinks
#' @export IntpPathToPoint
#' @examples
#' -
#' 

IntpPathToPoint <- function(Data)
{

	q <- 0

	# Determine coordinates of middle of link:
	X_middle <- Y_middle <- c(NA)
	X_middle <- (Data$XStart+Data$XEnd)/2
	Y_middle <- (Data$YStart+Data$YEnd)/2  

	# Determine unique middle of links:
	Coord_dataf <- data.frame(cbind(X_middle,Y_middle))
	Coord_uniq <- unique(Coord_dataf)
	Rainlink <- array(NA,c(length(Coord_uniq[,1]),3))

	# Compute average number of unique link paths:
	NrPaths <- (length(Coord_uniq[,1]))
	
	# Compute average number of unique links (full-duplex links count twice)
	NrLinks <- length(Data$ID)

	# Compute mean path-averaged rainfall intensities for each unique middle of link. 
	# I.e., rainfall intensities from full-duplex links are averaged.
	Rainlink[,1] <- Coord_uniq[,1]
	Rainlink[,2] <- Coord_uniq[,2]
	number_linkpaths <- length(Coord_uniq[,1])
	for (i in 1:number_linkpaths)
	{
		q <- q + 1
		Rainlink[i,3] <- mean(Data$RainfallDepthPath[Rainlink[i,1]==X_middle&Rainlink[i,2]==Y_middle]) *
 		ConversionDepthToIntensity
	}
  
	Rainlink <- as.data.frame(Rainlink)
	colnames(Rainlink) <- c("X","Y","RAIN")
	centres <- cbind(Rainlink$X,Rainlink$Y)
	coordinates(Rainlink) <- centres   	

	assign("Rainlink",Rainlink,envir = .GlobalEnv)
	assign("NrPaths",NrPaths,envir = .GlobalEnv)
	assign("NrLinks",NrLinks,envir = .GlobalEnv)

}



  


