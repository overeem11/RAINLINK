## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.2
## Copyright (C) 2020 Aart Overeem
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

#' Function for finding (interpolated) rainfall value for a given latitude and longitude.
#' @description  Function for finding (interpolated) rainfall value for a given latitude and longitude. 
#' I.e. find the grid cell which belongs to the location for which latitude and longitude are provided.
#'
#' @param CoorSystemInputData Coordinate system of the input data (e.g. "+init=epsg:4326" for WGS84 in degrees).
#' @param dataf Data frame of (interpolated) rainfall values.
#' @param FileGrid File with interpolation grid in same coordinate system as CoorSystemInputData.
#' @param Lat Latitude of location for which (interpolated) rainfall value is to be extracted 
#' (in coordinate system CoorSystemInputData).
#' @param Lot Longitude of location for which (interpolated) rainfall value is to be extracted 
#' (in coordinate system CoorSystemInputData).
#' @param XMiddle The longitude of the centre of the Azimuthal Equidistant Cartesian coordinate system, 
#' given in the coordinate system of the input data.
#' @param YMiddle The latitude of the centre of the Azimuthal Equidistant Cartesian coordinate system, 
#' given in the coordinate system of the input data.
#' @return Rainfall value for selected location (in unit of provided input rainfall data).
#' @export ReadRainLocation
#' @examples
#' ReadRainLocation(CoorSystemInputData=CoorSystemInputData,dataf=dataf,FileGrid=FileGrid,
#' Lat=Lat,Lon=Lon,XMiddle=XMiddle,YMiddle=YMiddle)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


ReadRainLocation <- function(CoorSystemInputData,dataf,FileGrid,Lat,Lon,XMiddle,YMiddle)
{

	# Construct projection string for Azimuthal Equidistant Cartesian coordinate system:
	projstring <- paste("+proj=aeqd +a=6378.137 +b=6356.752 +R_A +lat_0=",YMiddle," +lon_0=",XMiddle," +x_0=0 +y_0=0",sep="")

	# Read interpolation grid (the coordinates of the middle of the grid cells are provided):
	RainGrid <- read.table(FileGrid,header=TRUE,sep=",")
	# Convert to a Cartesian coordinate system 
	# (easting and northing of grid; m) of start of link, easting and northing of end of link, 
	# respectively; km). 
	d <- data.frame(lon=RainGrid$X,lat=RainGrid$Y)
	coordinates(d) <- c("lon", "lat")
	proj4string(d) <- CRS(CoorSystemInputData)
	CRS.cart <- CRS(projstring)
	Coor.cart <- spTransform(d, CRS.cart)
	#Coor.cart$lon  Easting (in km)
	#Coor.cart$lat  Northing (in km)

	# Convert supplied latitude and longitude to Azimuthal Equidistant Cartesian coordinate system:
	d <- data.frame(lon=Lon,lat=Lat)
	coordinates(d) <- c("lon", "lat")
	proj4string(d) <- CRS(CoorSystemInputData)
	CRS.cartPoint <- CRS(projstring)
	Coor.cartPoint <- spTransform(d, CRS.cartPoint)

	# Determine which grid cell is the closest to the chosen point:
	Dist <- sqrt( (Coor.cart$lon-Coor.cartPoint$lon)^2 + (Coor.cart$lat-Coor.cartPoint$lat)^2 )
	# [1] has been added. This is useful for situations for which the point is exactly between two grid cells.
	# I.e. that would result in two values. To prevent this [1] is used, i.e. the first grid cell is selected.
	GridCellNr <- which(Dist==min(Dist))[1]

	# Determine (interpolated) rainfall value for selected location:
	RainfallPoint <- dataf[GridCellNr]

	return(RainfallPoint)

}



