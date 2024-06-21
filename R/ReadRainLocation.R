## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.31
## Copyright (C) 2024 Aart Overeem
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
#' @param dataf Data frame of (interpolated) rainfall values.
#' @param FileGrid File with interpolation grid in same coordinate system as InputCoorSystem.
#' @param InputCoorSystem Define EPSG code for input coordinate system (e.g., 4326L for WGS84 in degrees).
#' @param Lat Latitude of location for which (interpolated) rainfall value is to be extracted 
#' (in coordinate system InputCoorSystem).
#' @param Lot Longitude of location for which (interpolated) rainfall value is to be extracted 
#' (in coordinate system InputCoorSystem).
#' @return Rainfall value for selected location (in unit of provided input rainfall data).
#' @export ReadRainLocation
#' @examples
#' ReadRainLocation(dataf=dataf,FileGrid=FileGrid,InputCoorSystem=4326L,
#' Lat=Lat,LocalCartesianCoorSystem=28992,Lon=Lon)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


ReadRainLocation <- function(dataf,FileGrid,InputCoorSystem,Lat,LocalCartesianCoorSystem,Lon)
{

	# Read interpolation grid (the coordinates of the middle of the grid cells are provided):
	RainGrid <- read.table(FileGrid,header=TRUE,sep=",")
	# Convert to (local) Cartesian EPSG coordinate system (in kilometers) 
        CoorInput <- data.frame(x = RainGrid$X, y = RainGrid$Y) %>% 
        st_as_sf(coords = 1:2, crs = InputCoorSystem)
        rain.grid <- st_transform(CoorInput, LocalCartesianCoorSystem)
   	Coor.cart  <- as.data.frame(st_coordinates(rain.grid)/1000)  # Easting and northing (in km)
   	colnames(Coor.cart) <- c("lon","lat")

	# Convert supplied latitude and longitude of point location to (local) Cartesian EPSG coordinate system (in kilometers):
	d <- data.frame(lon=Lon,lat=Lat) %>% 
        st_as_sf(coords = 1:2, crs = InputCoorSystem)
	Coor.cartPoint <- st_transform(d, LocalCartesianCoorSystem)
	Coor.cartPoint  <- as.data.frame(st_coordinates(Coor.cartPoint)/1000)
	colnames(Coor.cartPoint) <- c("lon","lat")

	# Determine which grid cell is the closest to the chosen point:
	Dist <- sqrt( (Coor.cart$lon-Coor.cartPoint$lon)^2 + (Coor.cart$lat-Coor.cartPoint$lat)^2 )
	# [1] has been added. This is useful for situations for which the point is exactly between two grid cells.
	# I.e. that would result in two values. To prevent this [1] is used, i.e. the first grid cell is selected.
	GridCellNr <- which(Dist==min(Dist))[1]

	# Determine (interpolated) rainfall value for selected location:
	RainfallPoint <- dataf[GridCellNr]

	return(RainfallPoint)

}



