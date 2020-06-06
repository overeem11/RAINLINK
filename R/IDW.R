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

#' Subfunction for inverse distance weighted interpolation on point data.
#' @description Subfunction for inverse distance weighted interpolation on point data. 
#'
#' @param idp The inverse distance weighting power.
#' @param rain.grid Interpolation grid in Azimuthal Equidistant Cartesian coordinate system.
#' @param Rainlink Coordinates of links in Azimuthal Equidistant Cartesian coordinate system. 
#' and rainfall intensity (latitude in km, longitude in km, intensity in mm h\eqn{^{-1}}).
#' @return Interpolated field of rainfall intensities.
#' @export IDW
#' @examples
#' IDW(idp=idp,rain.grid=rain.grid,Rainlink=Rainlink)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


IDW <- function(idp,rain.grid,Rainlink)
{

	# Interpolate with inverse distance weighting:
	InterpField <- idw(Rainlink$RAIN~1,Rainlink,rain.grid,idp=idp)

	# Negative values (if possible) are set to 0:
	InterpField$var1.pred[InterpField$var1.pred<0] <- 0

	return(InterpField$var1.pred)

}


