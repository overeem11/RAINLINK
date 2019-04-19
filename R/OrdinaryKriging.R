## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.14
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

#' Subfunction for ordinary kriging interpolation of point values using spherical variogram model 
#' with predefined parameters sill, range, and nugget.
#' @description Subfunction for ordinary kriging interpolation of point values using spherical 
#' variogram model with predefined parameters sill, range, and nugget.
#'
#' @param nmax The number of nearest observations that should be used for a kriging prediction 
#' or simulation, where nearest is defined in terms of the space of the spatial locations.
#' @param Nugget Nugget of spherical variogram model (mm).
#' @param rain.grid Interpolation grid in Azimuthal Equidistant Cartesian coordinate system.
#' @param Rainlink Coordinates of links in Azimuthal Equidistant Cartesian coordinate system 
#' and rainfall intensity (latitude in km, longitude in km, intensity in mm h\eqn{^{-1}}).
#' @param Range Range of spherical variogram model (km).
#' @param Sill Sill of spherical variogram model (mm\eqn{^2}).
#' @return Interpolated field of rainfall intensities.
#' @export OrdinaryKriging
#' @examples
#' OrdinaryKriging(nmax=50,Nugget=0.37,rain.grid=rain.grid,Rainlink=Rainlink,
#' Range=18.7,Sill=3.7)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


OrdinaryKriging <- function(nmax,Nugget,rain.grid,Rainlink,Range,Sill)
{

	# Interpolate with ordinary kriging:
	InterpField <- krige(Rainlink$RAIN~1,Rainlink,rain.grid,model=vgm(Sill,"Sph",Range,Nugget),nmax=nmax)

	# Negative values are set to 0:
	InterpField$var1.pred[InterpField$var1.pred<0] <- 0

	return(InterpField$var1.pred)

}
