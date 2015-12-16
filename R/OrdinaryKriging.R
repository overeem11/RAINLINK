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

#' Subfunction for ordinary kriging interpolation of point values using spherical variogram model with predefined parameters sill, range, and nugget.
#' @description Subfunction for ordinary kriging interpolation of point values using spherical variogram model with predefined parameters sill, range, and nugget.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item nmax: For local kriging: the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default, all observations are used.
#' }
#' @param rain.grid rain grid and rainfall intensities 
#' @return InterpField
#' @export OrdinaryKriging
#' @examples
#' -
#' 

OrdinaryKriging <- function(rain.grid)
{

	# Interpolate with ordinary kriging:
	InterpField <- krige(Rainlink$RAIN~1,Rainlink,rain.grid,model=vgm(Sill,"Sph",Range,Nugget),nmax=nmax)

	# Negative values are set to 0:
	InterpField$var1.pred[InterpField$var1.pred<0] <- 0

	assign("InterpField",InterpField$var1.pred,envir = .GlobalEnv)

}
