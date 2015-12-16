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

#' Subfunction for inverse distance weighted interpolation on point data.
#' @description Subfunction for inverse distance weighted interpolation on point data. 
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item idp: The inverse distance weighting power.
#' }
#' @return InterpField
#' @export IDW
#' @examples
#' -
#' 

IDW <- function(rain.grid)
{

	# Interpolate with inverse distance weighting:
	InterpField <- idw(Rainlink$RAIN~1,Rainlink,rain.grid,idp=idp)

	# Negative values (if possible) are set to 0:
	InterpField$var1.pred[InterpField$var1.pred<0] <- 0

	assign("InterpField",InterpField$var1.pred,envir = .GlobalEnv)

}


