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

#' Subfunction for path-averaged rainfall estimation using microwave links from minimum and maximum attenuations over link path.
#' @description Subfunction for path-averaged rainfall estimation using microwave links. Convert minimum and maximum path-averaged rainfall intensity to mean path-averaged rainfall intensity. Works for sampling strategy where minimum and maximum received powers are provided.
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item Aa: Wet antenna attenuation correction (dB).
#'   \item alpha: Parameter for conversion from minimum and maximum to mean path-averaged rainfall intensity.
#' }
#' @return Rmean
#' @export MinMaxToMean
#' @examples
#' -
#' 

MinMaxToMean <- function()
{
 
   	Rmin <- Rmax <- seq(0, 0, length.out = length(Amax))
   
	CondMax <- {Amax > Aa}
   	Rmax[CondMax] <- a[CondMax]*( (Amax[CondMax]-Aa) /PathLength[CondMax])^b[CondMax]
	cond_min <- {Amin > Aa}
   	Rmin[cond_min] <- a[cond_min]*( (Amin[cond_min]-Aa) /PathLength[cond_min])^b[cond_min]
 
   	Rmean <- c(NA)
   	Rmean <- (1 - alpha) * Rmin + alpha * Rmax   

	assign("Rmean",Rmean,envir = .GlobalEnv)

}  
