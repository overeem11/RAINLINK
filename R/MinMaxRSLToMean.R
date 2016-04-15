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

#' Subfunction for path-averaged rainfall estimation from minimum and maximum attenuations 
#' from microwave links.
#' @description Subfunction for path-averaged rainfall estimation using microwave links. 
#' Compute minimum and maximum attenuation over the link path. Convert these to minimum and 
#' maximum path-averaged rainfall intensities. Convert minimum and maximum path-averaged 
#' rainfall intensities to mean path-averaged rainfall intensities. 
#'
#' Works for a sampling strategy where minimum and maximum received signal powers
#' are provided, and the transmitted power levels are constant.
#' 
#' @param a Coefficients in relationship between rainfall intensity and specific 
#' attenuation (mm h\eqn{^{-1}} dB\eqn{^{-b}} km\eqn{^{b}})
#' @param Aa Wet antenna attenuation correction \eqn{A_{\mbox{a}}} (dB)
#' @param alpha Coefficient (\eqn{\alpha}) determining contribution of minimum and 
#' maximum path-averaged rainfall intensity to mean path-averaged rainfall intensity (-)
#' @param b Exponents in relationship between rainfall intensity and specific attenuation (-)
#' @param PathLength Lengths of link paths (km)
#' @return Data frame with mean path-averaged rainfall intensities (mm h\eqn{^{-1}})
#' @export MinMaxRSLToMeanR
#' @examples
#' MinMaxRSLToMeanR(a=a,Aa=Aa,alpha=alpha,b=b,PathLength=Data$PathLength,
#' PmaxCor=PmaxCor,PminCor=PminCor,Pref=Pref)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R. (2016): Retrieval algorithm for rainfall mapping from
#' microwave links in a cellular communication network, Atmospheric Measurement Techniques, under review.


MinMaxRSLToMeanR <- function(a,Aa,alpha,b,PathLength,PmaxCor,PminCor,Pref)
{

	# Convert received powers to attenuation over the link path
	# Attenuation related to maximum rainfall intensity:
	Amax <- Pref - PminCor

	# Attenuation related to minimum rainfall intensity, link approach:
	Amin <- Pref - PmaxCor
 
   	Rmin <- Rmax <- rep(0, length(Amax))

	CondMax <- which(Amax > Aa)
   	Rmax[CondMax] <- a[CondMax] * ((Amax[CondMax] - Aa) / PathLength[CondMax]) ^ b[CondMax]
	CondMin <- which(Amin > Aa)
   	Rmin[CondMin] <- a[CondMin] * ((Amin[CondMin] - Aa) / PathLength[CondMin]) ^ b[CondMin]
 
   	Rmean <- (1 - alpha) * Rmin + alpha * Rmax
	
	# Rmean to NA if applicable
	Rmean[is.na(Amax)] <- NA

	return(Rmean)

}  
