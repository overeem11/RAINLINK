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
#'
#' Microwave link dataset from which path-averaged rainfall intensities
#' can be computed. Received signal powers were obtained from Nokia microwave 
#' links in one of the national cellular communication networks in The Netherlands,
#' operated by T-Mobile NL. The minimum and maximum received powers over 15-min 
#' intervals were provided, based on 10-Hz sampling. The transmitted power was almost 
#' constant. Here the data have a resolution of 1 dB, and the majority of these Nokia 
#' links used vertically polarised signals. 
#'
#' Data were obtained from September 9, 0800 UTC - September 11, 0800 UTC (2011).
#' The data set contains data from 2612 microwave links.
#'
#' Several functions in the RAINLINK package read a data frame with microwave
#' link data. Such a data frame always contains the variables as indicated
#' below, i.e. the variables in the data set supplied to PreprocessingMinMaxRSL.
#' 
#' For each link and time interval the following variables are provided: 
#' 
#' \itemize{
#'   \item Frequency: microwave frequency f (GHz).
#'   \item DateTime: date and end time of observation (YYYYMMDDhhmm, i.e. year 
#' (2011), month (09), day (11), hour (08), minutes (00): 201109110800).
#'   \item Pmin: minimum received power P\eqn{_{\mbox{min}}} (dBm).
#'   \item Pmax: maximum received power P\eqn{_{\mbox{max}}} (dBm).
#'   \item PathLength: length of microwave link path L (km).
#'   \item XStart: Longitude of start of links (\eqn{^{\circ}}; WGS84).
#'   \item YStart: Latitude of start of links (\eqn{^{\circ}}; WGS84).
#'   \item XEnd: Longitude of end of links (\eqn{^{\circ}}; WGS84).
#'   \item YEnd: Latitude of end of links (\eqn{^{\circ}}; WGS84).
#'   \item ID: Link identifier.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name Linkdata
#' @usage data(Linkdata)
#' @format A data frame with link data from a commercial cellular communication network
NULL





