## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.12
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

#' Interpolation of link-based path-averaged rainfall estimates.
#' @description Interpolation of link-based path-averaged rainfall estimates. 
#' The type of interpolation has to be specified. The following types are 
#' available: 1) Inverse distance weighted interpolation on data (subfunction IDW); 
#' 2) Ordinary kriging with spherical variogram model. Its parameter values nugget, sill, 
#' and range, can be defined by the user; 3) Ordinary kriging with spherical variogram model 
#' with climatological parameter values based on a 30-year rain gauge data set. 
#' These are computed for the day of year as obtained from the file name, thus taking into 
#' account seasonality in spatial rainfall correlation. The subfunction ClimVarParam 
#' computes these parameter values. 
#'
#' Ordinary kriging is performed by subfunction OrdinaryKriging. Note that this interpolation 
#' algorithm is developed for interpolation of link-based rainfall estimates, which are path 
#' averages. The subfunction IntpPathToPoint computes the path-averaged rainfall intensities 
#' for unique link paths. And it assigns path-averaged intensity to the point at the middle of 
#' the link path.
#'
#' The time interval does not have to be an integer but should be equidistant. The minimum time 
#' interval length is automatically computed and is employed as the time 
#' interval length.
#'
#' @param Data Data frame with microwave link data.
#' @param CoorSystemInputData Define coordinate system of input data (in case of
#' WGS84 provide NULL).
#' @param idp The inverse distance weighting power.
#' @param IntpMethod Interpolation method: Ordinary kriging ("OK") or inverse distance weighted 
#' interpolation ("IDW").
#' @param nmax The number of nearest observations that should be used for a kriging prediction 
#' or simulation, where nearest is defined in terms of the space of the spatial locations.
#' @param NUGGET Nugget of spherical variogram model (mm).
#' @param OutputDir If supplied (not NULL), files with resulting interpolated rainfall fields will be 
#' written to this directory.
#' If not supplied, the interpolated fields will be returned.
#' @param RANGE Range of spherical variogram model (km).
#' @param RainGrid Data frame containing information on the points in space where rainfall 
#' needs to be estimated, is assumed to be in the same coordinate system as the original link data.
#' @param Rmean Vector of link-derived rainfall intensities (mm h\eqn{^{-1}}) with length equal to Data.
#' @param SILL Sill of spherical variogram model (mm\eqn{^2}).
#' @param TimeZone Time zone of data (e.g. "UTC").
#' @param Variogram For OK: which variogram to use? Use "ClimvdBeek" for climatological spherical 
#' variogram model.
#' Use "Manual" for spherical variogram model with NUGGET, SILL, and RANGE values supplied as 
#' function arguments.
#' @return Interpolated field of rainfall intensities (mm h\eqn{^{-1}}).
#' @export Interpolation
#' @examples
#' Interpolation(Data=DataPreprocessed,CoorSystemInputData=NULL,idp=2.0,
#' IntpMethod="OK",nmax=50,NUGGET=0.37,RANGE=18.7,RainGrid=RainGrid,
#' Rmean=Rmean,SILL=3.7,TimeZone="UTC",Variogram="ClimVar",OutputDir="RainMapsLinks15min")
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


Interpolation <- function(Data,CoorSystemInputData=NULL,idp=2.0,IntpMethod="OK",nmax=50,NUGGET,RANGE,RainGrid,Rmean,SILL,TimeZone="UTC",Variogram="ClimVar",OutputDir=NULL)
{

	# Determine the middle of the area over which there are data (for reprojection onto a Cartesian coordinate system)
	if (!is.null(CoorSystemInputData))
	{
		Coor <- data.frame(x = c(min(Data$XStart, Data$XEnd), max(Data$XStart, Data$XEnd)), y = c(min(Data$YStart, Data$YEnd), max(Data$YStart, Data$YEnd)))
		coordinates(Coor) <- c("x", "y")
		proj4string(Coor) <- CRS(CoorSystemInputData) 
		CRS.latlon <- CRS("+proj=longlat +ellps=WGS84")
		Coor.latlon <- spTransform(Coor, CRS.latlon)
		XMiddle <- (Coor.latlon$x[1] + Coor.latlon$x[2]) / 2
		YMiddle <- (Coor.latlon$y[1] + Coor.latlon$y[2]) / 2
	} else {
		XMiddle <- (min(Data$XStart, Data$XEnd) + max(Data$XStart, Data$XEnd)) / 2
		YMiddle <- (min(Data$YStart, Data$YEnd) + max(Data$YStart, Data$YEnd)) / 2
		CoorSystemInputData <- "+proj=longlat +ellps=WGS84"
	}
 

	# Construct projection string for Azimuthal Equidistant Cartesian coordinate system:
	projstring <- paste("+proj=aeqd +a=6378.137 +b=6356.752 +R_A +lat_0=",YMiddle,
	" +lon_0=",XMiddle," +x_0=0 +y_0=0",sep="")


	# Convert to a Cartesian coordinate system 
	# (easting and northing of grid; m) of start of link, easting and northing of end of link, 
	# respectively; km). 
	d <- data.frame(x = RainGrid$X, y = RainGrid$Y)
	coordinates(d) <- c("x", "y")
	proj4string(d) <- CRS(CoorSystemInputData)
	CRS.cart <- CRS(projstring)
	Coor.cart <- spTransform(d, CRS.cart)
	#Coor.cart$lon  Easting (in km)
	#Coor.cart$lat  Northing (in km)
	rain.grid <- data.frame(cbind(Coor.cart$x, Coor.cart$y))
	coordinates(rain.grid) <- as.data.frame(rain.grid[,])


	Data$ID <- as.character(Data$ID)
   	IDLink <- unique(Data$ID)
   	N_links <- length(IDLink)
   	XStart <- rep(NA, length(Data$ID))
	YStart <- rep(NA, length(Data$ID))
	XEnd <- rep(NA, length(Data$ID))
	YEnd <- rep(NA, length(Data$ID))
	# Loop over all links for coordinate transformation and putting data in an array
   	for (p in 1 : N_links)
   	{
		# Find indices corresponding to this link
		Cond <- which(Data$ID == IDLink[p])
		
		#Convert coordinates to a system in km, centered on the area covered by the links
		Coor <- data.frame(x = c(Data$XStart[Cond[1]], Data$XEnd[Cond[1]]), 
		y = c(Data$YStart[Cond[1]], Data$YEnd[Cond[1]]))
		coordinates(Coor) <- c("x", "y")
		proj4string(Coor) <- CRS(CoorSystemInputData) 
		CRS.cart <- CRS(projstring)
		Coor.cart <- spTransform(Coor, CRS.cart)
		XStart[Cond] <- Coor.cart$x[1]  # Easting (in km)
		YStart[Cond] <- Coor.cart$y[1]  # Northing (in km)
		XEnd[Cond] <- Coor.cart$x[2]  # Easting (in km)
		YEnd[Cond] <- Coor.cart$y[2]  # Northing (in km)
	}
	

	t <- sort(unique(Data$DateTime))
	N_t <- length(t)
	
	# Make numeric representation of time in seconds from an arbitrary origin
	t_sec <- as.numeric(as.POSIXct(as.character(t), format = "%Y%m%d%H%M"))
	
	# Determine time step length (in seconds)
	dt <- min(diff(t_sec))

	# Check if output fields should be returned or written to files
	if (is.null(OutputDir))
	{
		# Initialize array for output fields
		RainFields <- array(NA, c(N_t, length(rain.grid[, 1])))
	} else {
		# Construct header of output file:
		Header <- c("RainIntensity")
		# Create directory for output files:
		if(!dir.exists(OutputDir)){ dir.create(OutputDir) }
	}
	
	for (i in 1 : N_t)
	{
		Cond_t <- which(Data$DateTime == t[i])
		Cond <- Cond_t[!is.na(Rmean[Cond_t])]
		if (length(Cond) == 0)
		{
			next
		}
		# Compute path-averaged rainfall intensity for unique link paths.
		# Assign path-averaged intensity to point at middle of link path.
		Rainlink <- IntpPathToPoint(Data$ID[Cond], Rmean[Cond], XEnd[Cond], XStart[Cond], YEnd[Cond], YStart[Cond])


		if (IntpMethod!="OK"&IntpMethod!="IDW")
		{

			print("No interpolation method specified! Function stops.")
			stop()			

		}

		# Ordinary kriging interpolation:
		if (IntpMethod=="OK")
		{	
			# Obtain values of sill, range, and nugget of spherical variogram model.
			# This is based on a climatological variogram based on long historical 
			# automatic rain gauge data sets from The Netherlands.
			if (Variogram=="ClimVar")
			{

				ParamVarModel <- ClimVarParam(DateStr = as.character(t[i]), TimeScaleHours = dt / 3600, TimeZone = TimeZone)
				attach(ParamVarModel, warn.conflicts = FALSE)
				Method <- "# Ordinary kriging interpolation. Climatological sill, range, and nugget."

			}

			# Use supplied values of sill, range, and nugget of spherical variogram model 
			if (Variogram=="Manual")
			{

				Sill <- SILL
				Range <- RANGE
				Nugget <- NUGGET
				Method <- "# Ordinary kriging interpolation. Predefined sill, range, and nugget."

			}

			if (Variogram!="ClimVar"&Variogram!="Manual")
			{

				print("Values of sill, range, and nugget not specified! Function stops.")
				stop()			

			}

			InterpField <- OrdinaryKriging(nmax=nmax,Nugget=Nugget,rain.grid=rain.grid,Rainlink=Rainlink,Range=Range,Sill=Sill)
		}


		# Inverse distance weighted interpolation using real link data:
		if (IntpMethod=="IDW")
		{	

			InterpField <-  IDW(idp=idp,rain.grid=rain.grid,Rainlink=Rainlink)
			Method <- "# Inverse distance weighted interpolation"

		}

		# Write to file if desired, otherwise fill output array
		if (is.null(OutputDir))
		{
			RainFields[i, ] <- InterpField
		} else {
			# Constructing output filename:
			Filename <- paste(OutputDir, "/linkmap_", t[i], ".dat", sep = "")
			# Write header of output file to output file:
			writeLines(Header, Filename)
			# Write name of method to output file:
			write.table(Method,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
			# Write data to output file:
			write.table(InterpField,Filename,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
		}
	}   	
	# Return rain fields if desired, otherwise 0 on success
	if (is.null(OutputDir))
	{
		return(RainFields)
	} else {
		return(0)
	}

}
