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

#' Interpolation of link-based path-averaged rainfall estimates.
#' @description Interpolation of link-based path-averaged rainfall estimates. The type of interpolation can be specified in the configuration file. The following types are available: 1) Inverse distance weighted interpolation on data (subfunction IDW); 2) Ordinary kriging with spherical variogram model. Its parameter values nugget, sill, and range, can be defined by the user; 3) Ordinary kriging with spherical variogram model with climatological parameter values based on a 30-year rain gauge data set. These are computed for the day of year as obtained from the file name, thus taking into account seasonality in spatial rainfall correlation. The subfunction ClimVarParam computes these parameter values. Ordinary kriging is performed by subfunction OrdinaryKriging. Note that this interpolation algorithm is developed for interpolation of link-based rainfall estimates, which are path averages. The subfunction IntpPathToPoint computes the path-averaged rainfall intensity for unique link paths. And it assigns path-averaged intensity to the point at the middle of the link path.
#' \itemize{
#'   \item FolderRainEstimates: Folder name of input data.
#'   \item FolderRainMaps: Folder name of output data.
#'   \item CoorSystemInputData: Coordinate system of input data (e.g. "+init=epsg:4326"	for WGS84 in degrees).
#'   \item FileGrid: File with interpolation grid.
#'   \item IntpMethod: Ordinary kriging ("OK") or inverse distance weighted interpolation ("IDW").
#'   \item ClimVar: Use "ClimvdBeek" for climatological spherical variogram model. Use "Manual" for spherical variogram model with nugget, sill, and range values manually specified.
#'   \item projstring: Proj4string of an Azimuthal Equidistant Cartesian output coordinate system.
#' }
#' @return InterpField
#' @export Interpolation
#' @examples
#' -
#' 

Interpolation <- function()
{
	StartTime <- proc.time()	 
 
	# Create directory for output files:
	dir.create(FolderRainMaps)

	# Read interpolation grid:
	rain.grid <- read.table(FileGrid,header=TRUE,sep=",")
	# Convert to a Cartesian coordinate system 
	# (easting and northing of grid; m) of start of link, easting and northing of end of link, 
	# respectively; km). 
	d <- data.frame(lon=rain.grid$X,lat=rain.grid$Y)
	coordinates(d) <- c("lon", "lat")
	proj4string(d) <- CRS(CoorSystemInputData)
	CRS.cart <- CRS(projstring)
	Coor.cart <- spTransform(d, CRS.cart)
	#Coor.cart$lon  Easting (in km)
	#Coor.cart$lat  Northing (in km)
	rain.grid <- data.frame(cbind(Coor.cart$lon,Coor.cart$lat))
	coordinates(rain.grid) <- as.data.frame(rain.grid[,])


	# Make list of input files: 
	Files <- list.files(path = FolderRainEstimates, all.files=FALSE,
	full.names=TRUE, recursive=FALSE, pattern="linkdata")
	Files <- Files[which(file.info(Files)$size>0)]
	if (length(Files)==0)
	{
		print("No files with data! Function stops.")
		stop()
	}

	# Construct header of output file:
	Header <- c("RainIntensity")

	NrUniqueLinks <- NrUniqueLinkPaths <- c(NA)
	q <- 0
	for (FileNr in FileNrStartInterp:FileNrEndInterp)
	{

		DateStr <- substr(Files[FileNr],(nchar(FolderRainEstimates)+10),
		(nchar(FolderRainEstimates)+17)) 

		# Read path-averaged link rainfall depths:
		Data <- read.table(Files[FileNr], header=TRUE)   
		# Determine time interval number:
		IntNumber <- unique(Data$IntervalNumber)


		# Compute path-averaged rainfall intensity for unique link paths.
		# Assign path-averaged intensity to point at middle of link path.
		IntpPathToPoint(Data)

		# Average number of unique link paths:
		NrUniqueLinkPaths[FileNr] <- NrPaths
	
		# Average number of unique links (full-duplex links count twice)
		NrUniqueLinks[FileNr] <- NrLinks


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
				ClimVarParam(DateStr)
				Method <- "# Ordinary kriging interpolation. Climatological sill, range, and nugget."
			}

			# Use values of sill, range, and nugget of spherical variogram model 
			# as given in "Config.R".
			if (Variogram=="Manual")
			{
				Nugget <- NUGGET
				Sill <- SILL
				Range <- RANGE
				Method <- "# Ordinary kriging interpolation. Predefined sill, range, and nugget."
			}

			OrdinaryKriging(rain.grid)
		}


		# Inverse distance weighted interpolation using real link data:
		if (IntpMethod=="IDW")
		{	
			IDW(rain.grid)
			Method <- "# Inverse distance weighted interpolation"
		}


		# Constructing output filename:
		IntNumberFile <- IntNumber
		IntNumberFile[IntNumber<10] <- paste("0",as.character(IntNumberFile),sep="")
		Filename <- paste(FolderRainMaps,"/linkmap",DateStr,"_",IntNumberFile,".dat",sep="")
		# Write header of output file to output file:
		writeLines(Header, Filename)
		# Write name of method to output file:
		write.table(Method,Filename,row.names=FALSE,col.names=FALSE,append=TRUE,quote=FALSE)
		# Write data to output file:
		write.table(InterpField,Filename,quote=FALSE,row.names=FALSE,col.names=FALSE,append=TRUE)
     
	}   	

	print("All files")
	print(paste("Average number of unique link paths over those time intervals which have data: ",
	formatC(mean(NrUniqueLinkPaths), format="f", digits=1),sep=""))
	print(paste("Average number of unique links over those time intervals which have data (full-duplex links count twice): ",
	formatC(mean(NrUniqueLinks), format="f", digits=1),sep=""))

	# Print warnings:
	print(warnings())
	# Remove warnings:
	assign("last.warning", NULL, envir = baseenv())

cat(sprintf("Finished. (%.1f seconds)\n",round((proc.time()-StartTime)[3],digits=1)))
}


  


