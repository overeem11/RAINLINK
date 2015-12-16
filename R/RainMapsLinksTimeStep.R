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

#' Function which visualises link-based rainfall depths for chosen TIMESTEP in configuration file.
#' @description Function which visualises link-based rainfall depths for chosen TIMESTEP in configuration file. Requires interpolation grid and file with polygons of pixels, the polygons having four angular points. Note that the zoom level for Google Maps can be chosen. This zoom level will not always match with the area to be plotted. Downloading a GoogleMaps background map is very fast, but downloading an OpenStreetMap may require tenths of seconds. Note that such a map is downloaded only once, which saves time in case of multiple rainfall maps. 
#' The following parameters can be changed in the configuration file ``Config.R'':
#' \itemize{
#'   \item TIMESTEP: Duration of time interval of sampling strategy (min).
#' } 
#' @export RainMapsLinksTimeStep
#' @examples
#' RainMapsLinksTimeStep()
#' 


RainMapsLinksTimeStep <- function()
{

	# Create directory for output files:
	dir.create(FolderFigures)

	# Make list of input files: 
	Files <- list.files(path = FolderRainMaps, all.files=FALSE,
	full.names=TRUE, recursive=FALSE, pattern="linkmap")
	Files <- Files[which(file.info(Files)$size>0)]
	if (length(Files)==0)
	{
		print("No files with data! Function stops.")
		stop()
	}

	# Read polygons describing the interpolation/radar grid:
	PolygonsGrid <- read.table(FilePolygonsGrid)


	# Use Google Map as background.
	if (MapBackground=="Google")
	{
	
		if (LocDegSpecified=="yes"&LocNameSpecified=="yes")
		{	
			print("LocDegSpecified and LocNameSpecified cannot be yes both! Function stops.")
			stop()			
		}

		if (LocNameSpecified=="yes")
		{
			map <- get_map(location = LocName, maptype = MapType, source = "google",
			zoom=zoomlevel,color=colortype)
		}

		if (LocDegSpecified=="yes")
		{
			map <- get_map(location = c(LocLon,LocLat), maptype = MapType, source = "google",
			zoom=zoomlevel,color=colortype)
		}

		if (LocDegSpecified!="yes"&LocNameSpecified!="yes")
		{
			# Map is extracted for bounding box determined from interpolation grid:
			bbox <- make_bbox(PolygonsGrid[,1], PolygonsGrid[,2])
			map <- get_map(location = bbox, maptype = MapType, source = "google",color=colortype,
			zoom=zoomlevel) 
		}

		LabelAxisLon <- LabelAxisLonGoogle

	}


	# Use OpenStreetMap as background. # Use predefined scale.
	if (MapBackground=="OSM")
	{
		
		if (BBoxOSMauto!="yes")
		{
			# Determine bounding box from specified coordinates.
			map <- get_openstreetmap(bbox = c(left = left, bottom = bottom, 
			right = right, top = top),format="png",scale=Scale,color=colortype)

		}

		if (BBoxOSMauto=="yes")
		{

			# Determine bounding box determined from interpolation grid:
			bbox <- make_bbox(PolygonsGrid[,1], PolygonsGrid[,2])

			map <- get_openstreetmap(bbox = bbox,format="png",scale=Scale,
			color=colortype)

		}

		LabelAxisLon <- LabelAxisLonOSM

	}



	for (FileNr in FileNrStartRainMaps:FileNrEndRainMaps)
	{

		# Read interpolated link rainfall intensities:
		Data <- read.table(Files[FileNr],header=TRUE)

		# Read file with locations of microwave links. 
		if (PlotLocLinks=="yes")
		{		
			FileLinkLoc <- gsub("map","data",substr(Files[FileNr],(nchar(FolderRainMaps)+2),nchar(Files[FileNr])))
			tempdata <- read.table(paste(FolderRainEstimates,"/",FileLinkLoc,sep=""),header=TRUE)
			d1 <- data.frame(lon=tempdata$XStart,lat=tempdata$YStart)
			coordinates(d1) <- c("lon", "lat")
			proj4string(d1) <- CRS(projstring)
			CRS.cart <- CRS(CoorSystemInputData)
			Coor.cart1 <- spTransform(d1, CRS.cart)
			#Coor.cart$lon  Easting (in km)
			#Coor.cart$lat  Northing (in km)
			d2 <- data.frame(lon=tempdata$XEnd,lat=tempdata$YEnd)
			coordinates(d2) <- c("lon", "lat")
			proj4string(d2) <- CRS(projstring)
			CRS.cart <- CRS(CoorSystemInputData)
			Coor.cart2 <- spTransform(d2, CRS.cart)
			data_coor <<- data.frame(cbind(Coor.cart1$lon,Coor.cart1$lat,Coor.cart2$lon,Coor.cart2$lat))
		}

		IntervalNr <- as.numeric(substr(Files[FileNr],(nchar(FolderRainMaps)+18),(nchar(Files[FileNr])-4)))

		# Convert rainfall intensities to rainfall depths:
		Data$RainDepths <- Data$RainIntensity / ConversionDepthToIntensity

		# End date:
		DateStr <- substr(Files[FileNr],(nchar(FolderRainMaps)+9),(nchar(Files[FileNr])-7))
		
		# Compute time and date of end of observation:
		EndDateTime <- as.character(strptime(paste(DateStr,PERIOD,sep=""),"%Y%m%d%H%M",tz=TimeZone) - 86400 +
		TIMESTEP * 60 * IntervalNr)

		# Compute time and date of start of observation:
		StartDateTime <- as.character(strptime(paste(DateStr,PERIOD,sep=""),"%Y%m%d%H%M",tz=TimeZone) - 86400 +
		(TIMESTEP) * 60 * (IntervalNr-1))

		if ( nchar(EndDateTime)==10)
		{
			EndDateTime <- paste(substr(EndDateTime,1,10)," 00:00:00",sep="")
		}

		if ( nchar(StartDateTime)==10)
		{
			StartDateTime <- paste(substr(StartDateTime,1,10)," 00:00:00",sep="")
		}
		
		ToPolygonsRain(Data$RainDepths)

		DataPolygons <- data.frame(PolygonsGrid,Value)

		# Assign column names:
		names(DataPolygons) <- c("lon","lat","rain")

		if (AutDefineLegendTop=="yes")
		{
			scale_top_timestep <- ceiling(max(DataPolygons$rain,na.rm=T))
			# If all values are 0 mm, the automatic scale would not make sense and contain
			# negative values. A higher value is assigned to scale_top_timestep to prevent this.
			if (scale_top_timestep==0)
			{
				scale_top_timestep <- 10
			}
		}


		if (ManualScale=="no")
		{
			# Determine interval of scale:
			scale_interval <- (scale_top_timestep - scale_bottom_timestep)/colors_number

			# Determine lowest value of scale:
			scale_low <<- seq(from=scale_bottom_timestep,to=colors_number*scale_interval,by=scale_interval) 

			# Determine highest value of scale:
			scale_high <- seq(from=scale_bottom_timestep+scale_interval,to=colors_number*
			scale_interval+scale_bottom_timestep,by=scale_interval)
		}


		# Plot base map for the considered time interval:
		fig <- ggmap(map, extent = "normal", maprange=FALSE) + 
		theme(axis.title.x=element_text(size =rel(5),family="Times")) + 
		xlab(LabelAxisLon) + theme(axis.title.y=element_text(size =rel(5),family="Times")) + 
		ylab("Latitude (º)") + theme(axis.text = element_text(size=rel(4),family="Times")) + 
		theme(axis.ticks = element_line(size = 22)) +  
		theme(plot.title = element_text(family = "Times", face="bold", size=65, vjust=3))
		
		if (PlotLocLinks=="yes")
		{		
			fig <- fig + geom_segment(aes(x=data_coor[,1],y=data_coor[,2],
			xend=data_coor[,3],yend=data_coor[,4]),data=data_coor,alpha=alpha_links_timestep,
			col=ColorLinks,size=SizeLinks)
		}


		for (p in 1:colors_number)
		{ 
			p <<- p
			cond <- which(DataPolygons$rain>=scale_low[p]&DataPolygons$rain<scale_high[p])
			Selected = DataPolygons[cond,]
			Polygons(cond, Selected)
			if (available=="yes")
			{
				fig <- fig + geom_polygon(aes(x = Lon, y = Lat, fill = value), data = dataf, 
				alpha=alpha_polygon, col=PixelBorderCol, size=SizePixelBorder)   
			}
			if (available!="yes")
			{
				# To make sure that the color scale is always plotted:
				polygondata = data.frame(c(999),c(999))
				names(polygondata) = c("polygonlon","polygonlat")		
				fig <- fig + geom_polygon(aes(x = polygonlon, y = polygonlat, fill = scale_low[p]), 
				data = polygondata, alpha=alpha_polygon, col=PixelBorderCol, size=SizePixelBorder)   
			}
		}


		if (plot_below_scale_bottom=="yes")
		{
			cond <- which(DataPolygons$rain<scale_bottom_timestep)
			Selected = DataPolygons[cond,]
			Polygons(cond, Selected)
			if (available=="yes")
			{
				fig <- fig + geom_polygon(aes(x = Lon, y = Lat), data = dataf, 
				alpha=alpha_polygon, col=PixelBorderCol, size=SizePixelBorder, 
				fill = NA)
			}
		}


		if (AutDefineLegendTop!="yes")
		{
			cond <- which(DataPolygons$rain>=scale_high[colors_number])
			Selected = DataPolygons[cond,]
			Polygons(cond, Selected)
			if (available=="yes")
			{
				fig <- fig + geom_polygon(aes(x = Lon, y = Lat), data = dataf, alpha=alpha_polygon,
				col=PixelBorderCol, size=SizePixelBorder, fill = ColourHighestClass)
			}
		}


		label_names <- paste(" ",formatC(scale_low, format="f", digits=1),"-",
		formatC(scale_high, format="f", digits=1),sep="") 
		label_values = c(scale_low)

		pointdata <- data.frame(999,999)
		names(pointdata) = c("pointlon","pointlat")


		# Plot remainder of figure and send to jpeg file:
		DateTime <- paste(StartDateTime," - ",EndDateTime,sep="")
		Title <- paste(paste(TitleLinks,"; ",ExtraText,sep=""),DateTime,sep="\n")
		EndDateTimeFile <- gsub(" ","_",EndDateTime)
		FigFilename <- paste(FolderFigures,"/",FigFileLinksTimeStep,EndDateTimeFile,".jpeg",sep="")
		
		jpeg(FigFilename,width = FigWidth, height = FigHeight) 
		par(family="Times")
		fig_final <- fig + theme(legend.text = element_text(size=rel(5),family="Times")) + 
		theme(legend.title = element_text(size=rel(5),family="Times")) + coord_map(projection="mercator",
	 	xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon), 
		ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) + 
		scale_fill_distiller(palette = palette, breaks=label_values, limits=c(min(scale_low), 
		max(scale_high)), labels=label_names) + ggtitle(Title) +
		guides(fill = guide_legend(override.aes = list(colour = NULL,alpha=alpha_scale),
		reverse = TRUE,keywidth=10,keyheight=10,title=LegendTitleLinksTimeStep)) 
		if (AutDefineLegendTop!="yes")
		{
			fig_final <- fig_final + geom_point(data=pointdata,aes(pointlon,pointlat,color="black"),size=67.5,shape=15,
			alpha=alpha_polygon) + theme(legend.key = element_blank()) + scale_color_manual(values=ColourHighestClass,name="",
			labels=paste("> ",formatC(scale_top_timestep, format="f", digits=1),sep="")) 
		}
		print(fig_final)
		dev.off()

	}

}




