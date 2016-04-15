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

#' Function which visualises daily radar rainfall depths.
#' @description Function which visualises daily radar rainfall depths. Requires interpolation grid and file 
#' with polygons of pixels. The radar data have been obtained from 
#' http://climate4impact.eu (catalog ``Radar precipitation climatology'') and are freely available.
#'
#' @param AlphaPlotLocation Transparency of plotted symbol for specified location on map
#' @param AlphaPolygon Transparency of polygons
#' @param AlphaScale Transparency of legend
#' @param AutDefineLegendTop Let R automatically define highest value of legend in case of "yes". 
#' Then the highest class, i.e. the one plotted separately above the other classes, is not plotted anymore.
#' @param BBoxOSMauto Compute bounding box from input data or used bounding box defined above? 
#' (for OpenStreetMap only). Use "yes" if bounding box is to be computed from interpolation grid.
#' @param ColoursNumber Number of colour classes in legend
#' @param ColourPlotLocation Colour of plotted symbol for specified location on map
#' @param ColourPlotLocationText Colour of plotted rainfall depth for specified location on map
#' @param ColourType Colour or black-and-white background map? Use "color" for colour and "bw" for 
#' black-and-white background map.
#' @param ColourHighestClass Colour of highest class
#' @param CoorSystemInputData Define coordinate system of input data 
#' (e.g. "+init=epsg:4326" for WGS84 in degrees)
#' @param DateMap End date of daily period for which rainfall map should be plotted
#' @param ExtraDeg To reduce computational time, it is automatically determined which grid 
#' cells fall within the plotted region. To also plot grid cell values which partly fall outside 
#' the plotted region, a positive number for ExtraDeg should be specified (degrees). 
#' This should typically be at least the size of one grid cell in degrees.
#' @param ExtraText Second part of title of plot
#' @param FigFileRadarsDaily Part of figure output file name
#' @param FigHeight Figure height. 1280 times 1280 pixels seems maximum graphical resolution 
#' for downloaded Google Maps. Because also axes and legend are plotted, it is advised to use e.g. 
#' 1450 times 1450 pixels. Then the Google Map will remain approximately 1280 times 1280 pixels. 
#' Using higher values is not a problem (e.g. 2000). In this way it is tried to get the 
#' highest possible resolution. For OpenStreetMap the maps may reach resolutions of 1500 - 2000 pixels. 
#' Hence, using FigWidth and FigHeight of 2000 pixels or higher is advised. The OpenStreetMap itself 
#' is stored in file "ggmapTemp.png". From this file the resolution of the background map can be obtained. 
#' This can be useful for determining an appropriate FigWidth and FigHeight above.
#' @param FigWidth Figure width. 1280 times 1280 pixels seems maximum graphical resolution for downloaded 
#' Google Maps. Because also axes and legend are plotted, it is advised to use e.g. 1450 times 1450 pixels. 
#' Then the Google Map will remain approximately 1280 times 1280 pixels. Using higher values is not a problem 
#' (e.g. 2000). In this way it is tried to get the highest possible resolution. For OpenStreetMap the maps may 
#' reach resolutions of 1500 - 2000 pixels. Hence, using FigWidth and FigHeight of 2000 pixels or higher is 
#' advised. The OpenStreetMap itself is stored in file "ggmapTemp.png". From this file the resolution of the 
#' background map can be obtained. This can be useful for determining an appropriate FigWidth and FigHeight 
#' above.
#' @param FileGrid File with interpolation grid in same coordinate system as CoorSystemInputData
#' @param FileNameRadarDaily Filename of radar file of rainfall depths to be visualised (NetCDF4 format).
#' @param FilePolygonsGrid Name of file with polygons of interpolation grid
#' @param FolderFigures Folder name of figures
#' @param FolderRadarRainMapsDaily Name of folder which contains daily radar rainfall files (input data)
#' @param FontFamily Specify font family of text in figures. To select the default font use "". 
#' Using "Times" may give warnings when executing the visualisation. In that case the font is not installed 
#' on the computer. This can be solved by using the default font ("").
#' @param GoogleLocDegSpecified If GoogleLocDegSpecified is "yes" then the specified location in degrees 
#' is used as the centre of the Google Map. If both GoogleLocNameSpecified and GoogleLocDegSpecified are not 
#' equal to "yes", the bounding box of the map is determined from the provided grid and used as centre of the 
#' Google Map.
#' @param GoogleLocLat Latitude of middle of Google Map (degrees)
#' @param GoogleLocLon Longitude of middle of Google Map (degrees).
#' @param GoogleLocName Location of middle of Google Map, provided as text, e.g. name of city, street name, 
#' country
#' @param GoogleLocNameSpecified If GoogleLocNameSpecified is "yes" then the specified location name GoogleLocName 
#' is used as the centre of the Google Map. If both GoogleLocNameSpecified and GoogleLocDegSpecified are not equal 
#' to "yes", the bounding box of the map is determined from the provided grid and used as centre of the Google Map.
#' @param GoogleMapType In case of Google Maps: which map type should be used? Available map types: "terrain", 
#' "satellite", "roadmap", and "hybrid".
#' @param GoogleZoomlevel Which zoom level to use for the Google Maps?
#' @param LabelAxisLat Label name of vertical axis
#' @param LabelAxisLonGoogle Label name of horizontal axis (for Google Maps only)
#' @param LabelAxisLonOSM Label name of horizontal axis (for OpenStreetMap only)
#' @param LatLocation Latitude of location on map (degrees)
#' @param LatText Latitude of text (rainfall depth) of location on map (degrees)
#' @param LegendTitleRadarsDaily Title of legend
#' @param LonLocation Longitude of location on map (degrees)
#' @param LonText Longitude of text (rainfall depth) of location on map (degrees)
#' @param ManualScale Manually supply the legend breaks if ManuelScale is not equal to "no". 
#' Interval breaks are determined manually from ScaleLow and ScaleHigh. If ManualScale is "no" 
#' interval breaks are determined automatically. 
#' @param MapBackground Google Maps or OpenStreetMap as background? Use "Google" for Google Maps and "OSM" for 
#' OpenStreetMap. Note that Google Maps will only plot on a square figure.
#' @param OSMBottom Latitude in degrees (WGS84) for bottom side of the area for which rainfall depths are to 
#' be plotted (for OpenStreetMap only).
#' @param OSMLeft Longitude in degrees (WGS84) for left side of the area for which rainfall depths are to be 
#' plotted (for OpenStreetMap only). 
#' @param OSMRight Longitude in degrees (WGS84) for right side of the area for which rainfall depths are to 
#' be plotted (for OpenStreetMap only). 
#' @param OSMScale Give value of scale (for OpenStreetMap only). A proper choice of the scale parameter in 
#' get_openstreetmap is difficult. It cannot be computed automatically. Hence, a scale parameter value should 
#' be provided below. The scale parameter should be as small as possible to get the highest graphical resolution. 
#' However, a too low value may result in a map not being downloaded. Hence, the user should 
#' manually supply get_openstreetmap with a scale. It may require some iterations to find the appropriate value for 
#' scale. The file "ggmapTemp.png" is written to disk when an OpenStreetMap is loaded. The highest possible 
#' resolution for a square area is about 2000 x 2000 pixels. 
#' @param OSMTop Latitude in degrees (WGS84) for top side of the area for which rainfall depths are to be plotted 
#' (for OpenStreetMap only)
#' @param Palette Choose (RColorBrewer) palette. "Blues", "YlGnBu", "GnBu": sequential. 
#' "Spectral", "RdYlBu", "BrBG": diverging. Unfortunately, it seems not possible to select 
#' those colours from the full range of the palette. For 6 or less classes we get a reasonable 
#' set of colours though.
#' @param PathRadarRainfallDepth Path in NetCDF4 file with radar data
#' @param PERIOD Select daily time interval, i.e. "0800" implies 0800 UTC previous day - 0800 UTC present day 
#' (use 2400 for 0000 UTC)
#' @param PlotLocation A location is plotted on map if PlotLocation is "yes"
#' @param PixelBorderCol Choose colour of pixel borders. Use NA (without quotes) to not plot pixel borders. 
#' If the pixels are relatively small with respect to the plotted region, the graphical quality of the pixel borders 
#' deteriorates due to low number of pixels (low resolution).
#' @param PlotBelowScaleBottom Plot grid lines for polygons below threshold ScaleBottomTimeStep or ScaleBottomDaily? 
#' If "yes" grid lines are plotted, otherwise they are not plotted.
#' @param ScaleBottomDaily ScaleBottomDaily Lowest class starts at this threshold (minimum rainfall accumulation (mm) 
#' to be plotted). Using a value clearly above 0 mm can save a lot of computation time if the polygons belonging to 
#' values below the threshold are not plotted.
#' @param ScaleHigh ScaleHigh Highest value per class interval, i.e. the highest legend breaks, if these are manually 
#' chosen.  Please note that in case of x values in ColoursNumber, ScaleHigh should also contain x values.
#' @param ScaleLow ScaleLow Lowest value per class interval, i.e. the lowest legend breaks, if these are 
#' manually chosen. Please note that in case of x values in ColoursNumber, ScaleLow should also contain x values.
#' @param ScaleTopDaily Highest colour class ends here (maximum rainfall accumulation (mm) to be plotted). 
#' Sometimes the legend is not correctly plotted. In that case try other values for ScaleTopDaily and/or 
#' ScaleBottomDaily For instance, if the highest class (> x mm) is plotted below instead of above the other 
#' classes. Or if the number of classes does not match the number of chosen classes. Another way to prevent 
#' this is to manually give the legend breaks (ManualScale not equal to "no").
#' @param SizePixelBorder Size of pixel borders
#' @param SizePlotLocation Size of symbol and and accompanied text for specified location on map
#' @param SizePlotTitle Size of plot title
#' @param SymbolPlotLocation Symbol to be plotted for specified location on map
#' @param TitleRadars First part of title of plot
#' @param XMiddle The longitude of the centre of the Azimuthal Equidistant Cartesian coordinate system, 
#' given in the coordinate system of the input data
#' @param YMiddle The latitude of the centre of the Azimuthal Equidistant Cartesian coordinate system, 
#' given in the coordinate system of the input data
#' @export RainMapsRadarsDaily
#' @examples
#' RainMapsRadarsDaily(20110911) # To plot daily radar rainfall map ending at 11 September 2011.
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R. (2016): Retrieval algorithm for rainfall mapping from
#' microwave links in a cellular communication network, Atmospheric Measurement Techniques, under review.


RainMapsRadarsDaily <- function(AlphaPlotLocation,AlphaPolygon,AlphaScale,
AutDefineLegendTop,BBoxOSMauto,ColoursNumber,ColourPlotLocation,
ColourPlotLocationText,ColourType,ColourHighestClass,CoorSystemInputData,
DateMap,ExtraDeg,ExtraText,FigFileRadarsDaily,FigHeight,FigWidth,FileGrid,
FileNameRadarDaily,FilePolygonsGrid,FolderFigures,FolderRadarRainMapsDaily,
FontFamily,GoogleLocDegSpecified,GoogleLocLat,GoogleLocLon,GoogleLocName,
GoogleLocNameSpecified,GoogleMapType,GoogleZoomlevel,LabelAxisLat,
LabelAxisLonGoogle,LabelAxisLonOSM,LatLocation,LatText,LegendTitleRadarsDaily,
LonLocation,LonText,ManualScale,MapBackground,OSMBottom,OSMLeft,OSMRight,
OSMScale,OSMTop,Palette,PathRadarRainfallDepth,PERIOD,PlotLocation,
PixelBorderCol,PlotBelowScaleBottom,ScaleBottomDaily,ScaleHigh,ScaleLow,
ScaleTopDaily,SizePixelBorder,SizePlotLocation,SizePlotTitle,
SymbolPlotLocation,TIMESTEP,TitleRadars,XMiddle,YMiddle)
{
	# Create directory for output files:
	dir.create(FolderFigures)

	# Open daily radar rainfall file:
	RadarFile <- paste(FolderRadarRainMapsDaily,"/",FileNameRadarDaily,sep="")
	
	ncFILE <- nc_open(RadarFile,verbose=F)
	Rday <- c(ncvar_get(ncFILE,varid=PathRadarRainfallDepth))
	Rday <- Rday[!is.na(Rday)]
	nc_close(ncFILE)


	# Read polygons describing the interpolation/radar grid:
	PolygonsGrid <- read.table(FilePolygonsGrid)


	# Use Google Map as background.
	if (MapBackground=="Google")
	{
		if (GoogleLocDegSpecified=="yes"&GoogleLocNameSpecified=="yes")
		{	
			print("GoogleLocDegSpecified and GoogleLocNameSpecified cannot be yes both! Function stops.")
			stop()			
		}
		if (GoogleLocNameSpecified=="yes")
		{
			map <- get_map(location = GoogleLocName, maptype = GoogleMapType, source = "google",
			zoom=GoogleZoomlevel,color=ColourType)
		}
		if (GoogleLocDegSpecified=="yes")
		{
			map <- get_map(location = c(GoogleLocLon,GoogleLocLat), maptype = GoogleMapType, source = "google",
			zoom=GoogleZoomlevel,color=ColourType)
		}
		if (GoogleLocDegSpecified!="yes"&GoogleLocNameSpecified!="yes")
		{
			# Map is extracted for bounding box determined from interpolation grid:
			bbox <- make_bbox(PolygonsGrid[,1], PolygonsGrid[,2])
			map <- get_map(location = bbox, maptype = GoogleMapType, source = "google",color=ColourType,
			zoom=GoogleZoomlevel) 
		}
		LabelAxisLon <- LabelAxisLonGoogle

	}


	# Use OpenStreetMap as background. # Use predefined scale.
	if (MapBackground=="OSM")
	{
		if (BBoxOSMauto!="yes")
		{
			# Determine bounding box from specified coordinates.
			map <- get_openstreetmap(bbox = c(left = OSMLeft, bottom = OSMBottom, 
			right = OSMRight, top = OSMTop),format="png",scale=OSMScale,color=ColourType)
		}
		if (BBoxOSMauto=="yes")
		{
			# Determine bounding box determined from interpolation grid:
			bbox <- make_bbox(PolygonsGrid[,1], PolygonsGrid[,2])

			map <- get_openstreetmap(bbox = bbox,format="png",scale=OSMScale,
			color=ColourType)
		}
		LabelAxisLon <- LabelAxisLonOSM

	}


	# End date:
	DateStr <- DateMap
	
	# Compute time and date of end of observation:
	EndDateTime <- as.character(strptime(paste(DateStr,PERIOD,sep=""),"%Y%m%d%H%M")) 

	# Compute time and date of start of observation:
	StartDateTime <- as.character(strptime(paste(DateStr,PERIOD,sep=""),"%Y%m%d%H%M") - 86400)


	if ( nchar(EndDateTime)==10)
	{
		EndDateTime <- paste(substr(EndDateTime,1,10)," 00:00:00",sep="")
	}
	if ( nchar(StartDateTime)==10)
	{
		StartDateTime <- paste(substr(StartDateTime,1,10)," 00:00:00",sep="")
	}

	
	# In case of zooming the processing time can become quite large. To speed up the process here only 
	# data are selected which are relevant for the plotting region.
	PlotRegion <- attr(map, "bb")
	# Read interpolation grid:
	RainGrid <- read.table(FileGrid,header=TRUE,sep=",")
	CondMap <- which(RainGrid$X>min(PlotRegion$ll.lon-ExtraDeg,PlotRegion$ur.lon-ExtraDeg)&RainGrid$X<
	max(PlotRegion$ll.lon+ExtraDeg,PlotRegion$ur.lon+ExtraDeg)&RainGrid$Y>min(PlotRegion$ll.lat-ExtraDeg,
	PlotRegion$ur.lat-ExtraDeg)&RainGrid$Y<max(PlotRegion$ll.lat+ExtraDeg,PlotRegion$ur.lat+ExtraDeg))
	CondPolygon <- c(NA)
	CondPolygon[(CondMap-1)*6+1] <- (CondMap-1)*6+1
	CondPolygon[(CondMap-1)*6+2] <- (CondMap-1)*6+2
	CondPolygon[(CondMap-1)*6+3] <- (CondMap-1)*6+3
	CondPolygon[(CondMap-1)*6+4] <- (CondMap-1)*6+4
	CondPolygon[(CondMap-1)*6+5] <- (CondMap-1)*6+5
	CondPolygon[(CondMap-1)*6+6] <- (CondMap-1)*6+6

	CondPolygon <- CondPolygon[!is.na(CondPolygon)]

	Value <- ToPolygonsRain(Data=Rday[CondMap])

	DataPolygons <- data.frame(PolygonsGrid[CondPolygon,],Value)

	# Assign column names:
	names(DataPolygons) <- c("lon","lat","rain")

	if (AutDefineLegendTop=="yes")
	{
		ScaleTopDaily <- ceiling(max(DataPolygons$rain,na.rm=T))
		# If all values are 0 mm, the automatic scale would not make sense and contain
		# negative values. A higher value is assigned to ScaleTopDaily to prevent this.
		if (ScaleTopDaily==0)
		{
			ScaleTopDaily <- 10
		}
	}


	if (ManualScale=="no")
	{
		# Determine interval of scale:
		ScaleInterval <- (ScaleTopDaily - ScaleBottomDaily)/ColoursNumber

		# Determine lowest value of scale:
		ScaleLow <- seq(from=ScaleBottomDaily,to=ColoursNumber*ScaleInterval,by=ScaleInterval) 

		# Determine highest value of scale:
		ScaleHigh <- seq(from=ScaleBottomDaily+ScaleInterval,to=ColoursNumber*
		ScaleInterval+ScaleBottomDaily,by=ScaleInterval)
	}
	ScaleLow <<- ScaleLow


	# Plot base map for the considered time interval:
	Fig <- ggmap(map, extent = "normal", maprange=FALSE) + 
	theme(axis.title.x=element_text(size =rel(5),family=FontFamily)) + 
	xlab(LabelAxisLon) + theme(axis.title.y=element_text(size =rel(5),family=FontFamily)) + 
	ylab(LabelAxisLat) + theme(axis.text = element_text(size=rel(4),family=FontFamily)) + 
	theme(axis.ticks = element_line(size = 22)) +  
	theme(plot.title = element_text(family = FontFamily, face="bold", size=SizePlotTitle, vjust=3))
		

	for (p in 1:ColoursNumber)
	{ 
		p <<- p
		cond <- which(DataPolygons$rain>=ScaleLow[p]&DataPolygons$rain<ScaleHigh[p])
		Selected = DataPolygons[cond,]
		dataf <- Polygons(cond=cond, Selected=Selected)
		if (dataf[1,1]!=-99999999)
		{
			Fig <- Fig + geom_polygon(aes(x = Lon, y = Lat, fill = value), data = dataf, 
			alpha=AlphaPolygon, col=PixelBorderCol, size=SizePixelBorder)   
		}
		if (dataf[1,1]==-99999999)
		{
			# To make sure that the colour scale is always plotted:
			polygondata = data.frame(c(999),c(999))
			names(polygondata) = c("polygonlon","polygonlat")		
			Fig <- Fig + geom_polygon(aes(x = polygonlon, y = polygonlat, fill = ScaleLow[p]), 
			data = polygondata, alpha=AlphaPolygon, col=PixelBorderCol, size=SizePixelBorder)   
		}
	}


	if (PlotBelowScaleBottom=="yes")
	{
		cond <- which(DataPolygons$rain<ScaleBottomDaily)
		Selected = DataPolygons[cond,]
		dataf <- Polygons(cond=cond, Selected=Selected)
		if (dataf[1,1]!=-99999999)
		{
			Fig <- Fig + geom_polygon(aes(x = Lon, y = Lat), data = dataf, 
			alpha=AlphaPolygon, col=PixelBorderCol, size=SizePixelBorder, 
			fill = NA)
		}
	}


	if (AutDefineLegendTop!="yes")
	{
		cond <- which(DataPolygons$rain>=ScaleHigh[ColoursNumber])
		Selected = DataPolygons[cond,]
		dataf <- Polygons(cond=cond, Selected=Selected)
		if (dataf[1,1]!=-99999999)
		{
			Fig <- Fig + geom_polygon(aes(x = Lon, y = Lat), data = dataf, alpha=AlphaPolygon,
			col=PixelBorderCol, size=SizePixelBorder, fill = ColourHighestClass)
		}
	}


	LabelNames <- paste(" ",formatC(ScaleLow, format="f", digits=1),"-",
	formatC(ScaleHigh, format="f", digits=1),sep="") 
	LabelValues = c(ScaleLow)

	pointdata <- data.frame(999,999)
	names(pointdata) = c("pointlon","pointlat")


	# Plot remainder of figure and send to jpeg file:
	EndDateTime <- substr(EndDateTime,1,16)
	EndDateTime <- gsub(" ","",EndDateTime)
	EndDateTime <- gsub(":","",EndDateTime)
	EndDateTime <- gsub("-","",EndDateTime)
	StartDateTime <- substr(StartDateTime,1,16)
	StartDateTime <- gsub(" ","",StartDateTime)
	StartDateTime <- gsub(":","",StartDateTime)
	StartDateTime <- gsub("-","",StartDateTime)
	DateTime <- paste(StartDateTime," - ",EndDateTime,sep="")
	Title <- paste(paste(TitleRadars,"; ",ExtraText,sep=""),DateTime,sep="\n")
	FigFilename <- paste(FolderFigures,"/",FigFileRadarsDaily,EndDateTime,".jpeg",sep="")
	
	jpeg(FigFilename,width = FigWidth, height = FigHeight) 
	par(family=FontFamily)
	FigFinal <- Fig + theme(legend.text = element_text(size=rel(5),family=FontFamily)) + 
	theme(legend.title = element_text(size=rel(5),family=FontFamily)) + coord_map(projection="mercator",
 	xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon), 
	ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) + 
	scale_fill_distiller(palette = Palette, breaks=LabelValues, limits=c(min(ScaleLow), 
	max(ScaleHigh)), labels=LabelNames) + ggtitle(Title) +
	guides(fill = guide_legend(override.aes = list(colour = NULL,alpha=AlphaScale),
	reverse = TRUE,keywidth=10,keyheight=10,title=LegendTitleRadarsDaily)) 
	if (AutDefineLegendTop!="yes")
	{
		FigFinal <- FigFinal + geom_point(data=pointdata,aes(pointlon,pointlat,color="black"),size=67.5,shape=15,
		alpha=AlphaPolygon) + theme(legend.key = element_blank()) + scale_color_manual(values=ColourHighestClass,name="",
		labels=paste("> ",formatC(ScaleTopDaily, format="f", digits=1),sep="")) 
	}
	if (PlotLocation=="yes")
	{
		LonLocation <<- LonLocation
		LatLocation <<- LatLocation
		LocationOnMap <- revgeocode(c(LonLocation,LatLocation))
		Title <- paste(paste(TitleRadars,"; ",ExtraText,sep=""),DateTime,paste(SymbolPlotLocation,LocationOnMap,sep=""),sep="\n")
		FigFinal <- FigFinal + geom_point(aes(LonLocation,LatLocation),color=ColourPlotLocation,size=SizePlotLocation,
		shape=SymbolPlotLocation,alpha=AlphaPlotLocation) 
		PointValue <- ReadRainLocation(CoorSystemInputData=CoorSystemInputData,dataf=Rday,
		FileGrid=FileGrid,Lat=LatLocation,Lon=LonLocation,XMiddle=XMiddle,YMiddle=YMiddle) 
		PointValueStr <<- formatC(PointValue, format="f", digits=1) 
		LonText <<- LonText
		LatText <<- LatText
		FigFinal <- FigFinal + geom_text(aes(x=LonText,y=LatText,label=PointValueStr),
		col=ColourPlotLocationText,family=FontFamily,size=SizePlotLocation) + ggtitle(Title)
	}
	print(FigFinal)
	dev.off()
}




