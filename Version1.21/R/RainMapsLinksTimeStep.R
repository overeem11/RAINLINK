## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.21
## Copyright (C) 2021 Aart Overeem
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

#' Function which visualises link-based rainfall depths for each time interval in a supplied period.
#' @description Function which visualises link-based rainfall depths for each time interval in a supplied period. 
#' Requires interpolation grid and file with polygons of pixels, the polygons having four angular points. 
#' Note that the zoom level for Google Maps can be chosen. This zoom level will not always match with the area 
#' to be plotted. Downloading a GoogleMaps background map is very fast, but downloading an OpenStreetMap may 
#' require tenths of seconds. Note that such a map is downloaded only once, which saves time in case of multiple 
#' rainfall maps. 
#'
#' @param AlphaLinksTimeStep Transparency of link paths.
#' @param AlphaPlotLocation Transparency of plotted symbol for specified location on map.
#' @param AlphaPolygon Transparency of polygons.
#' @param AutDefineLegendTop Let R automatically define highest value of legend in case of "yes". 
#' Then the highest class, i.e. the one plotted separately above the other classes, is not plotted anymore.
#' @param BBoxOSMauto Compute bounding box from input data or used bounding box defined above? 
#' (for OpenStreetMap and Stamen Map only). Use "yes" if bounding box is to be computed from interpolation grid.
#' @param ColourLinks Colour of plotted link paths.
#' @param ColoursNumber Number of colour classes in legend.
#' @param ColourPlotLocation Colour of plotted symbol for specified location on map.
#' @param ColourPlotLocationText Colour of plotted rainfall depth for specified location on map.
#' @param ColourScheme Hexadecimal codes or names for colours of legend.
#' @param ColourType Colour or black-and-white background map? Use "color" for colour and "bw" 
#' for black-and-white background map.
#' @param ColourHighestClass Colour of highest class.
#' @param ConversionDepthToIntensity Conversion factor from rainfall depth (mm) to intensity (mm/h).
#' @param CoorSystemInputData Define coordinate system of input data 
#' (e.g. "+init=epsg:4326" for WGS84 in decimal degrees).
#' @param DateTimeEndRainMaps Date and time at which rainfall mapping ends.
#' @param DateTimeStartRainMaps Date and time at which rainfall mapping starts.
#' @param ExtraDeg To reduce computational time, it is automatically determined which grid cells fall 
#' within the plotted region. To also plot grid cell values which partly fall outside the plotted region, 
#' a positive number for ExtraDeg should be specified (decimal degrees). 
#' This should typically be at least the size of one grid cell in decimal degrees.
#' @param ExtraText Second part of title of plot.
#' @param FigFileLinksTimeStep Part of figure output file name.
#' @param FigHeight Figure height. 1280 times 1280 pixels seems maximum graphical resolution for downloaded 
#' Google Maps. Because also axes and legend are plotted, it is advised to use e.g. 1450 times 1450 pixels. 
#' Then the Google Map will remain approximately 1280 times 1280 pixels. Using higher values is not a problem 
#' (e.g. 2000). In this way it is tried to get the highest possible resolution. For OpenStreetMap the maps may 
#' reach resolutions of 1500 - 2000 pixels. Hence, using FigWidth and FigHeight of 2000 pixels or higher is 
#' advised. The OpenStreetMap itself is stored in file "ggmapTemp.png". From this file the resolution of the 
#' background map can be obtained. This can be useful for determining an appropriate FigWidth and FigHeight above.
#' @param FigWidth Figure width. 1280 times 1280 pixels seems maximum graphical resolution for downloaded Google 
#' Maps. Because also axes and legend are plotted, it is advised to use e.g. 1450 times 1450 pixels. Then the 
#' Google Map will remain approximately 1280 times 1280 pixels. Using higher values is not a problem (e.g. 2000). 
#' In this way it is tried to get the highest possible resolution. For OpenStreetMap the maps may reach 
#' resolutions of 1500 - 2000 pixels. Hence, using FigWidth and FigHeight of 2000 pixels or higher is advised. 
#' The OpenStreetMap itself is stored in file "ggmapTemp.png". From this file the resolution of the background map 
#' can be obtained. This can be useful for determining an appropriate FigWidth and FigHeight above.
#' @param FileGrid File with interpolation grid in same coordinate system as CoorSystemInputData.
#' @param FilePolygonsGrid Name of file with polygons of interpolation grid.
#' @param FolderFigures Folder name of figures.
#' @param FolderRainMaps Folder name of interpolated link data (input).
#' @param FolderRainEstimates Folder name of input link path data.
#' @param FontFamily Specify font family of text in figures. To select the default font use "". Using "Times" 
#' may give warnings 
#' when executing the visualisation. In that case the font is not installed on the computer. This can be solved by 
#' using the default font ("").
#' @param GoogleLocDegSpecified If GoogleLocDegSpecified is "yes" then the specified location in decimal degrees is used 
#' is used as the centre of the Google Map. If both GoogleLocNameSpecified and GoogleLocDegSpecified are not equal 
#' to "yes", the bounding box of the map is determined from the provided grid and used as centre of the Google Map.
#' @param GoogleLocLat Latitude of middle of Google Map (decimal degrees).
#' @param GoogleLocLon Longitude of middle of Google Map (decimal degrees).
#' @param GoogleLocName Location of middle of Google Map, provided as text, e.g. name of city, street name, country
#' @param GoogleLocNameSpecified If GoogleLocNameSpecified is "yes" then the specified location name GoogleLocName 
#' is used as the centre of the Google Map. If both GoogleLocNameSpecified and GoogleLocDegSpecified are not equal 
#' to "yes", the bounding box of the map is determined from the provided grid and used as centre of the Google Map.
#' @param GoogleMapType In case of Google Maps: which map type should be used? Available map types: "terrain", 
#' "satellite", "roadmap", and "hybrid".
#' @param GoogleZoomlevel Which zoom level to use for the Google Maps?
#' @param LabelAxisLat Label name of vertical axis.
#' @param LabelAxisLonGoogle Label name of horizontal axis (for Google Maps only).
#' @param LabelAxisLonOSM Label name of horizontal axis (for OpenStreetMap only).
#' @param LabelAxisLonStamen Label name of horizontal axis (for Stamen Map only).
#' @param LatLocation Latitude of location on map (decimal degrees).
#' @param LatText Latitude of text (rainfall depth) of location on map (decimal degrees).
#' @param LegendSize Size of legend (choose e.g. 75 for 6 classes and 50 for 10 classes).
#' @param LegendTitleLinksTimeStep Title of legend.
#' @param LonLocation Longitude of location on map (decimal degrees).
#' @param LonText Longitude of text (rainfall depth) of location on map (decimal degrees).
#' @param ManualScale Manually supply the legend breaks if ManuelScale is not equal to "no". 
#' Interval breaks are determined manually from ScaleLow and ScaleHigh. If ManualScale is "no" 
#' interval breaks are determined automatically. 
#' @param MapBackground Google Maps, OpenStreetMap or Stamen Map as background? Use "Google" for Google Maps, 
#' "OSM" for OpenStreetMap and "Stamen" for Stamen Map (based on OpenStreetMap data). 
#' Note that Google Maps will only plot on a square figure.
#' It seems that mapping with OpenStreetMap (“get openstreetmap”) is no langer supported.
#' This implies that mapping can only be done employing Google Maps (if Google API key is obtained) or via
#' Stamen Map. This is not related to the RAINLINK version.
#' @param OSMBottom Latitude in decimal degrees (WGS84) for bottom side of the area for which rainfall depths are to be plotted 
#' (for OpenStreetMap & Stamen Maps only).
#' @param OSMLeft Longitude in decimal degrees (WGS84) for left side of the area for which rainfall depths are to be plotted 
#' (for OpenStreetMap & Stamen Maps only). 
#' @param OSMRight Longitude in decimal degrees (WGS84) for right side of the area for which rainfall depths are to be plotted 
#' (for OpenStreetMap & Stamen Maps only). 
#' @param OSMScale Give value of scale (for OpenStreetMap only). A proper choice of the scale parameter in get_openstreetmap 
#' is difficult. It cannot be computed automatically. Hence, a scale parameter value should be provided below. The scale 
#' parameter should be as small as possible to get the highest graphical resolution. However, a too low value may result in a 
#' map not being downloaded. Hence, the user should manually supply get_openstreetmap with a scale. It may require some 
#' iterations to find the appropriate value for scale. The file "ggmapTemp.png" is written to disk when an OpenStreetMap is 
#' loaded. The highest possible resolution for a square area is about 2000 x 2000 pixels. 
#' @param OSMTop Latitude in decimal degrees (WGS84) for top side of the area for which rainfall depths are to be plotted 
#' (for OpenStreetMap & Stamen Maps only).
#' @param OutputFileType Choose output file type of image: jpeg, png or tiff.
#' @param PlotLocation A location is plotted on map if PlotLocation is "yes".
#' @param PixelBorderCol Choose colour of pixel borders. Use NA (without quotes) to not plot pixel borders. 
#' If the pixels are relatively small with respect to the plotted region, the graphical quality of the pixel borders 
#' deteriorates due to low number of pixels (low resolution).
#' @param PlotBelowScaleBottom Plot grid lines for polygons below threshold ScaleBottomTimeStep or ScaleBottomDaily? 
#' If "yes" grid lines are plotted, otherwise they are not plotted.
#' @param PlotLocLinks Plot locations of links in plot? If "yes" than locations of links are plotted in the plot. 
#' Note that full-duplex links are plotted twice.
#' @param ScaleBottomTimeStep ScaleBottomDaily Lowest class starts at this threshold (minimum rainfall accumulation 
#' (mm) to be plotted). Using a value clearly above 0 mm can save a lot of computation time if the polygons belonging 
#' to values below the threshold are not plotted.
#' @param ScaleHigh ScaleHigh Highest value per class interval, i.e. the highest legend breaks, if these are manually chosen.  
#' Please note that in case of x values in ColoursNumber, ScaleHigh should also contain x values.
#' @param ScaleLow ScaleLow Lowest value per class interval, i.e. the lowest legend breaks, if these are manually chosen. 
#' Please note that in case of x values in ColoursNumber, ScaleLow should also contain x values.
#' @param ScaleTopTimeStep Highest colour class ends here (maximum rainfall accumulation (mm) to be plotted). 
#' Sometimes the legend is not correctly plotted. In that case try other values for ScaleTopTimeStep and/or 
#' ScaleBottomTimeStep For instance, if the highest class (> x mm) is plotted below instead of above the other 
#' classes. Or if the number of classes does not match the number of chosen classes. Another way to prevent 
#' this is to manually give the legend breaks (ManualScale not equal to "no").
#' @param SizeLinks  Size of plotted link paths.
#' @param SizePixelBorder Size of pixel borders.
#' @param SizePlotLocation Size of symbol and accompanied text for specified location on map.
#' @param SizePlotTitle Size of plot title.
#' @param StamenMapType In case of Stamen Maps: which map type should be used? Available map types which 
#' seem most useful and work: "toner-hybrid" &, recommended: "toner-lite", "terrain" & "watercolor".  
#' @param StamenZoomlevel Which zoom level to use for the Stamen Maps? This determines the level of detail. 
#' Large values take more time. It does not determine the domain of the area which is plotted.
#' @param SymbolPlotLocation Symbol to be plotted for specified location on map.
#' @param TitleLinks First part of title of plot.
#' @param XMiddle The longitude of the centre of the Azimuthal Equidistant Cartesian coordinate system, 
#' given in the coordinate system of the input data.
#' @param YMiddle The latitude of the centre of the Azimuthal Equidistant Cartesian coordinate system, 
#' given in the coordinate system of the input data.
#' @export RainMapsLinksTimeStep
#' @examples
#' RainMapsLinksTimeStep(AlphaLinksTimeStep=AlphaLinksTimeStep,
#' AlphaPlotLocation=AlphaPlotLocation,AlphaPolygon=AlphaPolygon,
#' AutDefineLegendTop=AutDefineLegendTop,BBoxOSMauto=BBoxOSMauto,ColourLinks=ColourLinks,
#' ColoursNumber=ColoursNumber,ColourPlotLocation=ColourPlotLocation,
#' ColourPlotLocationText=ColourPlotLocationText,ColourScheme=ColourScheme,
#' ColourType=ColourType,ConversionDepthToIntensity=ConversionDepthToIntensity,
#' CoorSystemInputData=CoorSystemInputData,DateTimeEndRainMaps=DateTimeEndRainMaps,
#' DateTimeStartRainMaps=DateTimeStartRainMaps,ExtraDeg=ExtraDeg,ExtraText=ExtraText,
#' FigFileLinksTimeStep=FigFileLinksTimeStep,FigHeight=FigHeight,FigWidth=FigWidth,
#' FileGrid=FileGrid,FilePolygonsGrid=FilePolygonsGrid,FolderFigures=FolderFigures,
#' FolderRainMaps=FolderRainMaps,FolderRainEstimates=FolderRainEstimates,
#' FontFamily=FontFamily,GoogleLocDegSpecified=GoogleLocDegSpecified,
#' GoogleLocLat=GoogleLocLat,GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,
#' GoogleLocNameSpecified=GoogleLocNameSpecified,GoogleMapType=GoogleMapType,
#' GoogleZoomlevel=GoogleZoomlevel,LabelAxisLat=LabelAxisLat,
#' LabelAxisLonGoogle=LabelAxisLonGoogle,LabelAxisLonOSM=LabelAxisLonOSM,
#' LabelAxisLonStamen=LabelAxisLonStamen,LatLocation=LatLocation,LatText=LatText,
#' LegendSize=LegendSize,LegendTitleLinksTimeStep=LegendTitleLinksTimeStep,
#' LonLocation=LonLocation,LonText=LonText,ManualScale=ManualScale,
#' MapBackground=MapBackground,OSMBottom=OSMBottom,OSMLeft=OSMLeft,
#' OSMRight=OSMRight,OSMScale=OSMScale,OSMTop=OSMTop,OutputFileType=OutputFileType,
#' PlotLocation=PlotLocation,PixelBorderCol=PixelBorderCol,
#' PlotBelowScaleBottom=PlotBelowScaleBottom,PlotLocLinks=PlotLocLinks,
#' ScaleBottomTimeStep=ScaleBottomTimeStep,ScaleHigh=ScaleHigh,ScaleLow=ScaleLow,
#' ScaleTopTimeStep=ScaleTopTimeStep,SizeLinks=SizeLinks,SizePixelBorder=SizePixelBorder,
#' SizePlotLocation=SizePlotLocation,SizePlotTitle=SizePlotTitle,
#' StamenMapType=StamenMapType,StamenZoomlevel=StamenZoomlevel,
#' SymbolPlotLocation=SymbolPlotLocation,TitleLinks=TitleLinks,XMiddle=XMiddle,
#' YMiddle=YMiddle)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


RainMapsLinksTimeStep <- function(AlphaLinksTimeStep,AlphaPlotLocation,AlphaPolygon,
AutDefineLegendTop,BBoxOSMauto,ColourLinks,ColoursNumber,ColourPlotLocation,
ColourPlotLocationText,ColourScheme,ColourType,ColourHighestClass,ConversionDepthToIntensity,
CoorSystemInputData,DateTimeEndRainMaps,DateTimeStartRainMaps,ExtraDeg,ExtraText,
FigFileLinksTimeStep,FigHeight,FigWidth,FileGrid,FilePolygonsGrid,FolderFigures,
FolderRainMaps,FolderRainEstimates,FontFamily,GoogleLocDegSpecified,GoogleLocLat,
GoogleLocLon,GoogleLocName,GoogleLocNameSpecified,GoogleMapType,GoogleZoomlevel,
LabelAxisLat,LabelAxisLonGoogle,LabelAxisLonOSM,LabelAxisLonStamen,LatLocation,LatText,
LegendSize,LegendTitleLinksTimeStep,LonLocation,LonText,ManualScale,MapBackground,OSMBottom,OSMLeft,
OSMRight,OSMScale,OSMTop,OutputFileType,PlotLocation,PixelBorderCol,PlotBelowScaleBottom,
PlotLocLinks,ScaleBottomTimeStep,ScaleHigh,ScaleLow,ScaleTopTimeStep,SizeLinks,
SizePixelBorder,SizePlotLocation,SizePlotTitle,StamenMapType,StamenZoomlevel,
SymbolPlotLocation,TitleLinks,XMiddle,YMiddle)
{

	# Create directory for output files:
	if(!dir.exists(FolderFigures)){ dir.create(FolderFigures) }

	# Make list of input files: 
	Files <- list.files(path = FolderRainMaps, all.files=FALSE,
	full.names=TRUE, recursive=FALSE, pattern="linkmap")
	FilesNames <- list.files(path = FolderRainMaps, all.files=FALSE,
	full.names=FALSE, recursive=FALSE, pattern="linkmap")
	DateFiles <- substr(FilesNames,9,20)
	# Only select files for supplied period:
	Files <- Files[which(DateFiles>=DateTimeStartRainMaps&DateFiles<=DateTimeEndRainMaps)]
	DateFiles <- DateFiles[which(DateFiles>=DateTimeStartRainMaps&DateFiles<=DateTimeEndRainMaps)]
	DateFiles <- DateFiles[which(file.info(Files)$size>0)]
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


	# Use Stamen Map as background. # Use predefined scale.
	if (MapBackground=="Stamen")
	{
		if (BBoxOSMauto!="yes")
		{
			# Determine bounding box from specified coordinates.
			map <- get_stamenmap(bbox = c(left = OSMLeft, bottom = OSMBottom, 
			right = OSMRight, top = OSMTop),zoom=StamenZoomlevel,maptype=StamenMapType,crop=T,messaging=F,urlonly=FALSE,force=FALSE)
		}
		if (BBoxOSMauto=="yes")
		{
			# Determine bounding box determined from interpolation grid:
			bbox <- make_bbox(PolygonsGrid[,1], PolygonsGrid[,2])

			map <- get_stamenmap(bbox = bbox,zoom=StamenZoomlevel,maptype=StamenMapType,crop=T,messaging=F,urlonly=FALSE,force=FALSE)
		}
		LabelAxisLon <- LabelAxisLonStamen
	}


	for (FileNr in 1:length(Files))
	{
		# Read interpolated link rainfall intensities:
		Data <- read.table(Files[FileNr],header=TRUE)

		# Read file with locations of microwave links. 
		if (PlotLocLinks=="yes")
		{		
			FileLinkLoc <- gsub("map","data",substr(Files[FileNr],(nchar(FolderRainMaps)+2),nchar(Files[FileNr])))
			TEMP <- read.table(paste(FolderRainEstimates,"/",FileLinkLoc,sep=""),header=TRUE)
		        # Select unique links. Full-duplex links will be plotted twice. Links with only no data (= NA value for RainfallDepthPath) will not be plotted.
                        cond <- which(TEMP$RainfallDepthPath>=0)
		        DataCoor <- unique(data.frame(cbind(TEMP$XStart[cond],TEMP$YStart[cond],TEMP$XEnd[cond],TEMP$YEnd[cond])))
		}

		IntervalNr <- as.numeric(substr(Files[FileNr],(nchar(FolderRainMaps)+18),(nchar(Files[FileNr])-4)))

		# Convert rainfall intensities to rainfall depths:
		Data$RainDepths <- Data$RainIntensity / ConversionDepthToIntensity

		
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

		Value <- ToPolygonsRain(Data=Data$RainDepths[CondMap])

		DataPolygons <- data.frame(PolygonsGrid[CondPolygon,],Value)

		# Assign column names:
		names(DataPolygons) <- c("lon","lat","rain")

		if (AutDefineLegendTop=="yes")
		{
			ScaleTopTimeStep <- ceiling(max(DataPolygons$rain,na.rm=T))
			# If all values are 0 mm, the automatic scale would not make sense and contain
			# negative values. A higher value is assigned to ScaleTopTimeStep to prevent this.
			if (ScaleTopTimeStep==0)
			{
				ScaleTopTimeStep <- 10
			}
		}


		if (ManualScale=="no")
		{
			# Determine interval of scale:
			ScaleInterval <- (ScaleTopTimeStep - ScaleBottomTimeStep)/ColoursNumber

			# Determine lowest value of scale:
			ScaleLow <- seq(from=ScaleBottomTimeStep,to=ColoursNumber*ScaleInterval,by=ScaleInterval) 

			# Determine highest value of scale:
			ScaleHigh <- seq(from=ScaleBottomTimeStep+ScaleInterval,to=ColoursNumber*
			ScaleInterval+ScaleBottomTimeStep,by=ScaleInterval)
		}
		if (ManualScale!="no")
		{
			ScaleTopTimeStep <- max(ScaleHigh)
		}
		ScaleLow <<- ScaleLow
		

		# Plot base map for the considered time interval:
		Fig <- ggmap(map, extent = "normal", maprange=FALSE) + 
		theme(axis.title.x=element_text(size =rel(5),family=FontFamily)) + 
		xlab(LabelAxisLon) + theme(axis.title.y=element_text(size =rel(5),family=FontFamily)) + 
		ylab(LabelAxisLat) + theme(axis.text = element_text(size=rel(4),family=FontFamily)) + 
		theme(axis.ticks = element_line(size = 22)) +  
		theme(plot.title = element_text(family = FontFamily, face="bold", size=SizePlotTitle, vjust=3))

		LabelNames <- paste(" ",formatC(ScaleLow, format="f", digits=1),"-",
		formatC(ScaleHigh, format="f", digits=1),sep="") 
		LabelValues = c(ScaleLow)
	
		Colours <- ColourScheme
		MaxColour <- Colours[length(Colours)]
		ColoursOrig <- rev(Colours)
		Colours[length(Colours)] <- c(NA)
		Colours <- Colours[!is.na(Colours)]
			
		if (PlotLocLinks=="yes")
		{	
			# DataCoor has to be made global, otherwise geom_segment will not recognize DataCoor.
			DataCoor <<- DataCoor	
			Fig <- Fig + geom_segment(aes(x=DataCoor[,1],y=DataCoor[,2],
			xend=DataCoor[,3],yend=DataCoor[,4]),data=DataCoor,alpha=AlphaLinksTimeStep,
			col=ColourLinks,size=SizeLinks)
		}


		for (p in 1:ColoursNumber)
		{ 
			p <<- p
			cond <- which(DataPolygons$rain>=ScaleLow[p]&DataPolygons$rain<ScaleHigh[p])
			Selected = DataPolygons[cond,]
			dataf <- RAINLINK::Polygons(cond=cond, Selected=Selected)
			if (dataf[1,1]!=-99999999)
			{
				Fig <- Fig + geom_polygon(aes(x = Lon, y = Lat, fill = value), fill = Colours[p], data = dataf, 
				alpha=AlphaPolygon, col=PixelBorderCol, size=SizePixelBorder)   
			}
			if (dataf[1,1]==-99999999)
			{
				# To make sure that the colour scale is always plotted:
				polygondata = data.frame(c(999),c(999),ScaleLow[p])
				names(polygondata) = c("polygonlon","polygonlat","value")	
				Fig <- Fig + geom_polygon(aes(x = polygonlon, y = polygonlat, fill = value), fill = Colours[p], 
				data = polygondata, alpha=AlphaPolygon, col=PixelBorderCol, size=SizePixelBorder)     
			}
		}


		if (PlotBelowScaleBottom=="yes")
		{
			cond <- which(DataPolygons$rain<ScaleBottomTimeStep)
			Selected = DataPolygons[cond,]
			dataf <- RAINLINK::Polygons(cond=cond, Selected=Selected)
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
			dataf <- RAINLINK::Polygons(cond=cond, Selected=Selected)
			if (dataf[1,1]!=-99999999)
			{
				Fig <- Fig + geom_polygon(aes(x = Lon, y = Lat), data = dataf, alpha=AlphaPolygon,
				col=PixelBorderCol, size=SizePixelBorder, fill = MaxColour)
			}
		}


		pointdata <<- data.frame(999,999)
		names(pointdata) = c("pointlon","pointlat")


		# Plot remainder of figure and send to output file:
		Title <- paste(paste(TitleLinks,"; ",ExtraText,sep=""),paste("End time: ",DateFiles[FileNr]),sep="\n")
		FigFilename <- paste(FolderFigures,"/",FigFileLinksTimeStep,DateFiles[FileNr],".",OutputFileType,sep="")
		
                if (OutputFileType=="jpeg")
                {
		   jpeg(FigFilename,width = FigWidth, height = FigHeight) 
                }
                if (OutputFileType=="png")
                {
		   png(FigFilename,width = FigWidth, height = FigHeight) 
                }
                if (OutputFileType=="tiff")
                {
		   tiff(FigFilename,width = FigWidth, height = FigHeight) 
                }
		par(family=FontFamily)
		FigFinal <- Fig + theme(legend.text = element_text(size=rel(5),family=FontFamily)) + 
		theme(legend.title = element_text(size=rel(5),family=FontFamily)) + coord_map(projection="mercator",
	 	xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon), 
		ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) + ggtitle(Title)

		if (AutDefineLegendTop!="yes")
		{
			LabelNames2 <- c(LabelNames,paste("> ",formatC(ScaleTopTimeStep, format="f", digits=1),sep=""))
			FigFinal <- FigFinal + geom_point(data=pointdata,aes(pointdata[,1],pointdata[,2],color="black"),size=67.5,shape=15,
			alpha=AlphaPolygon,na.rm=TRUE) + theme(legend.key = element_blank()) + scale_color_manual(values=ColoursOrig,name=LegendTitleRadarsTimeStep,
			labels=rev(LabelNames2),limits=rev(c(min(ScaleLow),ScaleHigh))) + theme(legend.title = element_text(face="bold")) + 
                        guides(color = guide_legend(override.aes = list(size = LegendSize)))
			# With na.rm=TRUE we suppress warnings, which was needed since the following warning was provided many times: 
			# "Removed 1 rows containing missing values (geom_point)". This warning can be disregarded.				
		}

		if (AutDefineLegendTop!="no")
		{
			FigFinal <- FigFinal + geom_point(data=pointdata,aes(pointdata[,1],pointdata[,2],color="black"),size=67.5,shape=15,
			alpha=AlphaPolygon,na.rm=TRUE) + theme(legend.key = element_blank()) + scale_color_manual(values=rev(Colours),name=LegendTitleRadarsTimeStep,
			labels=rev(LabelNames),limits=rev(c(ScaleLow))) + theme(legend.title = element_text(face="bold")) + 
                        guides(color = guide_legend(override.aes = list(size = LegendSize)))
			# With na.rm=TRUE we suppress warnings, which was needed since the following warning was provided many times: 
			# "Removed 1 rows containing missing values (geom_point)". This warning can be disregarded.
		}

		if (PlotLocation=="yes")
		{
			LonLocation <<- LonLocation
			LatLocation <<- LatLocation
			LocationOnMap <- revgeocode(c(LonLocation,LatLocation))
			Title <- paste(paste(TitleLinks,"; ",ExtraText,sep=""),paste("End time: ",DateFiles[FileNr]),paste("+",LocationOnMap,sep=""),sep="\n")
			FigFinal <- FigFinal + geom_point(aes(LonLocation,LatLocation),color=ColourPlotLocation,size=SizePlotLocation,
			shape=SymbolPlotLocation,alpha=AlphaPlotLocation) 
			PointValue <- ReadRainLocation(CoorSystemInputData=CoorSystemInputData,dataf=Data$RainDepths,
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

}




