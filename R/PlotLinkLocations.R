## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
##
## Version 1.11
## Copyright (C) 2017 Aart Overeem
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

#' Function which visualises microwave link paths on a map.
#' @description Function which visualises microwave link paths on a map. 
#'
#' @param AlphaLinkLocations Transparency of link paths.
#' @param BBoxOSMauto Compute bounding box from input data or used bounding box defined above?
#' (for OpenStreetMap only). Use "yes" if bounding box is to be computed from interpolation grid.
#' @param ColourLinks Colour of plotted link paths.
#' @param ColourType Colour or black-and-white background map? Use "color" for colour and "bw" 
#' for black-and-white background map.
#' @param dataf data frame which contains (at least) locations of microwave links in Azimuthal 
#' Equidistant Cartesian coordinate system.
#' @param DateTime Date and time for which link locations are plotted. This is used in the
#' title caption of the figure and in the file name.
#' @param ExtraTextLinkLocations Second part of title of plot.
#' @param FigFileLinkLocations Part of figure output file name.
#' @param FigHeight Figure height. 1280 times 1280 pixels seems maximum graphical resolution for 
#' downloaded Google Maps. Because also axes and legend are plotted, it is advised to use e.g. 
#' 1450 times 1450 pixels. 
#' Then the Google Map will remain 
#' approximately 1280 times 1280 pixels. Using higher values is not a problem (e.g. 2000). 
#' In this way it is tried to get the highest possible resolution. For OpenStreetMap the maps may 
#' reach resolutions of 1500 - 2000 pixels. Hence, using FigWidth and FigHeight of 2000 pixels or 
#' higher is advised. The OpenStreetMap itself is stored in file "ggmapTemp.png". From this file 
#' the resolution of the background map can be obtained. This can be useful for determining an 
#' appropriate FigWidth and FigHeight above.
#' @param FigWidth Figure width. 1280 times 1280 pixels seems maximum graphical resolution for 
#' downloaded Google Maps. Because also axes and legend are plotted, it is advised to use e.g. 
#' 1450 times 1450 pixels. Then the Google Map will remain approximately 1280 times 1280 pixels. 
#' Using higher values is not a problem (e.g. 2000). In this way it is tried to get the 
#' highest possible resolution. For OpenStreetMap the maps may reach resolutions of 1500 - 2000 
#' pixels. Hence, using FigWidth and FigHeight of 2000 pixels or higher is advised. The OpenStreetMap 
#' itself is stored in file "ggmapTemp.png". From this file the resolution of the background map can 
#' be obtained. This can be useful for determining an appropriate FigWidth and FigHeight above.
#' @param FolderFigures Folder name of figures.
#' @param FontFamily Specify font family of text in figures. To select the default font use "". 
#' Using "Times" may give warnings when executing the visualisation. In that case the font is not 
#' installed on the computer. This can be solved by using the default font ("").
#' @param GoogleLocDegSpecified If GoogleLocDegSpecified is "yes" then the specified location in 
#' degrees is used is used as the centre of the Google Map. If both GoogleLocNameSpecified and 
#' GoogleLocDegSpecified are not equal to "yes", the bounding box of the map is determined from 
#' the provided grid and used as centre of the Google Map.
#' @param GoogleLocLat Latitude of middle of Google Map (degrees).
#' @param GoogleLocLon Longitude of middle of Google Map (degrees).
#' @param GoogleLocName Location of middle of Google Map, provided as text, e.g. name of city,
#' street name, country.
#' @param GoogleLocNameSpecified If GoogleLocNameSpecified is "yes" then the specified location 
#' name GoogleLocName is used as the centre of the Google Map. If both GoogleLocNameSpecified and 
#' GoogleLocDegSpecified are not equal to "yes", the bounding box of the map is determined from the 
#' provided grid and used as centre of the Google Map.
#' @param GoogleMapType In case of Google Maps: which map type should be used? Available map types: 
#' "terrain", "satellite", "roadmap", and "hybrid".
#' @param GoogleZoomlevel Which zoom level to use for the Google Maps?
#' @param LabelAxisLat Label name of vertical axis.
#' @param LabelAxisLonGoogle Label name of horizontal axis (for Google Maps only).
#' @param LabelAxisLonOSM Label name of horizontal axis (for OpenStreetMap only).
#' @param MapBackground Google Maps or OpenStreetMap as background? Use "Google" for Google Maps and 
#' "OSM" for OpenStreetMap. Note that Google Maps will only plot on a square figure.
#' It seems that mapping with OpenStreetMap (“get openstreetmap”) is no langer supported.
#' This implies that mapping can only be done employing Google Maps. This is not related to
#' the RAINLINK version.
#' @param OSMBottom Latitude in degrees (WGS84) for bottom side of the area for which rainfall depths 
#' are to be plotted (for OpenStreetMap only).
#' @param OSMLeft Longitude in degrees (WGS84) for left side of the area for which rainfall depths are 
#' to be plotted (for OpenStreetMap only). 
#' @param OSMRight Longitude in degrees (WGS84) for right side of the area for which rainfall depths 
#' are to be plotted (for OpenStreetMap only). 
#' @param OSMScale Give value of scale (for OpenStreetMap only). A proper choice of the scale parameter 
#' in get_openstreetmap is difficult. It cannot be computed automatically. Hence, a scale parameter value 
#' should be provided below. The scale parameter should be as small as possible to get the highest 
#' graphical resolution. However, a too low value may result in a map not being downloaded. Hence, the user 
#' should manually supply get_openstreetmap with a scale. It may require some iterations to find the 
#' appropriate value for scale. The file "ggmapTemp.png" is written to disk when an OpenStreetMap is loaded. 
#' The highest possible resolution for a square area is about 2000 x 2000 pixels. 
#' @param OSMTop Latitude in degrees (WGS84) for top side of the area for which rainfall depths are to be 
#' plotted (for OpenStreetMap only).
#' @param SizeLinks Size of plotted link paths.
#' @param SizePlotTitle Size of plot title.
#' @param TitleLinkLocations First part of title of plot.
#' @export PlotLinkLocations
#' @examples
#' PlotLinkLocations(AlphaLinkLocations=AlphaLinkLocations,BBoxOSMauto=BBoxOSMauto,
#' OSMBottom=OSMBottom,ColourLinks=ColourLinks,ColourType=ColourType,dataf=dataf,
#' DateTime=DateTime,ExtraTextLinkLocations=ExtraTextLinkLocations,
#' FigFileLinkLocations=FigFileLinkLocations,FigHeight=FigHeight,FigWidth=FigWidth,
#' FolderFigures=FolderFigures,FontFamily=FontFamily,
#' GoogleLocDegSpecified=GoogleLocDegSpecified,GoogleLocLat=GoogleLocLat,
#' GoogleLocLon=GoogleLocLon,GoogleLocName=GoogleLocName,
#' GoogleLocNameSpecified=GoogleLocNameSpecified,GoogleMapType=GoogleMapType,
#' GoogleZoomlevel=GoogleZoomlevel,LabelAxisLat=LabelAxisLat,
#' LabelAxisLonGoogle=LabelAxisLonGoogle,LabelAxisLonOSM=LabelAxisLonOSM,
#' OSMLeft=OSMLeft,MapBackground=MapBackground,OSMRight=OSMRight,OSMScale=OSMScale,
#' SizeLinks=SizeLinks,SizePlotTitle=SizePlotTitle,
#' TitleLinkLocations=TitleLinkLocations,OSMTop=OSMTop)
#' @author Aart Overeem & Hidde Leijnse
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


PlotLinkLocations <- function(AlphaLinkLocations,BBoxOSMauto,OSMBottom,ColourLinks,ColourType,
dataf,DateTime,ExtraTextLinkLocations,FigFileLinkLocations,FigHeight,FigWidth,FolderFigures,
FontFamily,GoogleLocDegSpecified,GoogleLocLat,GoogleLocLon,GoogleLocName,GoogleLocNameSpecified,
GoogleMapType,GoogleZoomlevel,LabelAxisLat,LabelAxisLonGoogle,LabelAxisLonOSM,OSMLeft,MapBackground,
OSMRight,OSMScale,SizeLinks,SizePlotTitle,TitleLinkLocations,OSMTop)
{

	# Create directory for output files:
	if(!dir.exists(FolderFigures)){ dir.create(FolderFigures) }

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


	# Select unique links over entire data frame. Full-duplex links will be plotted twice.
	DataCoor <- unique(data.frame(cbind(dataf$XStart,dataf$YStart,dataf$XEnd,dataf$YEnd)))
		

	# Plot base map for the considered time interval:
	fig <- ggmap(map, extent = "normal", maprange=FALSE) + 
	theme(axis.title.x=element_text(size =rel(5),family=FontFamily)) + 
	xlab(LabelAxisLon) + theme(axis.title.y=element_text(size =rel(5),family=FontFamily)) + 
	ylab(LabelAxisLat) + theme(axis.text = element_text(size=rel(4),family=FontFamily)) + 
	theme(axis.ticks = element_line(size = 22)) +  
	theme(plot.title = element_text(family = FontFamily, face="bold", size=SizePlotTitle, vjust=3))
		
	
	# Plot microwave link locations:
	# DataCoor has to be made global, otherwise geom_segment will not recognize DataCoor.
	DataCoor <<- DataCoor
	fig <- fig + geom_segment(aes(x=DataCoor[,1],y=DataCoor[,2],
	xend=DataCoor[,3],yend=DataCoor[,4]),data=DataCoor,alpha=AlphaLinkLocations,
	col=ColourLinks,size=SizeLinks)
	# Remove DataCoor:
	rm(DataCoor)


	# Plot remainder of figure and send to jpeg file:
	Title <- paste("\n",TitleLinkLocations,"\n",paste(ExtraTextLinkLocations,DateTime,sep=", "),sep="")
	FigFilename <- paste(FolderFigures,"/",FigFileLinkLocations,DateTime,".jpeg",sep="")
		
	jpeg(FigFilename,width = FigWidth, height = FigHeight) 
	par(family=FontFamily)
	FigFinal <- fig + theme(legend.text = element_text(size=rel(5),family=FontFamily)) + 
	coord_map(projection="mercator",
 	xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon), 
	ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) + 
	ggtitle(Title)
	print(FigFinal)
	dev.off()

}




