## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
## 
## Version 1.2
## Copyright (C) 2020 Aart Overeem
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

#' Function for plotting characteristics of topology of commercial microwave link network.
#' @description Function for plotting characteristics of topology of commercial microwave link data. This function provides the following figures:  
#' \enumerate{
#'   \item Bar plot with percentage of links for bins of link path length (km).
#'   \item Bar plot with percentage of links for bins of microwave frequency (GHz).
#'   \item Bar plot with percentage of links for bins of orientation (degrees).
#'   \item Scatter plot of microwave frequency (GHz) versus link path length (km).
#'   \item Scatter density plot of microwave frequency (GHz) versus link path length (km).
#' }
#'
#' Does not depend on sampling strategy.
#'
#' The input microwave link data do not have to be sorted chronologically. Full-duplex links will give two data entries, these will both be used. 
#'
#' The computed percentages in the bar plots are only based on the range of classes presented in the bar plots, i.e. data outside these classes are not used in the computations.
#'
#' When Rmean is provided, all figures are only based on data where the link-derived rainfall intensities are equal to or larger than 0 mm h\eqn{^{-1}}.
#' Note that Data object must be preprocessed by function ''PreprocessingMinMaxRSL'' if Rmean is provided.
#'
#' @param Data Data frame with microwave link data (use data(Linkdata) to load example data).
#' @param CoorSystemInputData Define coordinate system of input data (in case of
#' WGS84 provide NULL).
#' @param FigNameBarplotAngle Name of file with bar plot with percentage of links for bins of link orientation. The extension must be ''.pdf''.
#' @param FigNameBarplotFrequency Name of file with bar plot with percentage of links for bins of link path length. The extension must be ''.pdf''.
#' @param FigNameBarplotPathLength Name of file with bar plot with percentage of links for bins of microwave frequency. The extension must be ''.pdf''.
#' @param FigNameFrequencyVsPathLength Name of file with scatter plot of microwave frequency versus link path length. The extension must be ''.pdf''.
#' @param FigNameScatterdensityplotFrequencyVsPathLength Name of file with scatter density plot of microwave frequency versus link path length. The extension must be ''.pdf''.
#' @param Maxf Maximum microwave frequency to be plotted in bar plot (GHz). This is the value where the last bin class ends.
#' @param Minf Minimum microwave frequency to be plotted in bar plot (GHz). This is the value where the first bin class ends.
#' @param MaxL Maximum link path length to be plotted in bar plot (km). This is the value where the last bin class ends.
#' @param MinL Minimum link path length to be plotted in bar plot (km). This is the value where the first bin class ends.
#' @param Rmean Vector of link-derived rainfall intensities (mm h\eqn{^{-1}}) with length equal to Data.
#' @param Stepf Bin size of microwave frequency classes for bar plot in GHz.
#' @param StepL Bin size of link path length classes for bar plot in km.
#' @return Figures with characteristics of topology of commercial microwave link network.
#' @export Topology
#' @examples
#' data(Linkdata)
#' Topology(Data=Linkdata,CoorSystemInputData=NULL,FigNameBarplotAngle="Barplot_Orientation.pdf",
#' FigNameBarplotFrequency="Barplot_Frequency.pdf",FigNameBarplotPathLength="Barplot_PathLength.pdf",
#' FigNameFrequencyVsPathLength="Frequency_vs_PathLength.pdf",
#' FigNameScatterdensityplotFrequencyVsPathLength="ScatterdensityPlot_Frequency_vs_PathLength.pdf",
#' Maxf=40,Minf=13,MaxL=21,MinL=1,Rmean=Rmean,Stepf=1.5,StepL=2)
#' @author Aart Overeem
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


Topology <- function(Data,CoorSystemInputData=NULL,FigNameBarplotAngle,FigNameBarplotFrequency,FigNameBarplotPathLength, FigNameFrequencyVsPathLength,FigNameScatterdensityplotFrequencyVsPathLength,Maxf,Minf,MaxL,MinL,Rmean=NULL,Stepf,StepL,verbose=TRUE)
{

	# If Rmean is provided, only select data for which the link-derived rainfall intensities are equal to or larger than 0.
	if (!is.null(Rmean))
	{
		cond <- which(Rmean>=0)
		Data <- Data[cond,]
	}

	# Bar plot link length.
	L <- Data$PathLength 
	pdf(FigNameBarplotPathLength,family="Times")
	par(pty="s")
	par(ps=24)
	par(mar=c(5,5,1,1)+0.1)

	Ldata <- c(NA)
	NrLengthClass <- 0
	for (lengthclass in seq(MinL,MaxL,StepL))
	{
		if ((lengthclass-StepL) < 0)
		{
			stop("Use a higher value for MinL in combination with the chosen value for StepL!")
		}
		NrLengthClass <- NrLengthClass + 1
		Ldata[NrLengthClass] <- length(L[L>(lengthclass-StepL)&L<=lengthclass])
	}
	total=sum(Ldata)
	perc=100*Ldata/total 

	ylimbarplot <- c(0,max(perc))
	barplot(perc,xlab = "Link length (km)", ylab = "Percentage",
	xaxt="n",tcl=.5,ylim=ylimbarplot,cex.lab=1.3)
	at <- 0.1
	axis(side = 1, at = at, labels = (MinL-StepL), cex.axis = 0.6)
	for (lengthclass in seq(MinL,MaxL,StepL))
	{
		at <- at + 1.2
		axis(side = 1, at = at, labels = lengthclass, cex.axis = 0.6) 	
	}
        legend("topright",legend=substitute(bar(italic(L)) == value*" km",list(value=formatC(mean(L), format="f", digits=2))),bty="n",cex=1)
	dev.off()
	L <- c(NA)


	# Bar plot microwave frequency.
	f <- Data$Frequency
	pdf(FigNameBarplotFrequency,family="Times")
	par(pty="s")
	par(ps=24)
	par(mar=c(5,5,1,1)+0.1)

	fdata <- c(NA)
	NrFrequencyClass <- 0
	for (frequencyclass in seq(Minf,Maxf,Stepf))
	{
		if ((frequencyclass-Stepf) < 0)
		{
			stop("Use a higher value for Minf in combination with the chosen value for Stepf!")
		}
		NrFrequencyClass <- NrFrequencyClass + 1
		fdata[NrFrequencyClass] <- length(f[f>(frequencyclass-Stepf)&f<=frequencyclass])
	}
	total=sum(fdata)
	perc=100*fdata/total 

	ylimbarplot <- c(0,max(perc))
	barplot(perc,xlab = "Frequency (GHz)", ylab = "Percentage",
	xaxt="n",tcl=.5,ylim=ylimbarplot,cex.lab=1.3)
	at <- 0.1
	axis(side = 1, at = at, labels = (Minf-Stepf), cex.axis = 0.4)
	for (frequencyclass in seq(Minf,Maxf,Stepf))
	{
		at <- at + 1.2
		axis(side = 1, at = at, labels = frequencyclass, cex.axis = 0.4) 	
	}
        legend("topleft",legend=substitute(bar(italic(f)) == value*" GHz",list(value=formatC(mean(f), format="f", digits=2))),bty="n",cex=1)
	dev.off()
	f <- c(NA)




	# Bar plot orientation links.
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
	angle <- atan( (XEnd - XStart) / (YEnd - YStart) ) 
	angle <- 180 + angle * 180/pi

	pdf(FigNameBarplotAngle,family="Times")
	par(pty="s")
	par(ps=24)
	par(mar=c(5,5,1,1)+0.1)

	Angledata <- c(NA)
	NrAngleClass <- 0
	Stepangle <- 10
	for (angleclass in seq(100,270,Stepangle))
	{
		NrAngleClass <- NrAngleClass + 1
		Angledata[NrAngleClass] <- length(angle[angle>(angleclass-Stepangle)&angle<=angleclass])
	}
	total=sum(Angledata)
	perc=100*Angledata/total 

	ylimbarplot <- c(0,max(perc))
	barplot(perc,xlab = "Link direction (degrees)", ylab = "Percentage",
	xaxt="n",tcl=.5,ylim=ylimbarplot,cex.lab=1.3)
	at <- 0.1
	axis(side = 1, at = at, labels = "90", cex.axis = 0.5)
	for (angleclass in seq(100,270,Stepangle))
	{
		at <- at + 1.2
		axis(side = 1, at = at, labels = angleclass, cex.axis = 0.5) 	
	}
	dev.off()
	angle <- c(NA)



	# Frequency versus link length (unique combinations are plotted once).
	q <- unique(data.frame(cbind(Data$PathLength, Data$Frequency)))
	pdf(FigNameFrequencyVsPathLength,family="Times") 
	par(pty="s")
	par(mar=c(5,5,1,1)+0.1)
	par(ps=24)
	plot(q[,1],q[,2],xlab=expression(italic(L) *" (km)"),ylab=expression(italic(f) *" (GHz)"),pch=16,cex=1.4)
	dev.off()



	# Scatter density plot frequency versus link length.
	pdf(FigNameScatterdensityplotFrequencyVsPathLength,family="Times",width=8,height=6.5) 
	spam <- range(c(min(Data$PathLength),max(Data$PathLength),min(Data$Frequency),max(Data$Frequency)))
	figure <- hexbinplot(Data$Frequency ~ Data$PathLength, aspect = 1, cex.lab=1.3, cex.title=1.3, ylab=expression(italic(f) *" (GHz)"),xlab=expression(italic(L) *" (km)"), 					   xbnds=c(floor(spam[1]),ceiling(spam[2])),xbins=ceiling(spam[2])/1, style="colorscale",scales = list(x = list(cex=2),y = list(cex=2)),par.settings = 	list(par.xlab.text=list(cex=2),par.ylab.text=list(cex=2)),
	colorcut = seq(0, 1, length = 7), colramp = function(n) plinrain(n, beg=160, end=20), panel=function(x, y, ...){
               panel.hexbinplot(x,y,...)
           })
	print(figure)
	dev.off()

}



