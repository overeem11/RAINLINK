## The RAINLINK package. Retrieval algorithm for rainfall mapping from microwave links 
## in a cellular communication network.
## 
## Version 1.3
## Copyright (C) 2022 Aart Overeem
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

#' Function for plotting data availability of commercial microwave link network.
#' @description Function for plotting data availability of commercial microwave link data. This function provides the following figures:  
#' \enumerate{
#'   \item Bar plot with percentage of sub-links for bins of data availability (%).
#'   \item Bar plot with percentage of link paths for bins of data availability (%).
#'   \item Plot with average number of sub-links and link paths for each time interval of the sampling strategy.
#' }
#' In addition, the Average number of sub-links and link paths is printed to the screen.
#'
#' Does not depend on sampling strategy.
#'
#' The input microwave link data do not have to be sorted chronologically. Full-duplex links will give two data entries, these will both be used.
#'
#' Data availability is computed over the number of time intervals in the dataset as obtained from DateTime.
#' In case a time interval is missing, i.e. not present in DateTime, it is not included in the computations.
#' Hence, the computed availability does only refer to the time intervals for which a DateTime entry is present in the dataset.
#' In the computation of availabilities only links or paths are counted as available in case the link-derived rainfall intensities
#' are equal to or larger than 0 (mm h\eqn{^{-1}}). I.e., NA values in Rmean are taken into account in the computation of data availability.
#' So data availability is always computed over all time intervals in DateTime. Note that NA values do not exist in the DateTime object if function
#' ''PreprocessingMinMaxRSL'' has been run and note that Data must be preprocessed, because Rmean is used.
#'
#' @param Data Data frame with microwave link data (use data(Linkdata) to load example data).
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of ''cex'' in file FigNameTimeseriesAvailability. 
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of ''cex'' in file FigNameTimeseriesAvailability.
#' @param FigNameBarplotAvailabilityLinks Name of file with bar plot with percentage of links for bins of link orientation. The extension must be ''.pdf''.
#' @param FigNameBarplotAvailabilityLinkPaths Name of file with bar plot with percentage of links for bins of link path length. The extension must be ''.pdf''.
#' @param FigNameTimeseriesAvailability Name of file with bar plot with percentage of links for bins of microwave frequency. The extension must be ''.pdf''.
#' @param ps integer; the point size of text (but not symbols) in file FigNameTimeseriesAvailability. 
#' @param Rmean Vector of link-derived rainfall intensities (mm h\eqn{^{-1}}) with length equal to Data.
#' @param TimeZone Time zone of data (e.g. "UTC").
#' @return Figures with data availability of commercial microwave link network.
#' @export DataAvailability
#' @examples
#' data(Linkdata)
#' DataAvailability(Data=Linkdata,cex.axis=0.9,cex.lab=1.15,
#' FigNameBarplotAvailabilityLinks="Barplot_Availability_Links.pdf",
#' FigNameBarplotAvailabilityLinkPaths="Barplot_Availability_LinkPaths.pdf",
#' FigNameTimeseriesAvailability="TimeseriesAvailability.pdf",ps=18,Rmean=Rmean,TimeZone="UTC")
#' @author Aart Overeem
#' @references ''ManualRAINLINK.pdf''
#'
#' Overeem, A., Leijnse, H., and Uijlenhoet, R., 2016: Retrieval algorithm for rainfall mapping from microwave links in a 
#' cellular communication network, Atmospheric Measurement Techniques, 9, 2425-2444, https://doi.org/10.5194/amt-9-2425-2016.


DataAvailability <- function(Data,cex.axis,cex.lab,FigNameBarplotAvailabilityLinks,FigNameBarplotAvailabilityLinkPaths,FigNameTimeseriesAvailability,ps,Rmean,TimeZone="UTC",verbose=TRUE)
{

	# Bar plot data availability per sub-link.
  	# Set link IDs and time intervals
	Data$ID <- as.character(Data$ID)
   	IDLink <- unique(Data$ID)
   	N_links <- length(IDLink)

	# Compute number of time intervals in dataset.
	t <- sort(unique(Data$DateTime))
	N_t <- length(t)
	
	# Compute availability per sub-link.
	PercAvail <- c(NA)
        for (i in 1:N_links)
	{
		cond <- which(Data$ID==IDLink[i] & Rmean>=0)	
		PercAvail[i] <- length(cond) / N_t * 100
	}


	StepPercAvail <- 10
	pdf(FigNameBarplotAvailabilityLinks,family="Times")
	par(pty="s")
	par(ps=24)
	par(mar=c(5,5,1,1)+0.1)

	PercAvaildata <- c(NA)
	NrAvailClass <- 0
	for (availclass in seq(StepPercAvail,100,StepPercAvail))
	{
		NrAvailClass <- NrAvailClass + 1
		if (availclass==StepPercAvail)
		{
			# This is done to make sure that 0 availability is also shown in the graph.
			PercAvaildata[NrAvailClass] <- length(PercAvail[PercAvail>=(availclass-StepPercAvail)&PercAvail<=availclass])
		}
		if (availclass>StepPercAvail)
		{
			PercAvaildata[NrAvailClass] <- length(PercAvail[PercAvail>(availclass-StepPercAvail)&PercAvail<=availclass])
		}
	}
	total=sum(PercAvaildata)
	perc=100*PercAvaildata/total 

	ylimbarplot <- c(0,max(perc))
	barplot(perc,xlab = "Data availability sub-links (%)", ylab = "Percentage",
	xaxt="n",tcl=.5,ylim=ylimbarplot,cex.lab=1.3)
	at <- 0.1
	axis(side = 1, at = at, labels = 0, cex.axis = 0.6)
	for (availclass in seq(StepPercAvail,100,StepPercAvail))
	{
		at <- at + 1.2
		axis(side = 1, at = at, labels = availclass, cex.axis = 0.6) 	
	}
        legend("topleft",legend=substitute("Mean" == value*"%",list(value=formatC(mean(PercAvail), format="f", digits=2))),bty="n",cex=1)
	dev.off()




	# Bar plot data availability per link path.
  	# Set link IDs and time intervals
	# Determine coordinates of middle of link:
	X_middleLinks <- (Data$XStart + Data$XEnd) / 2
	Y_middleLinks <- (Data$YStart + Data$YEnd) / 2  

	# Determine unique middle of links:
	Coord_dataf <- data.frame(cbind(X_middleLinks, Y_middleLinks))
	Coord_uniq <- unique(Coord_dataf)
   	N_linkpaths <- length(Coord_uniq[,1])

	# Compute number of time intervals in dataset.
	t <- sort(unique(Data$DateTime))
	N_t <- length(t)
	
	# Compute availability per link path:
	PercAvail <- c(NA)
        for (i in 1:N_linkpaths)
	{
		cond <- which(X_middleLinks==Coord_uniq[i,1] & Y_middleLinks==Coord_uniq[i,2] & Rmean>=0)
		# Determine number of intervals with data, i.e. where the link path has at least 1 observation of Rmean>=0:
		NumberIntervals <- length(unique(Data$DateTime[cond]))
		PercAvail[i] <- NumberIntervals / N_t * 100
	}

	StepPercAvail <- 10
	pdf(FigNameBarplotAvailabilityLinkPaths,family="Times")
	par(pty="s")
	par(ps=24)
	par(mar=c(5,5,1,1)+0.1)

	PercAvaildata <- c(NA)
	NrAvailClass <- 0
	for (availclass in seq(StepPercAvail,100,StepPercAvail))
	{
		NrAvailClass <- NrAvailClass + 1
		if (availclass==StepPercAvail)
		{
			# This is done to make sure that 0 availability is also shown in the graph.
			PercAvaildata[NrAvailClass] <- length(PercAvail[PercAvail>=(availclass-StepPercAvail)&PercAvail<=availclass])
		}
		if (availclass>StepPercAvail)
		{
			PercAvaildata[NrAvailClass] <- length(PercAvail[PercAvail>(availclass-StepPercAvail)&PercAvail<=availclass])
		}

	}
	total=sum(PercAvaildata)
	perc=100*PercAvaildata/total 

	ylimbarplot <- c(0,max(perc))
	barplot(perc,xlab = "Data availability link paths (%)", ylab = "Percentage",
	xaxt="n",tcl=.5,ylim=ylimbarplot,cex.lab=1.3)
	at <- 0.1
	axis(side = 1, at = at, labels = 0, cex.axis = 0.6)
	for (availclass in seq(StepPercAvail,100,StepPercAvail))
	{
		at <- at + 1.2
		axis(side = 1, at = at, labels = availclass, cex.axis = 0.6) 	
	}
        legend("topleft",legend=substitute("Mean" == value*"%",list(value=formatC(mean(PercAvail), format="f", digits=2))),bty="n",cex=1)
	dev.off()




	# Plot with average number of sub-links and link paths for each time interval of the sampling strategy.
	UniqueTimestamp <- sort(unique(Data$DateTime))
	# Determine for each unique timestamp the number of sub-links with path-average rainfall intensities.
	NumberSubLinks <- NumberPaths <- c(NA)
	s <- 0
	for (Timestamp in UniqueTimestamp)
	{
	   	s <- s + 1
		cond <- which(Data$DateTime==Timestamp & Rmean>=0)
		NumberSubLinks[s] <- length(cond)

		# Determine coordinates of middle of link:
		X_middleLinks <- (Data$XStart[cond] + Data$XEnd[cond]) / 2
		Y_middleLinks <- (Data$YStart[cond] + Data$YEnd[cond]) / 2  

		# Determine unique middle of links:
		Coord_dataf <- data.frame(cbind(X_middleLinks, Y_middleLinks))
		Coord_uniq <- unique(Coord_dataf)

		# Compute average number of unique link paths:
		NumberPaths[s] <- (length(Coord_uniq[, 1]))
	}


	Timestamp_data = strptime(UniqueTimestamp,"%Y%m%d%H%M",tz=TimeZone)
	pdf(FigNameTimeseriesAvailability,width=13.2,height=4.4,family="Times")
	par(mar=c(3,3.8,1.8,0.1))
	par(ps=ps)

	plot(Timestamp_data,NumberPaths,xlab="",ylab="",type="p",ylim=c(0,max(NumberSubLinks)),mgp=c(1.8,0.5,0),xaxt="n",yaxt="n",lwd=1,lty=1,col="gray",pch=16,cex=0.4)
	par(new=TRUE)
	plot(Timestamp_data,NumberSubLinks,xlab="Date",ylab="Mean number per interval",type="p",ylim=c(0,max(NumberSubLinks)),cex.lab=cex.lab,cex.axis=cex.axis,col="black",mgp=c(1.8,0.5,0),lwd=1,lty=1,pch=16,cex=0.4)
	legend("bottomright",legend=c("Sub-links","Link paths"),col=c("black","gray"),pch=c(16,16),bty="n",cex=1.15)
	dev.off()
	print(paste("Average number of sub-links:",mean(NumberSubLinks)))
	print(paste("Average number of link paths:",mean(NumberPaths)))
	# A missing interval means that no timestamp and associated number of link (paths) is available, i.e. also no "NA" value is provided.
	# In such a case, the other intervals are correctly plotted, because the horizontal axis is an object of class "POSIXlt" (a date time object).

}



