#Installing in a Debian based linux environment

1. first install R/ rStudio
2. Install following packages in R, use ``install.packages("package_name")``
	- sp
	- gstat
	- ggplot
	- ggmap
	- maps
	- mapproj
	- labeling
	- rgdal		
	    * here you will be stucked asking for a dependency called gdal-config.
3. install the dependency gdal-config by 
	``sudo apt install gdal-bin libgdal-dev``
	(This is not a R package )
4.  Install these R packages in R again
	- rgdal
	- digest
	- ncdf4
5. Now go to the directory where you download the rainlink.
6. Open terminal and start R.
7. Install rainlink package by,
	install.packages("absolute path to the RAINLINK_1.11.tar.gz", 
	type = source, repos = NULL)

8. All set to run the example code "Run.R" on example data set data/LinkData.RData.
	this LinkData.RData is a link status file with following attributes explaining the link.
	- Frequency
	- DateTime
	- Pmin : minimum power of the recieved signal level
	- PMax : maximum " " " "
	- PathLength : Length between towers
	- XStart : x position of the start tower (lang)
	- YStart : (lat )
	- XEnd
	- YEnd
	- ID : unique identifier for the link

	if you need to see this data use the opening_from_r.txt in data/ directory
9. Run the script "Run.R" to calculate the rainfall on above mentioned data set.
	``source("Run.R")``
10. Out puts will be available in 
	- Figures/
	- LinkPathRainDepths15min/
	- RainMapsLinks15min/


