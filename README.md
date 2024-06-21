RAINLINK: Retrieval algorithm for rainfall mapping from microwave links in a cellular communication network.
------

This software can be employed to obtain rainfall estimates from signal level data from commercial microwave links (CMLs) in cellular communication networks.

RAINLINK versions 1.3x work with new(er) versions of proj4 and gdal libraries and works in principle as of R version 4. Note that RAINLINK version 1.3 has been tested on 3 months of CML data from over 1500 link paths over the Netherlands. Differences in rainfall estimates are small compared to those obtained with RAINLINK version 1.21. The old version 1.21 remains available on GitHub (https://github.com/overeem11/RAINLINK/tree/v.1.21; https://doi.org/10.5281/zenodo.5907524). RAINLINK version 1.3 does not contain any visualization functions anymore. Note that the plotting of data availability (''DataAvailability.R'') and topology (''Topology.R'') have been kept. Python software MapRAINLINK is now publicly available to visualize rain gauge, radar, and commercial microwave link locations and their rainfall estimates on a map (https://github.com/overeem11/MapRAINLINK). It also contains a recipe to obtain interpolation grids for RAINLINK.

See documents ManualRAINLINK.pdf and RAINLINK1.31.pdf for extensive descriptions of RAINLINK and how to use it. Note that the date and time in RAINLINK output is the end date and end time of observation, also in output files with CML path averages or interpolated values.

When referring to RAINLINK, please use:

Overeem, A., Leijnse, H., and Uijlenhoet, R.: Retrieval algorithm for rainfall mapping from microwave links in a cellular communication network, Atmos. Meas. Tech., 9, 2425–2444, https://doi.org/10.5194/amt-9-2425-2016, 2016.

Aart Overeem, Hidde Leijnse, Lotte de Vos, & Micha Silver. (2024). RAINLINK (v.1.31). Zenodo. [https://doi.org/10.5281/zenodo.7473635]
