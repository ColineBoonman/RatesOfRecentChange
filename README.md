# RatesOfRecentChange

### About
The 'Rates of Recent Change' repository includes all processed data, scripts and output of this scientific paper: 

Boonman, C.C.F.,Josep M. Serra-Diaz, Selwyn Hoeks, Wen-Yong Guo, Brian J. Enquist, Brian Maitner, Yadvinder Malhi, Cory Merow, Robert Buitenwerf, Jens-Christian Svenning, (2023). More than 17,000 tree species are at risk from rapid global change. In Review. 

- Species’ extents can be downloaded as spatial polygons - please note that water nor unsuitable climate zones are removed from these polygons
- Rates of recent change values per threat per species can be downloaded from RatesOfRecentChange.csv
- Candidate species for prioritization for each included threat, small range, outliers, line polygons and all combined can be downloaded from Prioritization95.txt, where the number 95 indicates the threshold used to make these list. The different lists are indicated by the name of the threat. The geographic distribution for these candidate species in the form of .tif files per threat and the global species richness can be requested to the corresponding author.
- All the R code that was used to process the data and create the figures are available numbered by the order of use:
  * 0.GetDataGEE.R
  * 1.Figures.R which requires PlottingFunctions.R
  * 2.Create.Prioritization.Lists.R
  * 3.Numbers.R which produces all the numbers that are mentioned in the paper
  * 4.FiguresGlobalMaps.R which maps all the prioritization species densities
  * 5.RRC.too.small.sp.R which produces the numbers and figures for the seperate analysis on species with extremely small extents
