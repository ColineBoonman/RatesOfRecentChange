# RatesOfRecentChange

### About
The 'Rates of Recent Change' repository includes all processed data, scripts and output of this scientific paper: 

Boonman, C.C.F.,Josep M. Serra-Diaz, Selwyn Hoeks, Wen-Yong Guo, Brian J. Enquist, Brian Maitner, Yadvinder Malhi, Cory Merow, Robert Buitenwerf, Jens-Christian Svenning, (2023). More than 17,000 tree species are at risk from rapid global change. In Review. 

- Species’ extents can be downloaded as spatial polygons 
- Rates of recent change values per threat per species can be downloaded from RatesOfRecentChange.csv
- Candidate species for prioritization for each included threats, small range, outliers and all combined can be downloaded from Prioritization95.txt, where the number 95 indicates the threshold used to make these list. The different lists are indicated by the name of the threat.
- All the R code that was used to process the data and create the figures are available numbered by the order of use:
  * 0.GetDataGEE.R
  * 1.TestRRCdata.R
  * 2.Create.Dataset.R
  * 3.Figures.R which requires PlottingFunctions.R
  * 4.Numbers.R which produces all the numbers that are mentioned in the paper
  * 5.Create.Prioritization.Lists.R
  * 6.Plot.GlobalMap.PriorLists.R which maps all the prioritization species densities
  * 7.RRC.too.small.sp.R which produces the numbers and figures for the seperate analysis on species with extremely small extents or species that were defined to have outlier RRC values
