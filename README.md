# RatesOfRecentChange

### About
The 'Rates of Recent Change' repository includes all processed data, scripts and output of this scientific paper: 

Boonman, C.C.F. & Svenning, J.-C. (2023). HomeRange: A global database of mammalian home ranges. Global Ecology and Biogeography. https://doi.org/10.1111/geb.13625.

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
