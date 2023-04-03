# RatesOfRecentChange

### About
The 'Rates of Recent Change' repository includes all processed data, scripts and output of this scientific paper: 

Broekman, M.J.E., Hoeks, S., Freriks, R., Langendoen, M.M., Runge, K.M., Savenco, E., ter Harmsel, R., Huijbregts, M.A.J. & Tucker, M.A. (2022). HomeRange: A global database of mammalian home ranges. Global Ecology and Biogeography. https://doi.org/10.1111/geb.13625.

- Species’ extents can be downloaded as spatial polygons 
- Rates of recent change values per threat per species can be downloaded from RatesOfRecentChange.csv
- Candidate species for prioritization for each included threats, small range, outliers and all combined can be downloaded from Prioritization95.txt, where the number 95 indicates the threshold used to make these list. The different lists are indicated by the name of the threat.
- The GEE code that was used to calculate the rate of recent change values is available: 0.GeeGetData_2022_09_28.R
- All the R code that was used to process the data and create the figures are available numbered by the order of use:
  1a.RRCtests.R
  1b.Create.Dataset.R
  2.Figures.R which requires PlottingFunctions.R
  2.FiguresVennDiagram.R
  3.ChangingWindow.R
  3.Numbers.R which produces all the numbers that are mentioned in the paper
  4.Create.Prioritization.Lists.R
  5.Plot.toosmallrangeSpecies.R
  6.Plot.GlobalMap.PriorLists.R
