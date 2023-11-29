# RatesOfRecentChange

### About
The 'Rates of Recent Change' repository includes all processed data, scripts and output of this scientific paper: 

Boonman, C.C.F.,Josep M. Serra-Diaz, Selwyn Hoeks, Wen-Yong Guo, Brian J. Enquist, Brian Maitner, Yadvinder Malhi, Cory Merow, Robert Buitenwerf, Jens-Christian Svenning, (2023). More than 17,000 tree species are at risk from rapid global change. In Review. 

- Species information is available as global files, where species can be found by ID. Species occurrence records can be downloaded from FigShare: https://doi.org/10.6084/m9.figshare.24168297.v1 Species’ extents can be downloaded here as spatial polygons: Global.polygon.shp - please note that waterbodies nor unsuitable climate zones are removed from these polygons. Additional species information is available in AOO.txt (area of occupancy per species) and NrOcc.txt (number of occurrences per species).
    
- Candidate species for prioritization for each included threat, small range, outliers, line polygons and all combined can be downloaded from Prioritization95.txt, where the number 95 indicates the threshold used to make these list. The different lists are indicated by the name of the threat. All prioritization lists include two columns: 1. Scientific species names, 2. IUCN Red List conservation status groups (see below; data retrieved on the 12th of January 2023). The geographic distribution for these candidate species in the form of .tif files per threat and the global species richness can be requested.
      
- All the R code that was used to process the data and create the figures are available numbered by the order of use:
    - Files to get the data, of which some need functions that are also provided
      * 01_GetMask.R
      * 02_GetOccClimRegions.R
      * 03_GetGeeData.R
      * 04_GetChelsaData.R
      * 05_ExtractSpData.R
      * 06_ExtractSpDataEnvChelsa.R
      * 07_CombineResults.R
      * 08_PlotPolyMethods.R
      * 09_SpeciesRichRaster.R
      * 10_SpeciesListOccRaster.R

    - Files to create the results
      * 1.Figures.R which requires PlottingFunctions.R
      * 2.Create.Prioritization.Lists.R
      * 3.Numbers.R which produces all the numbers that are mentioned in the paper
      * 4.FiguresGlobalMaps.R which maps all the prioritization species densities
      * 5.RRC.too.small.sp.R which produces the numbers and figures for the seperate analysis on species with extremely small extents
        
- Rates of recent change values per threat per species can be downloaded from RatesOfRecentChange.csv This .csv file contains all processed data used in this study, and is saved with comma seperator, dot decimal, including header. Each row (n=32,090) is a species and each column (n=27) contains different information. For all columns identified by 'slope', the mblm slopes are set to 0 when p-value >0.05.
    - The IUCN statuses as provided here are a recategorized from the official IUCN statuses into conservation status group , as follows: 
    "LR/lc" "LR/nt" "LR/cd" "NT" "LC" = "NotThreatened", 
    "CR" "EN" "EW" "EX" = "Threatened", 
    "VU" = "Vulnerable", 
    "DD" = "DD" (where DD stands for data deficient), 
    no data = "NotEvaluated"
    - Columns names and units:
      * 1.'species' species names, bound by _
      * 2.'eoo_km2' species extent size, with the unit m2 (to convert to km2, divide the EOO by 1,000,000)
      * 3.'IUCNstatus' IUCN conservation status groups: Threatened, Vulnerable, NotThreatened, DD (data deficient), NotEvaluated based on IUCN statuses retrieved on the 12th of January 2023
      * 4.'Assess_year' year of IUCN assessment per species retrieved on the 12th of January 2023
      * 5.'slope_fire' rate of burned area change over the entire species' extent for the years 2000-2020, with the unit % of species' extent area per year
      * 6.'rate_deforestation' rate of deforestation over the entire species' extent for the years 2000-2020, with the unit % of species' extent area per year
      * 7.'rate_cropland' rate of cropland expansion over the entire species' extent for the years 2000-2020, with the unit % of species' extent area per year
      * 8.'rate_urban' rate of build-up increase over the entire species' extent for the years 2000-2020, with the unit % of species' extent area per year
      * 9.'rate_treecover_change_area' rate of tree cover change over the entire species' extent for the years 2000-2020, with the unit % of species' extent area per year
      * 10.'slope_vpd_sd' rate of vapour pressure deficit seasonality (standard deviation) change over the entire species' extent for the years 2000-2020, with the unit Pa per year
      * 11.'slope_vpd' rate of vapour pressure deficit change over the entire species' extent for the years 2000-2020, with the unit Pa per year
      * 12.'slope_prec_sd' rate of precipitation seasonality (standard deviation) change over the entire species' extent for the years 2000-2020, with the unit mm per year
      * 13.'slope_prec' rate of precipitation change over the entire species' extent for the years 2000-2020, with the unit mm per year
      * 14.'slope_tmin' rate of minimum temperature change over the entire species' extent for the years 2000-2020, with the unit °C per year
      * 15.'slope_tmax' rate of maximum temperature change over the entire species' extent for the years 2000-2020, with the unit °C per year
      * 16.'slope_vpd_sd0010' rate of vapour pressure deficit seasonality (standard deviation) change over the entire species' extent for the years 2000-2010, with the unit Pa per year
      * 17.'slope_vpd0010' rate of vapour pressure deficit change over the entire species' extent for the years 2000-2010, with the unit Pa per year
      * 18.'slope_prec_sd0010' rate of precipitation seasonality (standard deviation) change over the entire species' extent for the years 2000-2010, with the unit mm per year
      * 19.'slope_prec0010' rate of precipitation change over the entire species' extent for the years 2000-2010, with the unit mm per year
      * 20.'slope_tmin0010' rate of minimum temperature change over the entire species' extent for the years 2000-2010, with the unit °C per year
      * 21.'slope_tmax0010' rate of maximum temperature change over the entire species' extent for the years 2000-2010, with the unit °C per year
      * 22.'slope_vpd_sd1020' rate of vapour pressure deficit seasonality (standard deviation) change over the entire species' extent for the years 2010-2020, with the unit Pa per year
      * 23.'slope_vpd1020' rate of vapour pressure deficit change over the entire species' extent for the years 2010-2020, with the unit Pa per year
      * 24.'slope_prec_sd1020' rate of precipitation seasonality (standard deviation) change over the entire species' extent for the years 2010-2020, with the unit mm per year
      * 25.'slope_prec1020' rate of precipitation change over the entire species' extent for the years 2010-2020, with the unit mm per year
      * 26.'slope_tmin1020' rate of minimum temperature change over the entire species' extent for the years 2010-2020, with the unit °C per year
      * 27.'slope_tmax1020' rate of maximum temperature change over the entire species' extent for the years 2010-2020, with the unit °C per year
    






