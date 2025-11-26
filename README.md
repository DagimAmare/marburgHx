# Marburg Virus Outbreaks History Interactive Map

## Overview
This interactive Shiny application visualizes historical Marburg virus outbreaks worldwide. Users can explore outbreak data by selecting different countries and time periods to see:
- Interactive map highlighting affected regions
- Outbreak statistics (infections, deaths, case fatality rates)
- Detailed information about each outbreak
- Source links for verification

## Requirements
- R (version 4.0 or higher recommended)
- Required R packages:
  - shiny
  - leaflet
  - dplyr
  - tidyr

## Installation

1. Install R from [CRAN](https://cran.r-project.org/)

2. Install required packages in R:
```r
install.packages(c("shiny", "leaflet", "dplyr", "tidyr"))
```

## Running the App

### Option 1: From RStudio
1. Open `app.R` in RStudio
2. Click the "Run App" button at the top of the editor
3. The app will launch in a new window or your default browser

### Option 2: From R Console
```r
library(shiny)
runApp("path/to/app.R")
```

### Option 3: From Command Line
```bash
R -e "shiny::runApp('app.R')"
```

## Data Source
The app uses the `marburg_virus_outbreaks_complete.csv` file, which contains comprehensive data on Marburg virus outbreaks from 1967 to 2025.


## License
This visualization tool is for educational and informational purposes only.
