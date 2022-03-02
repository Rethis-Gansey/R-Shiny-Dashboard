# R-Shiny-Dashboard

# Context
The health crisis caused by the spread of COVID19 requires the mobilization of various fields of 
expertise to mitigate its effects. From a statistical point of view, data visualization is a very important step in modeling the phenomenon. 

# Objective
The objective of this project is to report in a dynamic and automatic way the evolution of COVID19 in Benin and to make small comparisons with some of its neighbouring countries. 
We also represent with the help of interactive maps the situation in terms of incidence and mortality rates in the world and in some regions of Africa. 

## Table of contents
### Benin
- Confirmed cases
- Deceased cases
- Incident Rate
- Mortality Rate
- World map : by All world, Africa zone and by Incident and Mortality Rate
### Evolution of Covid 19 in Benin
- Time series evolution by Confirmed cases and Deceased cases on differents graphs
- Time series evolution by Confirmed cases and Deceased cases in same graph
### Evolution of Covid 19 in Benin and neighbouring countries
- Time series evolution in Benin, Burkina Faso, Togo and Niger by Confirmed and Deceased cases
### About
- Data source description

# Methodology
## Language
The Rmarkdown language was used for the realization of the Dashboard.
## Databases used
The data used comes from the COVID19 visualization data repository (GitHub repository) (which contains information on almost every country in the world) operated by 
the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). This repository is also supported by the ERSI Living Atlas team and the Johns Hopkins 
University Applied Physics Laboratory (JHU APL). The data contained in this repository are extracted from several authentic sources including: the World Health Organization (WHO) 
website, the European Centre for Disease Prevention and Control, the National Health Commission of the People's Republic of China and many other countries. 
These data are regularly updated.

### Field description

- Country_Region: Country, region or sovereignty name. The names of locations included on the Website correspond with the official designations used by the U.S. Department of State.
- Last Update: MM/DD/YYYY HH:mm:ss (24 hour format, in UTC).
Lat and Long_: Dot locations on the dashboard. All points (except for Australia) shown on the map are based on geographic centroids, and are not representative of a specific address, building or any location at a spatial scale finer than a province/state. Australian dots are located at the centroid of the largest city in each state.
- Confirmed: Counts include confirmed and probable (where reported).
- Deaths: Counts include confirmed and probable (where reported).
- Recovered: Recovered cases are estimates based on local media reports, and state and local reporting when available, and therefore may be substantially lower than the true number. US state-level recovered cases are from COVID Tracking Project. We stopped to maintain the recovered cases (see Issue #3464 and Issue #4465).
- Active: Active cases = total cases - total recovered - total deaths. This value is for reference only after we stopped to report the recovered cases (see Issue #4465)
- Incident_Rate: Incidence Rate = cases per 100,000 persons.
- Case_Fatality_Ratio (%): Case-Fatality Ratio (%) = Number recorded deaths / Number cases.
- All cases, deaths, and recoveries reported are based on the date of initial report. Exceptions to this are noted in the "Data Modification" and "Retrospective reporting of (probable) cases and deaths" subsections below.

For more information on the data sources, you can consult directly https://github.com/CSSEGISandData/COVID-19


## Design of the application
In the source code of the realized application, the data are directly loaded from the above mentioned repository, which allows an automatic update without any external manipulation. A filter was used to extract only data from Benin and some of its neighboring countries. Details on the codes used are available in the script used for the design.
## Deployment
The application has been deployed on the Shinyapps.io server and can be viewed via this link: https://rethisgansey.shinyapps.io/R-Shiny-Dashboard-main/

