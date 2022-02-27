# R-Shiny-Dashboard

# Context
The health crisis caused by the spread of COVID19 requires the mobilization of various fields of 
expertise to mitigate its effects. From a statistical point of view, data visualization is a very important step in modeling the phenomenon. 

# Objective
The objective of this project is to report in a dynamic and automatic way the evolution of COVID19 in Benin and to make small comparisons with some of its neighbouring countries. 
We also represent with the help of interactive maps the situation in terms of incidence and mortality rates in the world and in some regions of Africa. 

# Methodology
## Language
The Rmarkdown language was used for the realization of the Dashboard.
## Databases used
The data used comes from the COVID19 visualization data repository (GitHub repository) (which contains information on almost every country in the world) operated by 
the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). This repository is also supported by the ERSI Living Atlas team and the Johns Hopkins 
University Applied Physics Laboratory (JHU APL). The data contained in this repository are extracted from several authentic sources including: the World Health Organization (WHO) 
website, the European Centre for Disease Prevention and Control, the National Health Commission of the People's Republic of China and many other countries. 
These data are regularly updated.
## Design of the application
In the source code of the realized application, the data are directly loaded from the above mentioned repository, which allows an automatic update without any external manipulation. A filter was used to extract only data from Benin and some of its neighboring countries. Details on the codes used are available in the script used for the design.
## Deployment
The application has been deployed on the Shinyapps.io server and can be viewed via this link: 

