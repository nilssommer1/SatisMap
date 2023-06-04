# SatisMap
The project SatisMap is an interactive web application mapping and displaying various measures of well-being in Switzerland. With the data from the Swiss Household Panel [1], the application displays average levels of well-being in psychological and sociological domains per Canton for the year 2020. Moreover, SatisMap displays the associations of various demographical and well-being variables.  
The application was created using R [2] and shiny [3]. A running version can be found at https://nilssommer.shinyapps.io/satismap/.

### Project Report
In the Project Report, ...

### Folder 'satipmap'
This folder contains the main application code (R Shiny) and preprocessed data. 'SHP20_P.Rda' contains the data from the Swiss Household Panel [1], reduced to the focus variables for the project and with some changes to the data structure of some variables. 'varkey.Rda' contains iformation for each variable to be displayed in SatisMap. 'cantons2.Rda' contains the geospatial data necessary for plotting.

### References
[1] Swiss Household Panel; Lausanne: FORS (https://forscenter.ch/projekte/swiss-household-panel/)  
[2] The R Project for Statistical Computing (https://www.r-project.org/)   
[4] Shiny; Rstudio (https://shiny.rstudio.com/)   
