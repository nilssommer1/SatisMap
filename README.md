#SatisMap
The project SatisMap is an interactive web application mapping and displaying various measures of well-being in Switzerland. With the data from the Swiss Household Panel [1], the application displays average levels of well-being in psychological, sociological, and medical domains per Canton for the year 2020. Moreover, SatisMap displays the associations of various demographical and well-being variables. 

##Project Report
In the Project Report, ...

##Folder 'satipmap'
This folder contains the main application code (R Shiny) and preprocessed data. 'SHP20_P.Rda' contains the data from the Swiss Household Panel [1], reduced to the focus variables for the project and with some changes to the data structure of some variables. 'varkey.Rda' contains iformation for each variable to be displayed in SatisMap. 'cantons2.Rda' contains the geospatial data necessary for plotting.

##References
[1] Swiss Household Panel; Lausanne: FORS (https://forscenter.ch/projekte/swiss-household-panel/)