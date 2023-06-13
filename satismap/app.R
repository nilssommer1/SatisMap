# Load packages
library(tidyverse)
library(DescTools)
library(shiny)
library(tmap)
library(shinythemes)
library(RColorBrewer)

# Load data
load("data/SHP20_P.Rda")
load("data/cantons2.Rda")
load("data/varkey.Rda")

# Functions & base settings
subsetdf <- function(ds, sex){
  if (sex == "Female"){
    ds %>%  
      filter(sexCat == "f")
  }
  else if (sex == "Male"){
    ds %>% 
      filter(sexCat == "m")
  } 
  else {
    ds %>% 
      filter(sexCat == "f" | sexCat == "m")
  }
}

my_colors <- RColorBrewer::brewer.pal(3, "YlOrBr")[2:3]

# User interface
# Display input options and outputs
ui <- shinyUI(navbarPage("SatisMap",
                         theme = shinytheme("united"),
                         
                   tabPanel("Averages", # Tab Averages
                            sidebarPanel( # Sidebar --> input and details
                              p("Create maps displaying well-being reports in Switzerland with Data from the Swiss Household Panel in 2020. 
                                       The average value for the chosen variable in each Canton is displayed.
                                       Check 'Show variable details' for more details."),
                              #inputs
                              selectInput("varT1", 
                                          label = "Choose variable to display",
                                          choices = c("Life Satisfaction", 
                                                      "Satisfaction with relationships (general)",
                                                      "Happiness with partner",
                                                      "Satisfaction with health",
                                                      "Satisfaction with studies",
                                                      "Satisfaction with finances",
                                                      "Satisfaction with job",
                                                      "Satisfaction with free time",
                                                      "Satisfaction with democracy",
                                                      "Negative emotions",
                                                      "Positive emotions",
                                                      "Women penalized",
                                                      "Own sex penalized",
                                                      "Importance of promotion of women",
                                                      "Education",
                                                      "Work status",
                                                      "Socioeconomic status",
                                                      "Individual income (year)",
                                                      "Household income (year)",
                                                      "Number of own kids"
                                                      ),
                                          selected = "Life Satisfaction"),
                              
                              selectInput("sex", 
                                          label = "Choose sex",
                                          choices = c("All",
                                                      "Female", 
                                                      "Male"),
                                          selected = "All"),
                              
                              sliderInput("age", 
                                          label = "Choose age range",
                                          min = 16, max = 103, value = c(16, 103)),
                              
                              checkboxInput("det",
                                            label = "Show variable details", value = F),
                              
                              #details
                              conditionalPanel(condition = "input.det == 1", 
                                               uiOutput("selected_var"),
                                               br(),
                                               uiOutput("varCH"),
                                               br(),
                                               plotOutput(outputId = "descdiagT1", width = "100%", height = 250)
                                               ),
                              
                              
                              width = 3),
                            # map
                            mainPanel(
                              tmapOutput(outputId = "meanmap", width = "100%", height = 800),
                              width = 9)
                   ),
                   tabPanel("Associations", # Tab Associations
                            sidebarPanel(# Sidebar --> input and details
                            p("Create maps displaying well-being reports in Switzerland with Data from the Swiss Household Panel in 2020. 
                                       The association (correlation) for the chosen variables in each Canton is displayed.
                                       Check 'Show variable details' for more details."),
                            #inputs
                                         selectInput("varT2", 
                                                     label = "Choose variable to display",
                                                     choices = c("Life Satisfaction", 
                                                                 "Satisfaction with relationships (general)",
                                                                 "Happiness with partner",
                                                                 "Satisfaction with health",
                                                                 "Satisfaction with studies",
                                                                 "Satisfaction with finances",
                                                                 "Satisfaction with job",
                                                                 "Satisfaction with free time",
                                                                 "Satisfaction with democracy",
                                                                 "Negative emotions",
                                                                 "Positive emotions",
                                                                 "Women penalized",
                                                                 "Own sex penalized",
                                                                 "Importance of promotion of women",
                                                                 "Education",
                                                                 "Work status",
                                                                 "Socioeconomic status",
                                                                 "Individual income (year)",
                                                                 "Household income (year)",
                                                                 "Number of own kids",
                                                                 "Sex",
                                                                 "Age"
                                                     ),
                                                     selected = "Life Satisfaction"),
                                         
                                         selectInput("cor", 
                                                     label = "Choose variable to associate with",
                                                     choices = c("Life Satisfaction", 
                                                                 "Satisfaction with relationships (general)",
                                                                 "Happiness with partner",
                                                                 "Satisfaction with health",
                                                                 "Satisfaction with studies",
                                                                 "Satisfaction with finances",
                                                                 "Satisfaction with job",
                                                                 "Satisfaction with free time",
                                                                 "Satisfaction with democracy",
                                                                 "Negative emotions",
                                                                 "Positive emotions",
                                                                 "Women penalized",
                                                                 "Own sex penalized",
                                                                 "Importance of promotion of women",
                                                                 "Education",
                                                                 "Work status",
                                                                 "Socioeconomic status",
                                                                 "Individual income (year)",
                                                                 "Household income (year)",
                                                                 "Number of own kids",
                                                                 "Sex",
                                                                 "Age"),
                                                     selected = "Age"),
                                         
                                         selectInput("sexT2", 
                                                     label = "Choose sex",
                                                     choices = c("All",
                                                                 "Female", 
                                                                 "Male"),
                                                     selected = "All"),
                                         
                                         sliderInput("ageT2", 
                                                     label = "Choose age range",
                                                     min = 16, max = 103, value = c(16, 103)),
                                         
                                         checkboxInput("detT2",
                                                       label = "Show variable details", value = F),
                            #details
                                         conditionalPanel(condition = "input.detT2 == 1",
                                                          uiOutput(outputId = "selected_cor"),
                                                          HTML("<b>If 'Missing':</b> Correlations could not be calculated (too few participants / no variance). <br>"),
                                                          br(),
                                                          uiOutput(outputId = "selected_varT2v"),
                                                          br(),
                                                          uiOutput(outputId = "selected_varT2c"),
                                                          br(),
                                                          uiOutput(outputId = "corCH"),
                                                          br(),
                                                          plotOutput(outputId = "descdiagT2", width = "100%", height = 250)
                                         ), width = 3),
                            #map
                            mainPanel(
                              tmapOutput(outputId = "cormap", width = "100%", height = 800),
                              width = 9)
                   ),
                   tabPanel("References", # Tab References
                            mainPanel(
                              HTML("SatisMap was created with <a href = https://www.r-project.org/> R (the R Project for Statistical Computing) </a> and 
                              the <a href = https://shiny.posit.co/> package Shiny </a>. 
                              Data displayed in this app was retrieved with permission from the <a href = https://forscenter.ch/projekte/swiss-household-panel/> Swiss Household Panel </a> 
                              and the <a href = https://www.bfs.admin.ch/bfs/en/home/services/geostat/swiss-federal-statistics-geodata/administrative-boundaries/generalized-boundaries-local-regional-authorities.assetdetail.22484210.html> Federal Statistical Office of Switzerland </a>. <br>
                                See <a href = https://github.com/nilssommer1/SatisMap> Github </a> for programming code and a more detailed description."),
                              width = 6)
                   )

))

# Server logic
 # calculations
server <- function(input, output) {
  
  # Reactive
  # create variables based on input
  
  meanmapData <- reactive({ #in Averages Tab
    SHP20_P %>% 
      mutate(
        var = case_when(
          input$varT1 == "Life Satisfaction" ~ SHP20_P$satLife,
          input$varT1 == "Satisfaction with relationships (general)" ~ SHP20_P$satRelat,
          input$varT1 == "Happiness with partner" ~  SHP20_P$happyPartner,
          input$varT1 == "Satisfaction with health" ~ SHP20_P$satHealth,
          input$varT1 == "Satisfaction with studies" ~ SHP20_P$satStudies,
          input$varT1 == "Satisfaction with finances" ~ SHP20_P$satFinances,
          input$varT1 == "Satisfaction with job" ~ SHP20_P$satJob,
          input$varT1 == "Satisfaction with free time" ~ SHP20_P$satFreetime,
          input$varT1 == "Satisfaction with democracy" ~ SHP20_P$satDemocracy,
          input$varT1 == "Negative emotions" ~ SHP20_P$negAff,
          input$varT1 == "Positive emotions" ~ SHP20_P$posAff,
          input$varT1 == "Women penalized" ~ SHP20_P$womenPenalized,
          input$varT1 == "Own sex penalized" ~ SHP20_P$ownSexPenalized,
          input$varT1 == "Importance of promotion of women" ~ SHP20_P$womenPromotion,
          input$varT1 == "Education" ~ SHP20_P$education,
          input$varT1 == "Work status" ~ SHP20_P$workStatus,
          input$varT1 == "Socioeconomic status" ~ SHP20_P$SES,
          input$varT1 == "Individual income (year)" ~ SHP20_P$incomeYearIndiv,
          input$varT1 == "Household income (year)" ~ SHP20_P$incomeYearHouse,
          input$varT1 == "Number of own kids" ~ SHP20_P$nrOwnKids)
        )
  })
  
  cormapData <- reactive({ #in Associations Tab
    SHP20_P %>% 
      mutate(
        var = case_when(input$varT2 == "Life Satisfaction" ~ SHP20_P$satLife,
                        input$varT2 == "Satisfaction with relationships (general)" ~ SHP20_P$satRelat,
                        input$varT2 == "Happiness with partner" ~  SHP20_P$happyPartner,
                        input$varT2 == "Satisfaction with health" ~ SHP20_P$satHealth,
                        input$varT2 == "Satisfaction with studies" ~ SHP20_P$satStudies,
                        input$varT2 == "Satisfaction with finances" ~ SHP20_P$satFinances,
                        input$varT2 == "Satisfaction with job" ~ SHP20_P$satJob,
                        input$varT2 == "Satisfaction with free time" ~ SHP20_P$satFreetime,
                        input$varT2 == "Satisfaction with democracy" ~ SHP20_P$satDemocracy,
                        input$varT2 == "Negative emotions" ~ SHP20_P$negAff,
                        input$varT2 == "Positive emotions" ~ SHP20_P$posAff,
                        input$varT2 == "Women penalized" ~ SHP20_P$womenPenalized,
                        input$varT2 == "Own sex penalized" ~ SHP20_P$ownSexPenalized,
                        input$varT2 == "Importance of promotion of women" ~ SHP20_P$womenPromotion,
                        input$varT2 == "Education" ~ SHP20_P$education,
                        input$varT2 == "Work status" ~ SHP20_P$workStatus,
                        input$varT2 == "Socioeconomic status" ~ SHP20_P$SES,
                        input$varT2 == "Individual income (year)" ~ SHP20_P$incomeYearIndiv,
                        input$varT2 == "Household income (year)" ~ SHP20_P$incomeYearHouse,
                        input$varT2 == "Number of own kids" ~ SHP20_P$nrOwnKids,
                        input$varT2 == "Sex" ~ SHP20_P$sex,
                        input$varT2 == "Age" ~ SHP20_P$age),
        correlator = case_when(input$cor == "Life Satisfaction" ~ SHP20_P$satLife,
                               input$cor == "Satisfaction with relationships (general)" ~ SHP20_P$satRelat,
                               input$cor == "Happiness with partner" ~  SHP20_P$happyPartner,
                               input$cor == "Satisfaction with health" ~ SHP20_P$satHealth,
                               input$cor == "Satisfaction with studies" ~ SHP20_P$satStudies,
                               input$cor == "Satisfaction with finances" ~ SHP20_P$satFinances,
                               input$cor == "Satisfaction with job" ~ SHP20_P$satJob,
                               input$cor == "Satisfaction with free time" ~ SHP20_P$satFreetime,
                               input$cor == "Satisfaction with democracy" ~ SHP20_P$satDemocracy,
                               input$cor == "Negative emotions" ~ SHP20_P$negAff,
                               input$cor == "Positive emotions" ~ SHP20_P$posAff,
                               input$cor == "Women penalized" ~ SHP20_P$womenPenalized,
                               input$cor == "Own sex penalized" ~ SHP20_P$ownSexPenalized,
                               input$cor == "Importance of promotion of women" ~ SHP20_P$womenPromotion,
                               input$cor == "Education" ~ SHP20_P$education,
                               input$cor == "Work status" ~ SHP20_P$workStatus,
                               input$cor == "Socioeconomic status" ~ SHP20_P$SES,
                               input$cor == "Individual income (year)" ~ SHP20_P$incomeYearIndiv,
                               input$cor == "Household income (year)" ~ SHP20_P$incomeYearHouse,
                               input$cor == "Number of own kids" ~ SHP20_P$nrOwnKids,
                               input$cor == "Sex" ~ SHP20_P$sex,
                               input$cor == "Age" ~ SHP20_P$age))
    })
  
  #Calculations in Averages Tab
  #map
  output$meanmap <- renderTmap({
    meanmapData <- meanmapData() %>%
      subsetdf(sex = input$sex) %>% 
      filter(age >= input$age[1] & age <= input$age[2]) %>% 
      group_by(canton) %>% 
      summarize(var = mean(var, na.rm = T) %>% round(2),
                ncases = n())
    meanmapData <- merge(cantons2, meanmapData, by = 'canton', all.y = T)


    tm_shape(meanmapData) + 
      tm_polygons(col = "var", 
                  title = input$varT1, 
                  popup.vars = c("Average" = "var", "Number of cases" = "ncases"))
  })
  
  #details
  output$varCH <- renderUI({ 
      meanCH <- meanmapData() %>% 
      subsetdf(sex = input$sex) %>% 
      filter(age >= input$age[1] & age <= input$age[2]) %>% 
      drop_na(var) %>% 
      summarize(var = mean(var, na.rm = T) %>% round(2),
                ncases = n())
    
      str1 <- paste0("<b>Average Value for all of Switzerland:</b> ", meanCH$var)
      str2 <- paste0("<b>Number of Cases:</b> ", meanCH$ncases)
      HTML(paste(str1, str2, sep = '<br/>')) 
    
    
  })
  
  output$selected_var <- renderUI({ 
    varkeyT1 <- varkey %>% 
      filter(var == input$varT1)
    
      str1 <- paste0("<b>", input$varT1, "</b>")
      str2 <- paste0("<b>Question:</b> ", varkeyT1$question)
      str3 <- paste0("<b>Answer Range:</b> ", varkeyT1$score)
      HTML(paste(str1, str2, str3, sep = '<br/>')) 
    
  })
  
  # Descriptive Plot
  output$descdiagT1 <- renderPlot({ 
    
    meanmapData <- meanmapData() %>% 
      subsetdf(sex = input$sex) %>% 
      filter(age >= input$age[1] & age <= input$age[2]) %>% 
      drop_na(var)
    
    plot <- ggplot(data = meanmapData, aes(x = var)) +
      geom_bar(color = my_colors[2] , fill = my_colors[2]) +
      labs(x = input$varT1, y = "Number of Participants") +
      ggtitle(paste0("Distribution of ", input$varT1, "\n(Switzerland)")) +
      theme_classic()
    plot
  })
  
  #Calculations in Associations Tab
  #map
  output$cormap <- renderTmap({
   
    cormapData <- cormapData() %>% 
      subsetdf(sex = input$sexT2) %>%  
      filter(age >= input$ageT2[1] & age <= input$ageT2[2]) %>%
      drop_na(var, correlator) %>% 
      group_by(canton) %>% 
      summarize(cor = SpearmanRho(var, correlator, conf.level = .95)[1] %>% round(2),
                confint1 = SpearmanRho(var, correlator, conf.level = .95)[2] %>% round(2),
                confint2 = SpearmanRho(var, correlator, conf.level = .95)[3] %>% round(2),
                ncases = n())
    cormapData <- merge(cantons2, cormapData, by = 'canton', all.y = T)
    
    
    tm_shape(cormapData) + 
      tm_polygons(col = "cor", 
                  title = paste("Correlation of", input$varT2, "and", input$cor, sep = " "), 
                  popup.vars = c("Correlation" = "cor", "95% CI lower" = "confint1", "95% CI upper" = "confint2", "Number of cases" = "ncases"),
                  style = "fixed",
                  breaks = c(-1, -0.5, -0.3, -0.1, 0.1, 0.3, 0.5, 1))
    
    
    })
  #details
   output$selected_cor <- renderUI({
     
     if(input$varT2 == input$cor) HTML(paste("<b>Note:</b> ", "You have selected the same Variable twice. This results in perfect correlations that have no meaning."))
     else if(input$varT2 == "Sex" & input$sexT2 != "All") HTML(paste("Note:</b> ", "It is not possible to split by sex when sex is also selected as a variable."))
     else if(input$cor == "Sex" & input$sexT2 != "All") HTML(paste("<b>Note:</b> ", "It is not possible to split by sex when sex is also selected as a variable."))
     else if(input$varT2 == "Sex" & input$cor != "Sex") HTML(paste("<b>Note:</b> ", "A correlation above zero means that compared to men, women report higher", input$cor, "."))
     else if(input$cor == "Sex" & input$varT2 != "Sex") HTML(paste("<b>Note:</b> ", "A correlation above zero means that compared to men, women report higher", input$varT2, "."))
     else HTML(paste0("<b>Note:</b> ", "A correlation above zero means that  the higher ", input$varT2, ", the higher ", input$cor, "."))
     })
   
   output$corCH <- renderUI({ 
       corCH <- cormapData() %>% 
       subsetdf(sex = input$sexT2) %>%
         filter(age >= input$ageT2[1] & age <= input$ageT2[2]) %>% 
       drop_na(var, correlator) %>% 
       summarize(cor = SpearmanRho(var, correlator, conf.level = .95)[1] %>% round(2),
                 confint1 = SpearmanRho(var, correlator, conf.level = .95)[2] %>% round(2),
                 confint2 = SpearmanRho(var, correlator, conf.level = .95)[3] %>% round(2),
                 ncases = n())
     
     str1 <- paste0("<b>Correlation for all of Switzerland:</b> ", corCH$cor)
     str2 <- paste0("<b>95% Confidence Interval:</b> [", corCH$confint1, ", ", corCH$confint2, "]" )
     str3 <- paste0("<b>Number of Cases:</b> ", corCH$ncases)
     HTML(paste(str1, str2, str3, sep = '<br/>')) 
     
     
   })
   
   output$selected_varT2v <- renderUI({
     varkeyT2v <- varkey %>% 
       filter(var == input$varT2)
     
     if (input$detT2 == T){
     str1 <- paste0("<b>", input$varT2, "</b>")
     str2 <- paste0("<b>Question:</b> ", varkeyT2v$question)
     str3 <- paste0("<b>Answer Range:</b> ", varkeyT2v$score)
     HTML(paste(str1, str2, str3, sep = '<br/>')) 

     }     
   })
   
   output$selected_varT2c <- renderUI({
     varkeyT2c <- varkey %>% 
       filter(var == input$cor)
     
     if (input$detT2 == T){
       str1 <- paste0("<b>", input$cor, "</b>")
       str2 <- paste0("<b>Question:</b> ", varkeyT2c$question)
       str3 <- paste0("<b>Answer Range:</b> ", varkeyT2c$score)
       HTML(paste(str1, str2, str3, sep = '<br/>')) 
       
     }
   })
   
   # Descriptive Plot
   output$descdiagT2 <- renderPlot({ 
     
     cormapData <- cormapData() %>% 
       subsetdf(sex = input$sexT2) %>%  
       filter(age >= input$ageT2[1] & age <= input$ageT2[2]) %>%
       drop_na(var, correlator) 
     
     plot <- ggplot(data = cormapData, aes(x = var, y = correlator)) +
       geom_point(color = my_colors[2], position = "jitter") +
       geom_smooth(method = "lm", color = "black", formula = y ~ x) +
       labs(x = input$varT1, y = input$cor) +
       ggtitle(paste0("Correlation between ", input$varT1, " and\n", input$cor, " (Switzerland)")) +
       theme_classic()
     plot
   })
}

# Run app
shinyApp(ui, server)