#Load Packages ---------------

library(shiny)
library(rmarkdown)
library(rsconnect)
library(tidyverse)
library(broom)
library(mosaic)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(dplyr)
library(tidyr)
library(shinycssloaders)
library(ggrepel)
library(rintrojs)
library(shinyBS)
library(shinythemes)
library(DT)


# Import Data --------------
library(readr)
thatRaiseData2 <- read_csv("hi/dfpt2.csv")

thatRaiseData2 <- thatRaiseData2 %>%
    mutate(party = case_when(
        `PVI#` < -4 ~ "Rep",
        `PVI#` > 4 ~ "Dem",
        TRUE ~ "Moderate"  )  )


thatRaiseData2 <- thatRaiseData2 %>%
    mutate(rurality = case_when(
        RUCA2 < 3.05 ~ "Urban",
        RUCA2 < 4.05 ~ "Suburban",
        RUCA2 < 4.15 ~ "Urban",
        RUCA2 < 5.05 ~ "Suburban",
        RUCA2 < 5.15 ~ "Urban",
        RUCA2 < 6.9 ~ "Suburban",
        RUCA2 < 7.05 ~ "Rural",
        RUCA2 < 7.15 ~ "Urban",
        RUCA2 < 7.25 ~ "Suburban",
        RUCA2 < 8.05 ~ "Rural",
        RUCA2 < 8.15 ~ "Urban",
        RUCA2 < 8.25 ~ "Suburban",
        RUCA2 < 10.05 ~ "Rural",
        RUCA2 < 10.15 ~ "Urban",
        RUCA2 < 10.25 ~ "Suburban",
        TRUE ~ "Rural"  )  )

perChangeMay <- ((thatRaiseData2$`2020-06-30`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`)

perChangeApril <- ((thatRaiseData2$`2020-05-31`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`)

perChangeMarch <- ((thatRaiseData2$`2020-04-30`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`)

perChangeFeb <- ((thatRaiseData2$`2020-03-31`) - (thatRaiseData2$`2020-02-29`))/ (thatRaiseData2$`2020-02-29`)


rmdfiles <- c("hi/statisticalFindingsRaise.Rmd")
sapply(rmdfiles, knit, quiet = T)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

graphLabel <- function(LFG){
  if (LFG == "MedianAge") {return("Median Age")}
  else if (LFG == "rurality") {return("Rural-Urban")}
  else if (LFG == "party") {return("Party")}
  else if (LFG == "HousingCategorical") {return("Housing Price")}
  else if (LFG == "MedHouseIncomeCategorical") {return("Median Household Income")}
  else if (LFG == "CovidRestrictionsCategorical") {return("COVID-19 Restrictions")}
  else if (LFG == "CovidImpactCategorical") {return("COVID-19 Impact")}
  else if (LFG == "PercentPoverty") {return("Percent Poverty")}
  else if (LFG == "Covid.per.10.000") {return("COVID-19 Cases per 10,000 People")}
  else if (LFG == "PVI#") {return("PVI Value")}
  else if (LFG == "PopDensity") {return("Population Density")}
  else if (LFG == "2020-06-30") {return("Housing Price (May 2020)")}
  else {return(LFG)}
}
graphLabelQuant2 <- function(LFG2){
  if(LFG2 == "2020-06-30"){
    return("May")
  }
  else if(LFG2 == "2020-05-31"){
    return("April")
  }
  else if(LFG2 == "2020-04-30"){
    return("March")
  }
  else if(LFG2 == "2020-02-29"){
    return("Jan")
  }
  else if(LFG2 == "2020-03-31"){
    return("Feb")
  }
  else {return(LFG)}
}
graphLabelQuant <- function(LFG, LFG2){
  if(LFG == "2020-06-30"){
    
    return(paste("Perchent Change in Housing Price (May-", graphLabelQuant2(LFG2),  ")", sep=""))
  }
  else if(LFG == "2020-05-31"){
    return(paste("Perchent Change in Housing Price (April-",  graphLabelQuant2(LFG2),  ")", sep=""))
  }
  else if(LFG == "2020-04-30"){
    return(paste("Perchent Change in Housing Price (March-",  graphLabelQuant2(LFG2),  ")", sep=""))
  }
  else if(LFG == "2020-02-29"){
    return(paste("Perchent Change in Housing Price (Jan-",  graphLabelQuant2(LFG2),  ")", sep=""))
  }
  else if(LFG == "2020-03-31"){
    return(paste("Perchent Change in Housing Price (Feb-",  graphLabelQuant2(LFG2),  ")", sep=""))
  }
  else {return(LFG)}
}



# UI -------------
ui <- fluidPage(
    #Navbar structure for UI
    navbarPage("COVID-19 and Housing Prices", theme = shinytheme("united"),
               
               #TAB1 -------
               tabPanel("EXPlORE CATEGORICAL DATA", fluid = TRUE, icon = icon("bar-chart-o"),
                        tags$style(button_color_css),
                        
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                
                                
                                fluidRow(column(8,
                                  selectInput(
                                      inputId = "startingMonth",
                                      label = "Select a beginning month",
                                      choices = c("January" = "2020-02-29", "Feburary" = "2020-03-31", "March" = "2020-04-30", "April" = "2020-05-31"),
                                      selected = "2020-02-29" ),
                                                
                                  selectInput(
                                      inputId = "endingMonth",
                                      label = "Select a ending month",
                                        choices = c("Feburary" = "2020-03-31", "March" = "2020-04-30", "April" = "2020-05-31", "May" = "2020-06-30"),
                                         selected = "2020-06-30"
                                                ),
                                                
                                                                
                                radioButtons(inputId = "catData",
                                        label = "Select Variable:",
                                        choices = c("Race" = "Race", "Party" = "party", "Housing Price" = "HousingCategorical", "State" = "State", "Rural-Urban" = "rurality", 
                                                    "Median Household Income" = "MedHouseIncomeCategorical", "COVID-19 Restrictions" = "CovidRestrictionsCategorical", "COVID-19 Impact" = "CovidImpactCategorical"),
                                        selected = "Race"),
                                
                                radioButtons(inputId = "catData2",
                                             label = "Select Second Variable:",
                                             choices = c("Race" = "Race", "Party" = "party", "Housing Price" = "HousingCategorical", "State" = "State", "Rural-Urban" = "rurality", 
                                                         "Median Household Income" = "MedHouseIncomeCategorical", "COVID-19 Restrictions" = "CovidRestrictionsCategorical", "COVID-19 Impact" = "CovidImpactCategorical"),
                                             selected = "party"),
                                strong("About this page:"),
                                "Each point represents a zip code. The month entries determine what months' median housing prices are used compute the percent change in housing price found on the y-axis. The x-axis uses categorical variables to sort the zip codes into groups. The top two graphs use the first variable selection and the bottom graphs use the
                                    second selection. If the same variable is selected for both variables or both months, an error will occur and
                                    you will have to change one selection. The plots with boxes are box plots, and the middle line in the rectangle is the median. The top amd bottom of the rectangle represent the 75% and 25% marks respectively.
                                "
    
                                
                                )), width = 4), #end of column 
                                
                                mainPanel(
                                    fluidRow(
                                        column(6, plotOutput(outputId = "plot1")),
                                        column(6, plotOutput(outputId = "plot3"))
                                    ),
                                    
                                    fluidRow(
                                        column(6, plotOutput(outputId = "plot2")),
                                        column(6, plotOutput(outputId = "plot4"))
                                    )
                                    
                            
                                ))), #end of tabPanelEXPLORE CATEGORICAL
               
               #TAB2 -----
               tabPanel("EXPLORE QUANTITATIVE DATA", fluid = TRUE, icon = icon("chart-line"),
                        tags$style(button_color_css),
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            fluidRow(column(8,
                                            
                                            h4("Numerical Based Graph"), 
                                            
                                            selectInput(
                                              inputId = "xaxisVar1",
                                              label = "Select the x-axis variable",
                                              choices = c("Median Age" = "MedianAge", "Percent Poverty" = "PercentPoverty", "COVID-19 Cases per 10,000 People" = "Covid.per.10.000", "PVI Value" = "PVI#", "Population Density" = "PopDensity", "Housing Price (May 2020)" = "2020-06-30"),
                                              selected = "MedianAge"
                                            ),
                                            
                                            selectInput(
                                              inputId = "startingMonth1",
                                              label = "Select a beginning month",
                                              choices = c("January" = "2020-02-29", "Feburary" = "2020-03-31", "March" = "2020-04-30", "April" = "2020-05-31"),
                                              selected = "2020-02-29" ),
                                            
                                            selectInput(
                                              inputId = "endingMonth1",
                                              label = "Select a ending month",
                                              choices = c("Feburary" = "2020-03-31", "March" = "2020-04-30", "April" = "2020-05-31", "May" = "2020-06-30"),
                                              selected = "2020-06-30"
                                            ),
                                            
                                            radioButtons(
                                              inputId = "colorQuant1",
                                              label = "Select a variable to color",
                                              choices = c("Race" = "Race", "Party" = "party", "Housing Price" = "HousingCategorical", "State" = "State", "Rural-Urban" = "rurality", 
                                                          "Median Household Income" = "MedHouseIncomeCategorical", "COVID-19 Restrictions" = "CovidRestrictionsCategorical", "COVID-19 Impact" = "CovidImpactCategorical", "None" = NULL),
                                              selected = "Race"
                                            ),
                                            
                                            radioButtons(
                                              inputId = "graphOptions1",
                                              label = "Graph Options",
                                              choices = c("Points" = "noSmooth", "Line of Best Fit" = "lm", "Line of Best Fit by Variable" = "lmVar", "Curve" = "curve", "Curve by Variable" = "curveVar"),
                                              selected = "noSmooth"
                                            ),
                                            
                                            h5(strong("Include Standard Error?")),
                                            
                                            checkboxInput(
                                              inputId = "seQuestion1",
                                              label = "Don't Include",
                                              TRUE
                                              
                                            )
                                            
                            ) ), width = 4), 
                          
                          
                          mainPanel(
                            plotOutput(outputId = "plot6"),
                            strong("About this page:"),
                            "Each point on the graph represents a different zip code. The x-axis variable selection is used for the x-axis on the graph and the 
                            y-axis is calculated using the month selections to create percent change in housing price. The graph options allow 
                            you to customize if you want a line/curve of best fit. A line of best fit is a linear regression line, calculated by reducing the total difference between each point and the line. The curve is calculated by averaging y-axis values that correspond with x-axis values near that point on the curve. 
                            When a best fit curve is done by variable, it uses the 
                            groupings from the variable selected as the color. The gray band that appears when the checkbox \"Include Standard Error?\"
                            is not checked is the Standard Error of the curve of best fit. If there is overlapping gray between lines, then the data shows no statistical
                            difference between different categories within the groupings indicated by color."
                          )
                        )
                        
                        
               ),#end of tabPanel
               
               #TAB3 -----
               tabPanel("EXPLORE OTHER DATA", fluid = TRUE, icon = icon("chart-area"),
                        tags$style(button_color_css),
                        
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                fluidRow(column(8,
                                                
                                h4("Numerical Based Graph"), 
                                
                                selectInput(
                                    inputId = "xaxisVar",
                                    label = "Select the x-axis variable",
                                    choices = c("Median Age" = "MedianAge", "Percent Poverty" = "PercentPoverty", "COVID-19 Cases per 10,000 People" = "Covid.per.10.000", "PVI Value" = "PVI#", "Population Density" = "PopDensity", "Housing Price (May 2020)" = "2020-06-30"),
                                    selected = "MedianAge"
                                ),
                                
                                
                                selectInput(
                                    inputId = "yaxisVar",
                                    label = "Select the y-axis variable",
                                    choices = c("Median Age" = "MedianAge", "Percent Poverty" = "PercentPoverty", "COVID-19 Cases per 10,000 People" = "Covid.per.10.000", "PVI Value" = "PVI#", "Population Density" = "PopDensity", "Housing Price (May 2020)" = "2020-06-30"),
                                    selected = "PercentPoverty"
                                ),
                                
                                
                                radioButtons(
                                    inputId = "colorQuant",
                                    label = "Select a variable to color",
                                    choices = c("Race" = "Race", "Party" = "party", "Housing Price" = "HousingCategorical", "State" = "State", "Rural-Urban" = "rurality", 
                                                "Median Household Income" = "MedHouseIncomeCategorical", "COVID-19 Restrictions" = "CovidRestrictionsCategorical", "COVID-19 Impact" = "CovidImpactCategorical", "None" = NULL),
                                    selected = "Race"
                                ),
                                
                                radioButtons(
                                  inputId = "graphOptions",
                                  label = "Graph Options",
                                  choices = c("Points" = "noSmooth", "Line of Best Fit" = "lm", "Line of Best Fit by Variable" = "lmVar", "Curve" = "curve", "Curve by Variable" = "curveVar"),
                                  selected = "noSmooth"
                                ),
                                
                                h5(strong("Include Standard Error?")),
                                
                                checkboxInput(
                                  inputId = "seQuestion",
                                  label = "Don't Include", 
                                  TRUE
                                  
                                )
                                
                                ) ), width = 4), 
                            
                            
                            mainPanel(
                                plotOutput(outputId = "plot5"),
                                strong("About this page:"),
                                "Each point on the graph represents a different zip code. The graph options allow 
                            you to customize if you want a line/curve of best fit.  A line of best fit is a linear regression line, calculated by reducing the total difference between each point and the line. The curve is calculated by averaging y-axis values that correspond with x-axis values near that point on the curve. When a best fit curve is done by variable, it uses the 
                            groupings from the variable selected as the color. The gray band that appears when the checkbox \"Include Standard Error?\"
                            is not checked is the Standard Error of the curve of best fit. If there is overlapping gray between lines, then the data shows no statistical
                            difference between different categories within the groupings indicated by color. The other graph type has a point for each zip code's percent change in housing price, with the red dot being the median for each category."
                            )
                        )
                        
                        
               ),#end of tabPanelMOREEXPL>ORE
               
               #TAB4 -----
               tabPanel("STATISTICAL FINDINGS", fluid = TRUE, icon = icon("clipboard"),
                        tags$style(button_color_css),
                        
                       includeMarkdown("statisticalFindingsRaise.md")
                        
                        
                        
               ),#end of tabPanelSTATS
               
               #TAB5 -----
               tabPanel("PROCESS", fluid = TRUE, icon = icon("list-alt"),
                        tags$style(button_color_css),
                        
                        h3("The Process For This Project"),
                        h4("Motivation:"),
                        "As COVID-19 cases were rising, I wanted to look at the impact the virus had on society 
                        through a different lens than illness. Having been in quarantine for two months, the impact 
                        of housing on people’s lives was on my mind. Therefore, I decided to analyze the effect of 
                        the virus on housing prices in areas with varying demographics. As a prospective computer 
                        science and economics double major, this project provided a great intersection between my 
                        two interests. My goal was to analyze housing data in zip codes with varying race, income, political 
                        affiliation, COVID-19 impact, and COVID-19 restrictions, specifically between January and 
                        May of 2020. The analysis would determine which variables have a significant effect on changes
                        in housing cost and how COVID-19 unequally affects people across different social, geographic, 
                        and economic sectors. Then I wanted to make the data and results accessible. ",
                        hr(),
                        h4("Step One: Create a Dataset"),
                        "The first step was to see which states had COVID-19 data available by zip code, using Google to find states with maps or data tables that provided the number of COVID-19 cases in each zip code.
                        While states consistently had data by county, I chose to only use states where they had data available by 
                        zip code because a smaller area generally has less variability in demographics, so zip codes would provide more meaningful conlcusions. 
                        Since much of my analysis would be centered around variables such as race, income, and political affiliation,
                        using county data would sacrifice better results for ease. Then I selected eight states with 
                        good zip code virus data and randomly generated 200 zip codes from those states. After that, the making of the 
                        dataset involved finding reliable data for each variable I wanted to include and adding it to 
                        the dataset using the RStudio join() function and occasionally manual entry. Data was found using Cubit's zip code demographics by state (which uses the American Community Survey 5-Year Data); Cook Political Report's PVI index;
                        the Social Security Administration's data; the United States Department of Agriculture rural-urban continuum codes; Delaware, Illinois, Florida, Pennsylvania, Maryland, New Mexico, South Carolina, and Virginia's COVID-19 data; and an article by Adam McCann from WalletHub, ‘States with the Fewest Coronavirus Restrictions.’.
                        The final step was filtering and joining the housing data from Zillow to the dataset, using Zillow's housing datasets. Access to data locations can be found under references.",
                        hr(),
                        h4("Step Two: Analyze Data"),
                        "In order to analyze data, I used RStudio, which uses the coding language r, to run statistical 
                        tests and create graphs based off of my data. While trying out different types of graphs to see 
                        which ones best displayed my data, I would occasionally go back and alter the dataset in order 
                        to make it more practical for this project. Then, I ran an ANOVA test on each of my categorical 
                        variables, which is a statistical test to see if there is a difference between groups that seems 
                        to not be due to random chance, given by a p-value. The ANOVA test looked to see if there 
                        appeared to be a difference with variables such as race, political party, or intensity of COVID-19
                        restrictions in regard to the percent change in housing price over specified months, for example, 
                        January to May. For the quantitative variables, I ran linear regressions between the variables, 
                        such as income, percent poverty, or median age, to see how they interacted with percent change 
                        in housing price. This would give both a correlation constant, which is how much the change in 
                        percent change in housing price can be explained by the other variable, and a p-value. Linear 
                        regression can also be done using multiple variables, both categorical and quantitative, in 
                        order to see how combining factors can make a better model for predicting the percent change 
                        in housing price. By adding or removing different variables, I was able to find which variables 
                        worked together to have a significant impact on the change in housing price. ",
                        hr(),
                        h4("Step Three: Make the Webpage"),
                        "This webpage was made using the Shiny package on RStudio which provides a way to use the coding 
                        language r to make an application. I made two Shiny applications at the same time, one which had all 
                        my data and graphs, which became the one you see now. The other was very simple, and I used it when I got error 
                        messages with more complicated code. This allowed me to more easily pinpoint the problem and 
                        find solutions without having to worry about confusion caused by large amounts of data. After completing a version 1, I shared it with people and used their feedback to 
                        make changes to the webpage."

                       
                       
                        
               ),#end of tabPanelPROCESS
               
               #TAB6 -----
               tabPanel("DATA", fluid = TRUE, icon = icon("table"),
                        tags$style(button_color_css),
                        column(12,
                        h2("The dataset:"),
                        DT::dataTableOutput("thatDataTable")
                        )
                        
               ),#end of tabPanelDATA
               
               #TAB7 -----
               tabPanel("REFERENCES", fluid = TRUE, icon = icon("sort-alpha-up"),
                        tags$style(button_color_css),
                        
                        h4("RStudio Packages"),
                        "Winston Chang, Joe Cheng, JJ
                        Allaire, Yihui Xie and Jonathan
                        McPherson (2020). shiny: Web
                        Application Framework for R. R
                        package version 1.5.0.
                        https://CRAN.R-project.org/package=shiny",   HTML('<br/>'), HTML('<br/>'),
                        "Winston Chang and Barbara Borges Ribeiro (2018).
  shinydashboard: Create Dashboards with 'Shiny'. R
  package version 0.7.1.
  https://CRAN.R-project.org/package=shinydashboard",   HTML('<br/>'), HTML('<br/>'),
                        "Dean Attali (2020). shinyjs: Easily Improve the User
  Experience of Your Shiny Apps in Seconds. R package
  version 1.1. https://CRAN.R-project.org/package=shinyjs",   HTML('<br/>'), HTML('<br/>'),
                        "H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
  Springer-Verlag New York, 2016.",   HTML('<br/>'), HTML('<br/>'),
                        "JJ Allaire and Yihui Xie and Jonathan McPherson and
  Javier Luraschi and Kevin Ushey and Aron Atkins and
  Hadley Wickham and Joe Cheng and Winston Chang and
  Richard Iannone (2020). rmarkdown: Dynamic Documents for
  R. R package version 2.1. URL
  https://rmarkdown.rstudio.com.",   HTML('<br/>'), HTML('<br/>'),
  
  "Yihui Xie (2020). knitr: A General-Purpose Package for
  Dynamic Report Generation in R. R package version 1.27.", HTML('<br/>'), HTML('<br/>'),
  
 "Hadley Wickham, Romain François, Lionel Henry and Kirill
  Müller (2019). dplyr: A Grammar of Data Manipulation. R
  package version 0.8.3.
  https://CRAN.R-project.org/package=dplyr", HTML('<br/>'), HTML('<br/>'),
 "Hadley Wickham and Lionel Henry (2019). tidyr: Tidy
  Messy Data. R package version 1.0.0.
  https://CRAN.R-project.org/package=tidyr", HTML('<br/>'), HTML('<br/>'),
 "Andras Sali and Dean Attali (2020). shinycssloaders: Add
  CSS Loading Animations to 'shiny' Outputs. R package
  version 0.3.
  https://CRAN.R-project.org/package=shinycssloaders", HTML('<br/>'), HTML('<br/>'),
 "JJ Allaire (2019).
  rsconnect: Deployment
  Interface for R Markdown
  Documents and Shiny
  Applications. R package
  version 0.8.16.
  https://CRAN.R-project.org/package=rsconnect", HTML('<br/>'), HTML('<br/>'),
 "Hadley Wickham, Jim
 Hester and Romain
 Francois (2018). readr:
   Read Rectangular Text
 Data. R package version
 1.3.1.
 https://CRAN.R-project.org/package=readr", HTML('<br/>'), HTML('<br/>'),
 
 "Winston Chang (2018). shinythemes: Themes for Shiny. R
  package version 1.1.2.
  https://CRAN.R-project.org/package=shinythemes", HTML('<br/>'), HTML('<br/>'),
 "Kamil Slowikowski (2019). ggrepel: Automatically
  Position Non-Overlapping Text Labels with 'ggplot2'. R
  package version 0.8.1.
  https://CRAN.R-project.org/package=ggrepel", HTML('<br/>'), HTML('<br/>'),
 "Carl Ganz (2016). rintrojs: A Wrapper for the Intro.js
  Library. Journal of Open Source Software, 1(6), October
  2016. URL http://dx.doi.org/10.21105/joss.00063", HTML('<br/>'), HTML('<br/>'),
 "Eric Bailey (2015). shinyBS: Twitter Bootstrap
  Components for Shiny. R package version 0.61.
  https://CRAN.R-project.org/package=shinyBS", HTML('<br/>'), HTML('<br/>'),
 "Yihui Xie, Joe Cheng and Xianying Tan (2020). DT: A
  Wrapper of the JavaScript Library 'DataTables'. R
  package version 0.14.
  https://CRAN.R-project.org/package=DT", HTML('<br/>'), HTML('<br/>'),
                     
 h4("References"),
 "\"Cases By Zip Code.\"", em("Florida's COVID-19 Data and Surveillance Dashboard"), ", Florida Department of Health, Division of Disease Control and Health Protection, 26 June 2020, experience.arcgis.com/experience/96dd742462124fa0b38ddedb9b25e429.", HTML('<br/>'), HTML('<br/>'),
 "\"Coronavirus Disease (COVID-19)\"",  em("Delaware Division of Public Health, Coronavirus Response"), ", 26 June 2020, coronavirus.delaware.gov/.", HTML('<br/>'), HTML('<br/>'),
 "\"COVID-19 Data Insights.\"",  em("Virginia Department of Health"), ", 26 June 2020, www.vdh.virginia.gov/coronavirus/covid-19-data-insights/#ZIP-code.", HTML('<br/>'), HTML('<br/>'),
 "\"COVID-19 Statistics.\"",  em("IDPH | Illinois Department of Public Health"), ", 26 June 2020, www.dph.illinois.gov/covid19/covid19-statistics.", HTML('<br/>'), HTML('<br/>'),
 "Cromartie, John. \"Rural-Urban Continuum Codes.\"",  em("United States Department of Agriculture - Economic Research Service"), ", United States Department of Agriculture, 25 Oct. 2019, www.ers.usda.gov/data-products/rural-urban-continuum-codes.", HTML('<br/>'), HTML('<br/>'),
 "\"Delaware Zip Codes by Population.\"",  em("Delaware Demographics"), ", Cubit Planning, Inc., 2019, www.delaware-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"Florida Zip Codes by Population.\"",  em("Florida Demographics"), ", Cubit Planning, Inc., 2019, www.florida-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"Housing Data.\"",  em("Zillow Research"), ", www.zillow.com/research/data/.", HTML('<br/>'), HTML('<br/>'),
 "\"Illinois Zip Codes by Population.\"",  em("Illinois Demographics"), ", Cubit Planning, Inc., 2019, www.illinois-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"Maryland Coronavirus Data by ZIP Code.\"",  em("WBAL"), ", 26 June 2020, www.wbaltv.com/article/coronavirus-maryland-data-zip-code/32128818.", HTML('<br/>'), HTML('<br/>'),
 "\"Maryland Zip Codes by Population.\"",  em("Maryland Demographics"), ", Cubit Planning, Inc., 2019, www.maryland-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "McCann, Adam. \"States with the Fewest Coronavirus Restrictions.\"",  em("WalletHub"), ", Evolution Finance, Inc., 26 June 2020, wallethub.com/edu/states-with-the-fewest-coronavirus-restrictions/73818/.", HTML('<br/>'), HTML('<br/>'),
 "\"New Mexico Zip Codes by Population.\"",  em("New Mexico Demographics"), ", Cubit Planning, Inc., 2019, www.newmexico-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"NMDOH COVID-19 Public Dashboard.\"",  em("New Mexico Department of Health:"), ", 26 June 2020, cvprovider.nmhealth.org/public-dashboard.html.", HTML('<br/>'), HTML('<br/>'),
 "\"OASDI Beneficiaries by State and ZIP Code, 2019.\"",  em("Social Security Administration"), ", www.ssa.gov/policy/docs/statcomps/oasdi_zip/.", HTML('<br/>'), HTML('<br/>'),
 "\"Pennsylvania Zip Codes by Population.\"",  em("Pennsylvania Demographics"), ", Cubit Planning, Inc., 2019, www.pennsylvania-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"PVI Map and District List.\"",  em("The Cook Political Report"), ", Cook Political Report, cookpolitical.com/pvi-map-and-district-list.", HTML('<br/>'), HTML('<br/>'),
 "\"SC Cases by County & ZIP Code (COVID-19).\"",  em("S.C. Department of Health and Environmental Control"), ", 26 June 2020, www.scdhec.gov/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/sc-cases-county-zip-code-covid-19.", HTML('<br/>'), HTML('<br/>'),
 "\"South Carolina Zip Codes by Population.\"",  em("South Carolina Demographics"), ", Cubit Planning, Inc., 2019, www.southcarolina-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"State-by-State COVID-19 Guidance.\"",  em("Resources for COVID-19 Restrictions and Rules by State"), ", Husch Blackwell LLP, 26 June 2020, www.huschblackwell.com/state-by-state-covid-19-guidance.", HTML('<br/>'), HTML('<br/>'),
 "\"Virginia Zip Codes by Population.\"",  em("Virginia Demographics"), ", Cubit Planning, Inc., 2019, www.virginia-demographics.com/.", HTML('<br/>'), HTML('<br/>'),
 "\"Zip Code Case Data.\"",  em("Pennsylvania COVID-19 Dashboard"), ", 26 June 2020, experience.arcgis.com/experience/cfb3803eb93d42f7ab1c2cfccca78bf7.", HTML('<br/>'), HTML('<br/>'),
 "\"ZIP Code to Census Tract to Metro Equivalence Table.\"",  em("Proximity One"), ", Proximity, proximityone.com/ziptractequiv.htm.", HTML('<br/>'), HTML('<br/>'),
 "\"ZIP Codes by State.\"",  em("ZIP Codes To Go"), ", www.zipcodestogo.com.", HTML('<br/>'), HTML('<br/>'),  
 
                          )
                      
               )
               
               )
               
    
#server -------
server <- function(input, output, session) {
    #TAB1Stuff -----  
    selectedData <- reactive({thatRaiseData2[, c(input$catData, input$catData2, input$startingMonth, input$endingMonth)]})
    
    
    output$plot1 <- renderPlot({
      ggplot(selectedData(), aes(!!as.name(input$catData), ((!!as.name(input$endingMonth) - !!as.name(input$startingMonth)) / !!as.name(input$startingMonth))*100 )) +
            geom_point(alpha = .4) +
            labs(x = graphLabel(input$catData), y = graphLabelQuant(input$startingMonth, input$endingMonth)) + 
            stat_summary(fun.y = median, geom = "point", size = 3, color = "red") + 
        ylim(-3, 7) + 
        theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold"))
    })
           
    output$plot2 <- renderPlot({
        ggplot(selectedData()) + 
            geom_boxplot(aes(!!as.name(input$catData), ((!!as.name(input$endingMonth) - !!as.name(input$startingMonth)) / !!as.name(input$startingMonth))*100, color = !!as.name(input$catData))) +
        labs(x = graphLabel(input$catData), y = graphLabelQuant(input$startingMonth, input$endingMonth), color = graphLabel(input$catData)) + 
        ylim(-3, 7) + theme(axis.text=element_text(size=12),
                            axis.title=element_text(size=14,face="bold"))
    })
    
    output$plot3 <- renderPlot({
      ggplot(selectedData(), aes(!!as.name(input$catData2), ((!!as.name(input$endingMonth) - !!as.name(input$startingMonth)) / !!as.name(input$startingMonth))*100)) +
        geom_point(alpha = .4) +
        labs(x = graphLabel(input$catData2), y = graphLabelQuant(input$startingMonth, input$endingMonth)) + 
        stat_summary(fun.y = median, geom = "point", size = 3, color = "red") + 
        ylim(-3, 7) + theme(axis.text=element_text(size=12),
                            axis.title=element_text(size=14,face="bold"))
    })
    
    output$plot4 <- renderPlot({
      ggplot(selectedData()) + 
        labs(x = graphLabel(input$catData2), y = graphLabelQuant(input$startingMonth, input$endingMonth), color = graphLabel(input$catData2)) + 
        geom_boxplot(aes(!!as.name(input$catData2), ((!!as.name(input$endingMonth) - !!as.name(input$startingMonth)) / !!as.name(input$startingMonth))*100, color = !!as.name(input$catData2))) + 
        ylim(-3, 7) + theme(axis.text=element_text(size=12),
                            axis.title=element_text(size=14,face="bold"))
    })
    
    
    #DataTable -----           
   # output$thatDataTable = DT::renderDataTable({thatRaiseData2})
    
    output$thatDataTable <- renderDataTable({
      datatable(thatRaiseData2, 
                extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                options = list(
                  # dom = 't',
                  # deferRender = TRUE,
                  searching = TRUE,
                  autoWidth = TRUE,
                  # scrollCollapse = TRUE,
                  rownames = FALSE,
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = "500px",
                  fixedHeader = TRUE,
                  class = 'cell-border stripe',
                  fixedColumns = list(
                    leftColumns = 3,
                    heightMatch = 'none'
                  )
                )
      )
    })
    
    
    #TAB2Stuff ------
    
    output$plot6 <- renderPlot ({
      
      selectedData3 <- reactive({thatRaiseData2[, c(input$xaxisVar1, input$startingMonth1, input$endingMonth1, input$colorQuant1)]})
      
      if(!!as.name(input$seQuestion1) == FALSE){
        if(!!as.name(input$graphOptions1) == "noSmooth")   {
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3(), aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party)) + 
              geom_point(alpha = .7, aes(size = 0.05)) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                    axis.title=element_text(size=14,face="bold")) + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1))) + 
              geom_point(alpha = .7, aes(size = 0.05)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1))
          }
        }
        else if(!!as.name(input$graphOptions1) == "lmVar")   {
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3(), aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party)) + 
              geom_smooth(method = "lm") + 
              geom_point(alpha = .7, aes(size = 0.05)) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              ylim(-3, 7)  + theme(axis.text=element_text(size=12),
                                   axis.title=element_text(size=14,face="bold")) + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1))) + 
              geom_smooth(method = "lm") + 
              geom_point(alpha = .7, aes(size = 0.05)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1))
          }
        }
        else if(!!as.name(input$graphOptions1) == "lm"){
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3()) + 
              geom_smooth(method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) +
              geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party, size = 0.05)) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot() + 
              geom_smooth(method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1), size = 0.05))
          }
        }
        else if(!!as.name(input$graphOptions1) == "curve"){
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3()) + 
              geom_smooth(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) + 
              geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party, size = 0.05)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot() + 
              geom_smooth(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1), size = 0.05))
          }
        }
        else{
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3(), aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party)) + 
              geom_smooth() + 
              geom_point(alpha = .7, aes(size = 0.05)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                 axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"), 
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1))) + 
              geom_smooth() + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              geom_point(alpha = .7, aes(size = 0.05))
          }
        }
      }
      
      
      else {
        if(!!as.name(input$graphOptions1) == "noSmooth")   {
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3(), aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party)) + 
              geom_point( aes(size = 0.05)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot( aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1))) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              geom_point(aes(size = 0.05))
          }
        }
        else if(!!as.name(input$graphOptions1) == "lmVar")   {
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3(), aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party)) + 
              geom_smooth(se = FALSE, method = "lm") + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              geom_point(aes(size = 0.05)) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1))) + 
              geom_smooth(se = FALSE, method = "lm") + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              geom_point( aes(size = 0.05))
            
          }
        }
        else if(!!as.name(input$graphOptions1) == "lm"){
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3()) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              geom_smooth(se = FALSE, method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) + 
              geom_point( aes(x = as.numeric(!!as.name(input$xaxisVar)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party, size = 0.05)) +
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot() + 
              geom_smooth(se = FALSE, method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) +
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              geom_point( aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1), size = 0.05))
            
          }
        }
        else if(!!as.name(input$graphOptions1) == "curve"){
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3()) + 
              geom_smooth(se = FALSE, aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              geom_point( aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party, size = 0.05)) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot() + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              geom_smooth(se = FALSE, aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100))) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              geom_point(aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1), size = 0.05))
          }
        }
        else{
          if(!!as.name(input$colorQuant1) == "party") {
            ggplot(selectedData3(), aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = party)) + 
              geom_smooth(se = FALSE) + 
              geom_point( aes(size = 0.05)) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = "Party") + 
              scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                 values=c("blue", "purple", "red")) }
          else{
            selectedData3() %>% 
              ggplot( aes(x = as.numeric(!!as.name(input$xaxisVar1)), y = (as.numeric((!!as.name(input$endingMonth1) - !!as.name(input$startingMonth1)) / !!as.name(input$startingMonth1))*100), color = !!as.name(input$colorQuant1))) + 
              geom_smooth(se = FALSE) + 
              ylim(-3, 7) + theme(axis.text=element_text(size=12),
                                  axis.title=element_text(size=14,face="bold")) + 
              labs(x = graphLabel(input$xaxisVar1), y = graphLabelQuant(input$startingMonth1, input$endingMonth1), color = graphLabel(input$colorQuant1)) + 
              geom_point(aes(size = 0.05))
          }
        }
      }
    })
    
    
    
    
    
    #TAB3Stuff ------
    output$plot5 <- renderPlot ({

      selectedData2 <- reactive({thatRaiseData2[, c(input$xaxisVar, input$yaxisVar, input$colorQuant, input$startingMonth2, input$endingMonth2)]})
      
    if(!!as.name(input$seQuestion) == FALSE){
      if(!!as.name(input$graphOptions) == "noSmooth")   {
        if(!!as.name(input$colorQuant) == "party") {
            ggplot(selectedData2(), aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party)) + 
                geom_point(alpha = .7, aes(size = 0.05)) + theme(axis.text=element_text(size=12),
                                                                 axis.title=element_text(size=14,face="bold")) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
                scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                                   values=c("blue", "purple", "red")) }
        else{
            selectedData2() %>% 
            ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant))) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
                geom_point(alpha = .7, aes(size = 0.05)) + theme(axis.text=element_text(size=12),
                                                                 axis.title=element_text(size=14,face="bold"))
        }
     }
    else if(!!as.name(input$graphOptions) == "lmVar")   {
      if(!!as.name(input$colorQuant) == "party") {
        ggplot(selectedData2(), aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party)) + 
          geom_smooth(method = "lm") + 
          geom_point(alpha = .7, aes(size = 0.05)) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
          theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold")) + 
        scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                             values=c("blue", "purple", "red")) }
      else{
        selectedData2() %>% 
          ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant))) + 
          geom_smooth(method = "lm") + theme(axis.text=element_text(size=12),
                                             axis.title=element_text(size=14,face="bold")) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
          geom_point(alpha = .7, aes(size = 0.05))
      }
    }
    else if(!!as.name(input$graphOptions) == "lm"){
      if(!!as.name(input$colorQuant) == "party") {
        ggplot(selectedData2()) + theme(axis.text=element_text(size=12),
                                        axis.title=element_text(size=14,face="bold")) + 
          geom_smooth(method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
          geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party, size = 0.05)) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
          scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                             values=c("blue", "purple", "red")) }
      else{
        selectedData2() %>% 
          ggplot() + theme(axis.text=element_text(size=12),
                           axis.title=element_text(size=14,face="bold")) + 
          geom_smooth(method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
          geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant), size = 0.05))
      }
    }
    else if(!!as.name(input$graphOptions) == "curve"){
      if(!!as.name(input$colorQuant) == "party") {
        ggplot(selectedData2()) + theme(axis.text=element_text(size=12),
                                        axis.title=element_text(size=14,face="bold")) + 
          geom_smooth(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
          geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party, size = 0.05)) + 
          scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                             values=c("blue", "purple", "red")) }
      else{
        selectedData2() %>% 
          ggplot() + theme(axis.text=element_text(size=12),
                           axis.title=element_text(size=14,face="bold")) + 
          geom_smooth(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
          geom_point(alpha = .7, aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant), size = 0.05))
      }
    }
    else{
      if(!!as.name(input$colorQuant) == "party") {
        ggplot(selectedData2(), aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party)) + 
          geom_smooth() + theme(axis.text=element_text(size=12),
                                axis.title=element_text(size=14,face="bold")) + 
          geom_point(alpha = .7, aes(size = 0.05)) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
          scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                             values=c("blue", "purple", "red")) }
      else{
        selectedData2() %>% 
          ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant))) + 
          geom_smooth() + theme(axis.text=element_text(size=12),
                                axis.title=element_text(size=14,face="bold")) + 
          labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
          geom_point(alpha = .7, aes(size = 0.05))
      }
    }
    }
    
    
    else {
      if(!!as.name(input$graphOptions) == "noSmooth")   {
        if(!!as.name(input$colorQuant) == "party") {
          ggplot(selectedData2(), aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party)) + 
            geom_point( aes(size = 0.05)) + theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=14,face="bold")) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
            scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                               values=c("blue", "purple", "red")) }
        else{
          selectedData2() %>% 
            ggplot( aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant))) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
            geom_point(aes(size = 0.05)) + theme(axis.text=element_text(size=12),
                                                 axis.title=element_text(size=14,face="bold"))
        }
      }
      else if(!!as.name(input$graphOptions) == "lmVar")   {
        if(!!as.name(input$colorQuant) == "party") {
          ggplot(selectedData2(), aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party)) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
            geom_smooth(se = FALSE, method = "lm") + 
            geom_point(aes(size = 0.05)) + theme(axis.text=element_text(size=12),
                                                axis.title=element_text(size=14,face="bold")) + 
            scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                               values=c("blue", "purple", "red")) }
        else{
          selectedData2() %>% 
            ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant))) + 
            geom_smooth(se = FALSE, method = "lm") + theme(axis.text=element_text(size=12),
                                                           axis.title=element_text(size=14,face="bold")) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
            geom_point( aes(size = 0.05))
            
        }
      }
      else if(!!as.name(input$graphOptions) == "lm"){
        if(!!as.name(input$colorQuant) == "party") {
          ggplot(selectedData2()) + theme(axis.text=element_text(size=12),
                                          axis.title=element_text(size=14,face="bold")) + 
            geom_smooth(se = FALSE, method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
            geom_point( aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party, size = 0.05)) +
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
            scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                               values=c("blue", "purple", "red")) }
        else{
          selectedData2() %>% 
            ggplot() + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold")) + 
            geom_smooth(se = FALSE, method = "lm", aes( x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) +
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
            geom_point( aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant), size = 0.05))
            
        }
      }
      else if(!!as.name(input$graphOptions) == "curve"){
        if(!!as.name(input$colorQuant) == "party") {
          ggplot(selectedData2()) + theme(axis.text=element_text(size=12),
                                          axis.title=element_text(size=14,face="bold")) + 
            geom_smooth(se = FALSE, aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
            geom_point(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party, size = 0.05)) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
            scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                               values=c("blue", "purple", "red")) }
        else{
          selectedData2() %>% 
            ggplot() + theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold")) + 
            geom_smooth(se = FALSE, aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)))) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
            geom_point( aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant), size = 0.05))
        }
      }
      else{
        if(!!as.name(input$colorQuant) == "party") {
          ggplot(selectedData2(), aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = party)) + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = "Party") + 
            geom_smooth(se = FALSE) + theme(axis.text=element_text(size=12),
                                            axis.title=element_text(size=14,face="bold")) + 
            geom_point(aes(size = 0.05)) + 
            scale_color_manual(breaks = c("Dem", "Moderate", "Rep"),
                               values=c("blue", "purple", "red")) }
        else{
          selectedData2() %>% 
            ggplot(aes(x = as.numeric(!!as.name(input$xaxisVar)), y = as.numeric(!!as.name(input$yaxisVar)), color = !!as.name(input$colorQuant))) + 
            geom_smooth(se = FALSE) + theme(axis.text=element_text(size=12),
                                           axis.title=element_text(size=14,face="bold"))  + 
            labs(x = graphLabel(input$xaxisVar), y = graphLabel(input$yaxisVar), color = graphLabel(input$colorQuant)) + 
            geom_point(aes(size = 0.05))
        }
      }
    }
    })
    

}



    


shinyApp(ui = ui, server = server)



#Error in appObj()$onStart() : attempt to apply non-function
