### Gerekli Kütüphanelerin Yüklenmesi ###

library(shiny)
library(shinyalert)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)


############################################################################
############################################################################
###                                                                      ###
###                        VERILERIN DÜZENLENMESI                        ###
###                                                                      ###
############################################################################
############################################################################



corona <- read_csv("coronavirus.csv")


a <- corona %>%
  filter(continent %in% c("Africa", "Asia", "Europe", "North America", "South America")) %>%
  summarize(
    date = date,
    continent = continent,
    location = location,
    new_cases = new_cases,
    total_cases = total_cases,
    new_cases_per_million = new_cases_per_million,
    new_deaths = new_deaths,
    date = date,
    total_deaths = total_deaths,
    new_deaths_per_million = new_deaths_per_million,
    population = population,
    cases_density_population = total_cases / population
  )
a <- a %>%
  filter(date > "2020-06-01" & date < "2020-06-30") %>%
  drop_na()


africa <- a %>%
  filter(continent == "Africa")
Asia <- a %>%
  filter(continent == "Asia")
Europe <- a %>%
  filter(continent == "Europe")
North_America <- a %>%
  filter(continent == "North America")
South_America <- a %>%
  filter(continent == "South America")

vars <- setdiff(names(a), "date")


ui <- fluidPage(
  
  useShinyalert(),
  ############################################################################
  ############################################################################
  ###                                                                      ###
  ###                               ÖZEL CSS                               ###
  ###                                                                      ###
  ############################################################################
  ############################################################################
  
  
  tags$head(
    tags$style(
      HTML(
        "
      @import url('https://fonts.googleapis.com/css2?family=Source+Code+Pro:wght@400;700&family=Trocchi&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Abril+Fatface&display=swap');
      body {
        background-color: #ffffff;
        color: black;
        font-family: 'Source Code Pro', sans-serif;
  
      }
        h3{
          font-family: 'Abril Fatface', sans-serif;
          font-size: 2.7em;
          margin-left: 1em;
        }
        .item {
          font-weight: bold;
        }
        body > div.container-fluid > nav > div > div > span {
          
          font-weight: bold;
          color: black;
        }
      body > div.container-fluid > nav > div > ul >li > a {
      font-weight: bold;
      }
      .active {
      color: black;
      }
        .tabbable {
          width: 150% !important;
        }
          .col-sm-12 {
            padding-top: 2em !important;
            padding-bottom: 2em !important;
          }
      #tab-9837-3 > div > ul >li {
      list-decoration: none;
      }
      
      h1{
      font-family: 'Abril Fatface', sans-serif;
      }
      .shiny-input-container {
        color: #4f2f2f2;
      
     
      }"
      )
    )),
  
  
  
  
  ##################################################################
  ##                       Arayüz Oluşturma                       ##
  ##                              Ve                              ##
  ##                            Plotlar                           ##
  ##################################################################
  
  
  
  
  navbarPage(
   
    "Kıtalara Göre Haziran Ayı Covid-19 Verileri",
    tabPanel("Grafikler",
             
             
             
             mainPanel(
               tabsetPanel(id="selectedPanel",
                 ####LINE PLOT###
                 tabPanel("Line Plot",
                          
                          fluidRow(
                            column(
                              12,
                              
                              column(3,
                                     HTML("<h3>Değişkenler -> </h3>"))
                              ,
                              
                              
                              column(
                                3,
                                selectInput(
                                  "kita",
                                  label = "Kıta:",
                                  choices = list(
                                    "Tüm Kıtalar" = "a",
                                    "Afrika" = "africa",
                                    "Asya" = "Asia",
                                    "Avrupa" = "Europe",
                                    "Güney Amerika" = "South_America",
                                    "Kuzey Amerika" = "North_America"
                                  ),
                                  selected = "a"
                                )
                              )
                              
                              ,
                              
                              column(3, selectInput("xcol", "X Değişkeni:", vars, selected = vars[[4]])) ,
                              
                              column(3, selectInput("ycol", "Y Değişkeni:", vars, selected = vars[[7]]))
                              
                              
                            ),
                            
                            
                          ),
                          mainPanel(plotOutput("linePlot", width = "150%"))),
                 ###SCATTER PLOT###
                 tabPanel(
                   "Scatter Plot",
                   fluidRow(column(12,
                                   
                                   column(3,      
                                          HTML("<h3>Değişkenler -></h3>")),
                                   
                                   column(3,
                                          selectInput(
                                            "kita_scatter",
                                            label = "Kıta:",
                                            choices = list(
                                              "Tüm Kıtalar" = "a",
                                              "Afrika" = "africa",
                                              "Asya" = "Asia",
                                              "Avrupa" = "Europe",
                                              "Güney Amerika" = "South_America",
                                              "Kuzey Amerika" = "North_America"
                                            )),
                                          selected = "a"
                                   ),
                                   
                                   column(3,
                                          selectInput("xcol_scatter", "X Değişkeni:", vars, selected = vars[[4]])),
                                   
                                   column(3,
                                          selectInput("ycol_scatter", "Y Değişkeni:", vars, selected = vars[[7]]))
                                   
                                   
                                   
                   )),
                   mainPanel(plotOutput("scatterPlot", width = "150%"))
                 ),
                  ####DENSITY PLOT####
                  tabPanel(
                   "Density Plot",
                   fluidRow(column(12,
                                   
                    column(4,               
                     HTML("<h3>Değişkenler -></h3>")),
                    column(4,
                     selectInput(
                       "kita_density",
                       label = "Kıta:",
                       choices = list(
                         "Tüm Kıtalar" = "a",
                         "Afrika" = "africa",
                         "Asya" = "Asia",
                         "Avrupa" = "Europe",
                         "Güney Amerika" = "South_America",
                         "Kuzey Amerika" = "North_America"
                       ),
                       selected = "a"
                     )),
                     column(4,
                     selectInput("xcol_density", "X Değişkeni:", vars, selected = vars[[4]])
                     )
                   )
                     
                     
                     
                     
                     
                   ),
                   mainPanel(plotOutput("densityPlot", width = "150%"))
                   
                   
                 ),
                 ####BAR PLOT####
                 tabPanel(
                   "Bar Plot",
                   fluidRow((column(12,
                     
                        column(3,           
                        HTML("<h3>Değişkenler -></h3>")
                        ),
                        
                        column(3,
                     selectInput(
                       "kita_bar",
                       label = "Kıta:",
                       choices = list(
                         "Tüm Kıtalar" = "a",
                         "Afrika" = "africa",
                         "Asya" = "Asia",
                         "Avrupa" = "Europe",
                         "Güney Amerika" = "South_America",
                         "Kuzey Amerika" = "North_America"
                       )),
                       selected = "a"
                     ),
                     
                     column(3,
                     selectInput("xcol_bar", "X Değişkeni:", vars, selected = vars[[1]])),
                     
                     column(3,
                     selectInput("ycol_bar", "Y Değişkeni:", vars, selected = vars[[3]]))
                   ))
                     
                     
                   ),
                   mainPanel(plotOutput("barPlot", width = "150%"))
                 ),
                 

               )
             )),
    #################################################################
    ##                      Verilerin Tabloda                      ##
    ##                        Görüntülenmesi                       ##
    #################################################################
    tabPanel("Veriler",
             mainPanel(
               tabsetPanel(
                 tabPanel("Covid-19- Tüm Veriler", fluidRow(column(12, DT::dataTableOutput('csvTableAll')))),
                 tabPanel("Afrika", DT::dataTableOutput('csvTableAfrica')),
                 tabPanel("Asya", DT::dataTableOutput('csvTableAsia')),
                 tabPanel("Avrupa", DT::dataTableOutput('csvTableEurope')),
                 tabPanel("Güney Amerika", DT::dataTableOutput('csvTableSouthAmerica')),
                 tabPanel("Kuzey Amerika", DT::dataTableOutput('csvTableNorthAmerica')),
               )
             )),
    
    
    ###Hazırlayanlar Bölümü###
    tabPanel("Hazırlayanlar",
             mainPanel(
               HTML(
                 "<h1>İstatistiksel Yazılımlar Final Ödevi</h1> <br>
                 <h4>Hazırlayanlar:</h4> <br>
                 
                 <ul>
                    <li>Berca Akbayır: </li>
                    <li>Ramazan Erduran: 21821809</li>
                    <li>Mert Hasan Kılıç: 21936134</li>
                    <li>Talha Kurt: 21936191</li>
                    <li>İlkay Şafak Baytar: 21935712</li>

                  </ul>"
               )
             ))
  )
  
  
)
