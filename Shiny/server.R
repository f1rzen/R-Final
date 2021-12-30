##Gerekli Kütüphanelerin Yüklenmesi
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyalert)
##Gerekli Kütüphanelerin Yüklenmesi BİTTİ
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



####################################
# Server.R Kurulumu                #
####################################

server <- function(input, output, session) {
  ###Reactive verilerin Oluşturulması
  selectedX <- reactive({
    input$xcol
  })
  
  selectedY <- reactive({
    input$ycol
  })
  selectedKita <- reactive({
    get(input$kita)
  })
  
  selectedKitaDensity <- reactive({
    get(input$kita_density)
  })
  selectedKitaBar <- reactive({
    get(input$kita_bar)
  })
  selectedKitaScatter <- reactive({
    get(input$kita_scatter)
  })
  x_column <- selectedX
  y_column <- selectedY
  kita_data <- selectedKita
  
  
  #LinePlot Grafiği
  
  output$linePlot <- renderPlot({
    ggplot(selectedKita(), aes_string(x = input$xcol, y = input$ycol)) +
      geom_line(size = 0.5, colour = "#710193") +
      scale_x_continuous(trans = "log") +
      scale_y_continuous(trans = "log") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))+
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
      theme(text = element_text(size=15))
      
  })
  
  #scatterPlot
  
  output$scatterPlot <- renderPlot({
    ggplot(selectedKitaScatter(), aes_string(x = input$xcol_scatter, y = input$ycol_scatter)) +
      geom_point(shape=23, size=4, colour="#710193", alpha= 1/10) +
      scale_x_continuous(trans = "log") +
      scale_y_continuous(trans = "log") +
      geom_smooth(method = lm ,
                  color = "#239301",
                  se = FALSE) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(text = element_text(size=15))
        
    
  })
  
  rv <- reactiveValues(count=1)
  
  observeEvent(input$selectedPanel, {
    if (input$selectedPanel == "Scatter Plot") {
    if (rv$count == 1){
      
      shinyalert(title = "Dikkat!",
                 text = "Scatter Plot, alpha renk değeri kullandığı için grafiğin yüklenmesi uzun sürebilir.",
                 type = "warning",
                 showConfirmButton = TRUE,
                 confirmButtonText = "Tamam"
                 )
    
      
    }
      rv$count <- rv$count -1
      
    } })
    
    
 
    
  
  
  #densityPlot
  
  output$densityPlot <- renderPlot({
    ggplot(selectedKitaDensity(), aes_string(x = input$xcol_density)) +
      geom_density(adjust = 1L, fill = "#710193")  +
      
      scale_x_continuous(trans = "log") +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  
  

  
  #BarPlot
  output$barPlot <- renderPlot({
    ggplot(selectedKitaBar(),
           aes_string(x = input$xcol_bar, y = input$ycol_bar)) +
      geom_bar(fill = "#710193", stat = "identity") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(text = element_text(size=15))
  })
  
  
  ##Seçilen Değişkenlerin Data Frame'e atılması
  
  
  ####DATA TABLES####
  
  
  
  output$csvTableAll <- DT::renderDataTable(DT::datatable(a, options = list(pageLength = 15)))
  output$csvTableAfrica <- DT::renderDataTable(DT::datatable(africa, options = list(pageLength = 15)))
  output$csvTableAsia <- DT::renderDataTable(DT::datatable(Asia, options = list(pageLength = 15)))
  output$csvTableEurope <- DT::renderDataTable(DT::datatable(Europe, options = list(pageLength = 15)))
  output$csvTableNorthAmerica <- DT::renderDataTable(DT::datatable(North_America, options = list(pageLength = 15)))
  output$csvTableSouthAmerica <- DT::renderDataTable(DT::datatable(South_America, options = list(pageLength = 15)))
  
  
  
  
  
  
  
}