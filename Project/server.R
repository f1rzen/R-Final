##Gerekli Kütüphanelerin Yüklenmesi
library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
##Gerekli Kütüphanelerin Yüklenmesi BİTTİ


####################################
# Server.R Kurulumu                #
####################################

server <- function(input, output, session) {
  

  ##Veri Setinden Verilerin Oluşturulması
  
  corona <- read_csv("coronavirus.csv")
  a <-corona %>%
    filter(continent %in% c("Africa","Asia","Europe","North America","South America")) %>%
    summarize(date=date,continent=continent, location=location, new_cases=new_cases, total_cases=total_cases, new_cases_per_million=new_cases_per_million,
              new_deaths=new_deaths,date=date, total_deaths=total_deaths, new_deaths_per_million=new_deaths_per_million, population=population,cases_density_population=total_cases/population)
  
  a <- a%>%
    filter(date > "2020-06-01" & date <"2020-06-30") %>%
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
  ##Veri Setinden Verilerin Oluşturulması BİTTİ
  
  ##Verilerin columnlarının tarih dışında seçimleri için değişken oluşturma
  vars <- setdiff(names(a), "date")
  
  
##############Line Plot####################  
  

  
  ##Seçilen Değişkenlerin Data Frame'e atılması

  
  
  
}
