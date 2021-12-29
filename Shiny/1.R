#working directory belirleme. ?lkay? kendi kullan?c? ad?n?zla de?i?tirin.
setwd("C:/Users/ilkay/Documents/Github/R-Final/shiny")

#Gerekli Kütüphanelerin Yüklenmesi

library(shiny)
library(shinythemes)

#Kullanıcı Arayüzünün Belirlenmesi
ui <- fluidPage(
  #theme = shinytheme("united"),
  navbarPage(
    #theme = "united",
    "Kıtalara Göre Covid-19 Verileri",
    tabPanel("Grafikler",
             
             mainPanel(
               tabsetPanel(
                 tabPanel(
                   "Çizgi",
                   
                   sidebarPanel(
                     HTML("<h3>Değişkenler</h3>"),
                     
                     selectInput(
                       "outlook",
                       label = "Kıta:",
                       choices = list(
                         "Tüm Kıtalar" = "tum-kitalar",
                         "Afrika" = "afrika",
                         "Asya" = "asya",
                         "Avrupa" = "avrupa",
                         "Güney Amerika" = "guney_amerika",
                         "Kuzey Amerika" = "kuzey_merika"
                       ),
                       selected = "Asya"
                     ),
                     
                     selectInput(
                       "X_degiskeni",
                       label = "X Değişkeni:",
                       choices = list(
                         "Continent" = "continent",
                         "Location" = "location",
                         "New Cases" = "new_cases",
                         "Total Cases" = "total_cases",
                         "New Cases Per Million" = "new_cases_pm",
                         "New deaths" = "new_deaths",
                         "Total Deaths" = "total_deaths",
                         "New Deaths Per Million" = "new_deaths_pm",
                         "Population" = "population",
                         "Cases Density Population" = "cases_density_population"
                       ),
                       
                       selected = "continent"
                     ),
                     selectInput(
                       "Y_degiskeni",
                       label = "Y Değişkeni:",
                       choices = list(
                         "Continent" = "continent",
                         "Location" = "location",
                         "New Cases" = "new_cases",
                         "Total Cases" = "total_cases",
                         "New Cases Per Million" = "new_cases_pm",
                         "New deaths" = "new_deaths",
                         "Total Deaths" = "total_deaths",
                         "New Deaths Per Million" = "new_deaths_pm",
                         "Population" = "population",
                         "Cases Density Population" = "cases_density_population"
                         
                       )
                     ),
                     
                     actionButton("submitbutton", "Seç", class = "btn btn-primary")
                   )
                 ),
                 tabPanel("Yoğunluk",
                          h4("table")),
                 tabPanel("Bar",
                          h4("bar"))
               )
             )),
    tabPanel("Veriler",
             mainPanel(
               tabsetPanel(
                 tabPanel("Covid-19- Tüm Veriler"),
                 tabPanel("Afrika"),
                 tabPanel("Asya"),
                 tabPanel("Avrupa"),
                 tabPanel("Güney Amerika"),
                 tabPanel("Kuzey Amerika"),
               )
             )),
    tabPanel("Hazırlayanlar",
             mainPanel(
               HTML(
                 "<ul>
                    <li>Berca Akbayır</li>
                    <li>Ramazan Erduran</li>
                    <li>Mert Hasan Kılıç</li>
                    <li>Taha Durmuş</li>
                    <li>İlkay Şafak Baytar</li>

                  </ul>"
               )
             ))
  )
  
  
)

server <- function(input, output) {
  
}

shinyApp(ui = ui, server = server)