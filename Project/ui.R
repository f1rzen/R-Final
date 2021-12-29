
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
                       "kita",
                       label = "Kıta:",
                       choices = list(
                         "Tüm Kıtalar" = "a",
                         "Afrika" = "africa",
                         "Asya" = "Asia",
                         "Avrupa" = "Europe",
                         "Güney Amerika" = "Sout_America",
                         "Kuzey Amerika" = "North_America"
                       ),
                       selected = "Asya"
                     ),
                     
                     selectInput("xcol", "X Değişkeni:", vars),
                       
                      
                     
                     selectInput("ycol", "Y Değişkeni:", vars, selected = vars[[2]]),
                     
                     
                     actionButton("submitbutton", "Seç", class = "btn btn-primary")
                   ),
                   mainPanel(
                     plotOutput("linePlot")
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