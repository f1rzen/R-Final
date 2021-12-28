library(readr)
corona <- read_csv("coronavirus.csv")

library(dplyr)
library(tidyr)

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

x_asia <- Asia[sample(nrow(Asia),20),]
x_africa <- africa[sample(nrow(africa),20),]
x_europe <- Europe[sample(nrow(Europe),20),]
x_sth_america <- South_America[sample(nrow(South_America),20),]
x_nrth_america <- North_America[sample(nrow(North_America),20),]

x_corona <- rbind(x_asia,x_africa,x_europe,x_sth_america,x_nrth_america)

library(writexl)
write_xlsx(x_corona, "C:\\Users\\lenovo\\Documents\\GitHub\\R-Final\\Datasets\\y_corona.xlsx")

library(readxl)
y_corona <- read_excel("~/GitHub/R-Final/Datasets/y_corona.xlsx")

#Gozlemi yapilacak veri Y_CORONA verisi


#ESQUISSE PART
library(esquisse)
y_corona$continent <- as.factor(y_corona$continent)
y_corona$location <- as.factor(y_corona$location)
esquisse::esquisser(data=y_corona)







#TÜM KITALARIN toplam vaka sayılarına ilişkin BAR GRAFİĞİ: 
library(ggplot2)
ggplot(y_corona) +
 aes(x = continent, weight = total_cases) +
 geom_bar(fill = "#112446") +
 theme_minimal()







###ASIA###
#Asianın toplam vaka / toplam ölüm ÇİZGİ GRAFİĞİ:
library(dplyr)
library(ggplot2)
y_corona %>%
  filter(continent %in% "Asia") %>%
  ggplot() +
  aes(x = total_cases, y = total_deaths) +
  geom_line(size = 0.5, colour = "#B22222") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Toplam vaka", 
       y = "Toplam ölüm sayısı", title = "Vaka / Ölüm Çizgi Grafiği", subtitle = "Asia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Asianın Milyon kişi başına yeni ölüm sayısı YOĞUNLUK GRAFİĞİ
library(dplyr)
library(ggplot2)
y_corona %>%
 filter(continent %in% "Asia") %>%
 ggplot() +
 aes(x = new_deaths_per_million) +
 geom_density(adjust = 1L, fill = "#112446") +
 scale_x_continuous(trans = "log") +
 labs(x = "Milyon kişi başına yeni ölüm sayısı", y = "Yoğunluk", title = "Milyona oranla yeni ölüm sayısının yoğunluk grafiği", 
 subtitle = "Asia") +
 theme_minimal() +
 theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Asianın günlere göre yeni vaka sayısının BAR GRAFİĞİ:
library(ggplot2)
ggplot(Asia) +
 aes(x = date, weight = new_cases) +
 geom_bar(fill = "#B22222") +
 labs(x = "Günler", y = "Vaka sayısı", 
 title = "Haziran ayındaki yeni vaka sayıları", subtitle = "Asia") +
 theme_minimal()








###EUROPE###
#Europe'un toplam vaka / toplam ölüm ÇİZGİ GRAFİĞİ:
library(dplyr)
library(ggplot2)
y_corona %>%
 filter(continent %in% "Europe") %>%
 ggplot() +
 aes(x = total_cases, y = total_deaths) +
 geom_line(size = 0.5, colour = "#B22222") +
 labs(x = "Toplam vaka", y = "Toplam ölüm", title = "Toplam vaka / Toplam ölüm ") +
 theme_minimal()

#Europ'un Milyon kişi başına yeni ölüm sayısı YOĞUNLUK GRAFİĞİ
library(dplyr)
library(ggplot2)
y_corona %>%
 filter(continent %in% "Europe") %>%
 ggplot() +
 aes(x = new_deaths_per_million) +
 geom_density(adjust = 1L, fill = "#FF8C00") +
 scale_x_continuous(trans = "log") +
 labs(x = "Milyon kişi ölüm oranı", y = "Yoğunluk", title = "Milyon kişi başına düşen yeni ölüm sayısı") +
 theme_minimal()

#Europe'un günlere göre yeni vaka sayısının BAR GRAFİĞİ:
library(ggplot2)
ggplot(Europe) +
 aes(x = date, weight = new_cases) +
 geom_bar(fill = "#228B22") +
 theme_minimal()









###NORTH AMERICA###
#NAmerica toplam vaka / toplam ölüm ÇİZGİ GRAFİĞİ:
library(ggplot2)
ggplot(North_America) +
 aes(x = total_cases, y = total_deaths) +
 geom_line(size = 0.5, colour = "#B22222") +
 labs(x = "Toplam Vaka", y = "Toplam Ölüm", title = "Toplam Vaka / Toplam Ölüm grafiği", subtitle = "NAmerica") +
 theme_minimal()

#NAmerica Milyon kişi başına yeni ölüm sayısı YOĞUNLUK GRAFİĞİ
library(ggplot2)
ggplot(North_America) +
 aes(x = new_deaths_per_million) +
 geom_density(adjust = 1L, fill = "#EF562D") +
 scale_x_continuous(trans = "log") +
 theme_minimal()

#NAmerica günlere göre yeni vaka sayısının BAR GRAFİĞİ:
library(ggplot2)
ggplot(North_America) +
 aes(x = date, weight = new_cases) +
 geom_bar(fill = "#440154") +
 labs(x = "Günler", 
 y = "Yoğunluk", title = "Günlere göre yoğunluk grafiği", subtitle = "NAmerica") +
 theme_minimal()








###SOUTH AMERICA###
#SAmerica toplam vaka / toplam ölüm ÇİZGİ GRAFİĞİ:
library(dplyr)
library(ggplot2)
South_America %>%
 filter(total_cases >= 284703L & total_cases <= 1368195L) %>%
 ggplot() +
 aes(x = total_cases, y = total_deaths) +
 geom_line(size = 0.5, colour = "#FF8C00") +
 labs(x = "Toplam Vaka", y = "Toplam Ölüm", title = "Toplam vaka / Toplam ölüm ", subtitle = "SAmerica") +
 theme_minimal()

#SAmerica Milyon kişi başına yeni ölüm sayısı YOĞUNLUK GRAFİĞİ
library(ggplot2)
ggplot(South_America) +
 aes(x = new_deaths_per_million) +
 geom_density(adjust = 1L, fill = "#112446") +
 scale_x_continuous(trans = "log") +
 theme_minimal()

#SAmerica günlere göre yeni vaka sayısının BAR GRAFİĞİ:
library(ggplot2)
ggplot(South_America) +
 aes(x = date, weight = new_cases) +
 geom_bar(position = "dodge", fill = "#112446") +
 labs(x = "Günler", y = "Yeni Vaka", title = "Günlere göre yeni vaka sayısı") +
 theme_minimal()









###AFRICA
#Africa toplam vaka / toplam ölüm ÇİZGİ GRAFİĞİ:
library(dplyr)
library(ggplot2)
africa %>%
 filter(total_cases >= 70217L & total_cases <= 144264L) %>%
 ggplot() +
 aes(x = total_cases, y = total_deaths) +
 geom_line(size = 0.5, colour = "#B22222") +
 labs(x = "Toplam Vaka", y = "Toplam Ölüm", subtitle = "Africa") +
 theme_minimal()

#Africa Milyon kişi başına yeni ölüm sayısı YOĞUNLUK GRAFİĞİ
library(ggplot2)
ggplot(africa) +
 aes(x = new_deaths_per_million) +
 geom_density(adjust = 1.4, fill = "#FF69B4") +
 scale_x_continuous(trans = "log") +
 theme_minimal()

#Africa günlere göre yeni vaka sayısının BAR GRAFİĞİ:
esquisse::esquisser(data=africa)

