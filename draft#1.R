library(tidyverse)

data <- read_tsv("estat_nrg_cb_e.tsv")
getwd(energy_analysis)

head(data)
str(data)
summary()

data_selected <- data %>% #затем, select()-выбор нужных колонок
  select(country, innovation_index, r_d_expenditure)
#filer() - отбор наблюдений, 
data_filtered <- data %>%
  filtered(innovation_index > 0.5)
#Логика: -> это логические векторы
       #.-> TRUE остается, FALSE уходит

#mutate()- создать новую переменную
data_new <- %>%
  mutate(high_innovation = innovation_index > 0.6)
#новый вектор столбец

#group_by()+summatise()- бизнес-смысл
data %>%
  group_by(region) %>%
  summarise(
    avg_innovation = mean(innovation_index, na.rm = TRUE),
    n = n()
  )

#data=таблицы
#columns=vectors
#functions work on vectors
#dplyr makes it readble 

#задание
#выбрать три колонки
#отфильтровать по одному условию
#создать новую переменную через mutate()
#сделать summary()
