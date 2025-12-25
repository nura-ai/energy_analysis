library("tidyverse")
library("networkD3")
library("htmlwidgets")
library("renv")
renv::init()

energy <- read_tsv("data/estat_nrg_bal_s.tsv")
glimpse(energy)

#names(energy)   #shows all var.?
#colnames(energy)

energy_raw <- read_tsv("data/estat_nrg_bal_s.tsv")
#разделяем первую объединенную колонку (она всегда первая и с \ or /)
energy <- energy_raw %>%
  separate(col = 1,  #first column
           into = c("freq", "nrg_bal", "siec", "unit", "geo"),
           sep = ",") %>%
  #now pivoting: years in col TIME_PERIOD, znacheniya in values
  pivot_longer(cols = -c(freq, nrg_bal, siec, unit, geo),  #vse krome etih
               names_to = "TIME_PERIOD",
               values_to = "value_flag") %>% #vremenno s flagom
  separate(value_flag, into = c("values", "flag"), sep = "\\s+", fill = "right") %>% 
  #razdelyem po probelam, flag mojet otsutstvovat'
  
  #убираем возможные проблемы и пробразуем типы
  mutate(TIME_PERIOD = as.integer(TIME_PERIOD),
         values = as.numeric(values), #":" ili pusto stanet NA avtomaticheski
         flag = if_else(flag == "", NA_character_, flag)) 
#pustye flagi ->NA

#если данные только annual (yearly), freq will be "A" evrywhere- u may check it
glimpse(energy)

#Шаг 1. Убираем мусор
energy_clean <- energy %>%
  filter(freq=="A") %>%         #only yearly datas
  select(
    geo,
    TIME_PERIOD,
    nrg_bal,
    siec,
    unit,
    values
  )
glimpse(energy_clean)

#смотрим какие категории есть
unique(energy_clean$nrg_bal)

#шаг 3. сфокусируемсяБ иначе утонем, для первого анализа выбираем 1-2 страны, например IY, DE
#1-2 типа энергии (TOTAL, E7000 etc)
#1-2 баланса (production vs final consumption)
energy_focus <- energy_clean %>%
  filter(
    geo %in% c("IT", "DE"),
    siec == "TOTAL",
    nrg_bal %in% c("GEP", "FC_E")
  )

#шаг 4. ПЕрвая осмысленная агрегация
energy_summary <- energy_focus %>%
  group_by(geo, TIME_PERIOD, nrg_bal) %>%
  summarise(
    total_energy = sum(values, na.rm = TRUE),
    .groups = "drop"
  )

#делаем рентген одного года и одной страны
italy_2024 <- energy %>%
  filter(
    geo=="IT",
    TIME_PERIOD==2024,
    unit=="GWH"
  )
glimpse(italy_2024)

italy_2024 %>%
  select(nrg_bal, values) %>%
  arrange(desc(values))

italy_2024 %>%
  filter(nrg_bal %in% c(
    "TI_EHG_MAP",
    "DL",
    "FC_E",
    "GEP"
  ))

italy_2024 %>%
  filter(nrg_bal %in% c("GEP", "FC_E", "DL", "TI_EHG_MAP")) %>%
  select(nrg_bal, values)

#шаг Б. Sankey для Италии 2024- минимально и осмысленно 
#что нам нужно концептуально, узлы(nodes), потоки (links), 2 таблицы

nodes <- tibble(
  name = c(
    "Energy supply",
    "Transformation",
    "Transformation losses",
    "Distribution",
    "Distribution losses",
    "Final energy consumption"
  )
)

supply_value <- 100
transformation_losses <- 40
distribution_value <- 60
distribution_losses <- 10
final_consumption <- 50
#links (potoki)
links <- tibble(
  source = c(
    "Energy supply",
    "Transformation",
    "Transformation",
    "Distribution",
    "Distribution"
  ),
  target = c(
    "Transformation",
    "Transformation losses",
    "Distribution",
    "Distribution losses",
    "Final energy consumption"
  ),
  value = c(
    supply_value,
    transformation_losses,
    distribution_value,
    distribution_losses,
    final_consumption
  )
)

#связываем nodes and links, technical step
links <- links %>%
  mutate(
    source = match(source, nodes$name) - 1,
    target = match(target, nodes$name) - 1
  )

#проверяем, нет ли NA (должно быть всё ок)
print(links) #если есть NA значит еще опечатка

#сохраняем Sankey в объект
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12
 
   #0 для простых диаграмм- быстрее и стабильнее
)

#открыть в браузере
saveWidget(sankey, "italy_2024_sankey.html", selfcontained = TRUE)
browseURL("italy_2024_sankey.html")

#3. Cчитаем ЧИСЛЕННО (ВСЕ ПОТЕРИ)
#ШАг 1. Вытащить все потери
losses_it_2024 <- energy_focus %>%
  filter(
    geo == "IT",
    TIME_PERIOD == 2024,
    unit == "GWH",
    siec == "TOTAL",
    nrg_bal %in% c("TI_EHG_MAP", "DL", "STATDIFF")
  ) %>%
  group_by(nrg_bal) %>%
  summarise(
    loss_gwh = sum(values, na.rm = TRUE),
    .groups = "drop"
  )

#ШАГ 2. Посчитать общие потери
total_losses <- sum(losses_it_2024$loss_gwh)

#ШАГ 3. Для каждого типа потерь
losses_share <- losses_it_2024 %>%
  mutate(
    share = loss_gwh / total_losses * 100
  )
#проверка
energy_focus %>%
  filter(geo == "IT") %>%
  distinct(TIME_PERIOD) %>%
  arrange(desc(TIME_PERIOD))

energy_focus %>% 
  filter(geo == "IT") %>%
  distinct(nrg_bal) %>%
  arrange(nrg_bal)

energy_focus %>%
  filter(geo == "IT") %>%
  distinct(siec)

# ШАГ 4- агрегируем Италию 2024 целиком
it_2024 <- energy_focus %>%
  filter(
    geo == "IT",
    TIME_PERIOD == 2024,
    siec == "TOTAL"
  ) %>%
  group_by(nrg_bal) %>%
  summarise(
    value = sum(values, na.rm = TRUE),
    .groups = "drop"
  )

print(it_2024)

#ШАГ 5. Считаем не явные потери
#ПОТЕРИ = GEP - FC_E
losses_2024 <- it_2024 %>%
  summarise(
    GEP = value[nrg_bal == "GEP"],
    FC_E = value[nrg_bal == "FC_E"],
    losses = GEP - FC_E
  )

print(losses_2024)

#ШАГ 6. доли
losses_share <- losses_2024 %>%
  mutate(
    share_losses = losses/GEP,
    share_FC = FC_E/GEP
  )