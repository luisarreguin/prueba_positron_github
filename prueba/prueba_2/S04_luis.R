# Sesion 5

library(tidyverse)
library(readxl)

aire_tbl <- 
  as_tibble(airquality) %>% 
  print()

aire_tbl %>% 
  select(Day, Month, Ozone) 

aire_tbl %>% 
  select(Temp:Day)

aire_tbl %>% 
  select(-c(Temp, Wind))

aire_tbl %>%
  filter(Temp > 77 & Month == 5)

aire_tbl %>% 
  filter(!is.na(Ozone))

# Filtro valores por arriba del promedio Temp 
# y que sean de los dias 2, 7 y 11
aire_tbl %>% 
  filter(Temp > mean(Temp)& Day %in% c(2,7,11)) 

aire_tbl %>% 
  mutate(o3_log = log(Ozone), .after = Ozone)

aire_tbl %>% 
  mutate(mes = Month, .after = Month)

aire_tbl %>% 
  mutate(
    dia_mes = str_c(Day, Month, sep = "-"),
    escuela = "ESPM",
    dia_mes = str_c("2024",Day, Month, sep = "-")
  )

aire_tbl %>% 
  select(Day, Month, Ozone, Solar.R) %>% 
  filter(Ozone >= mean(Ozone, na.rm = TRUE) & Month == 7) %>%
  mutate(o3_log = log(Ozone))

aire_tbl %>% 
  summarise(
    media_ozono = mean(Ozone, na.rm = TRUE),
    media_temp = mean(Temp)
  )

aire_tbl %>% 
  group_by(Month) %>% 
  summarise(
    media_ozono = mean(Ozone, na.rm = TRUE),
    media_temp = mean(Temp),
    tot_datos = n(), # con cuantos datos se generan los promedios
    tot_na_o3 = sum(!is.na(Ozone)), # contabilizo el numeor de Na en Ozone
    porcent_mes_o3 = (tot_na_o3 * 100)/tot_datos # estimo el porcentaje con datos
  )

aire_tbl %>% 
  distinct(Month)

aire_tbl %>% 
  arrange(Solar.R)

aire_tbl %>% 
  distinct(Month) %>% 
  arrange(desc(Month)
)


pm <- 
  read_csv("C:/Users/luisr/Desktop/clase_r_2025/clase_r/data/red_manual_particulas_susp.csv", skip = 9) %>% 
  print()

pm %>% 
  distinct(id_parameter)

summary(pm$date)

pm %>% 
  filter(id_parameter == "PM10") %>% 
  arrange(value)
