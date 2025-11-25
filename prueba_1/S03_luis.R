# ============================================================================ #
# Escriba aquí la finalidad del script
# Proyecto:
# Autor: Tex
# Fecha:
# ============================================================================ #

# Paquetes a utilizar
library(tidyverse)
library(haven)
library(readxl)
library(foreign)
library(janitor)

# llamar a una malla precargada
aire <- airquality

# Nos dice que es lo que tiene el objeto
is(aire)

# imprimir en pantalla
aire

# estructura del dataframe
str(aire)

# ver los primeros 6 filas
head(aire)

# estadisticas de resumen
summary(aire)

# solo una columna
summary(aire$Temp)

# Convertimos a tibble
aire_tbl <- 
  as_tibble(aire) %>% 
  print()

print(aire_tbl, n = 20)


# generar 1 grafico

ggplot(aire_tbl) +
  geom_line(aes(Day, Temp)) +
  theme_bw()

ggplot(aire_tbl, aes(Day, Temp)) +
  geom_line() +
  geom_point() +
  theme_bw()

ggplot(aire_tbl) 

ggplot(aire_tbl, aes(Temp))

ggplot(aire_tbl, aes(Temp)) +
  geom_histogram() +
  theme_bw()

ggplot(aire_tbl) +
  geom_histogram(aes(Temp)) +
  theme_bw()

ggplot(aire_tbl, aes(Temp)) +
  geom_histogram(color = "black", fill = "grey80") +
  theme_bw()

ggplot(aire_tbl, aes(Temp)) +
  geom_histogram(color = "black", fill = "#DDA0DD") +
  theme_bw()

ggplot(aire_tbl, aes(Temp)) +
  geom_density(color = "black", fill = "#708ab4ff", alpha = 0.5) +
  theme_bw()

ggplot(aire_tbl, aes(Temp)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey80") +
  geom_density(color = "black", fill = "#708ab4ff", alpha = 0.5) +
  theme_light()

ggplot(aire_tbl, aes(y = Temp)) +
  geom_boxplot() +
  theme_light()

ggplot(aire_tbl, aes(Temp)) +
  geom_boxplot() +
  theme_bw()

ggplot(aire_tbl, aes(Temp)) +
  geom_boxplot() +
  coord_flip() +
  theme_light()



cdmx_dbf <- read.dbf("./data/09mun.dbf")

iris_dta <- read_dta("./data/iris.dta")

redma_csv <- read_csv("./data/red_manual_particulas_susp.csv", skip = 9)

pob_xlsx <- read_xlsx("./data/datos_pob_2020.xlsx")

pob_xlsx <- read_xlsx("./data/datos_pob_2020.xlsx", sheet = "contam", .name_repair = "universal")

pob_xlsx <- read_xlsx("./data/datos_pob_2020.xlsx", sheet = "contam") %>% 
  clean_names() %>% 
  print()

summary(pob_xlsx$`ID estación monitoreo`)
