#pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(stringr)
library(stringi)
library(geobr)

#calcular área por bairro
bairros_populacao_simplificados <- bairros_populacao_simplificados %>%
  mutate(area_m2 = st_area(geometry))
#transformar em km²
bairros_populacao_simplificados <- bairros_populacao_simplificados %>%
  mutate(area_km2 = as.numeric(area_m2)/10^6)
#mutate calcula a área de cada bairro através da função st_area, dividimos por 10^6 para converter em km², o resultado deve ser salvo em uma nova coluna area_km2
str(bairros_populacao_simplificados)
head(bairros_populacao_simplificados)

#densidade populacional [pessoas por km²]
bairros_populacao_simplificados <- bairros_populacao_simplificados %>%
  mutate(densidade_populacional = Populacao_2022 / area_km2)
#o mutate cria uma nova coluna com os dados desejados de forma direta, assim:

tmap_options(max.categories = 63)
breaks_densidade <- c(0, 500, 1000, 2000, 3000, 5000, 7000, 9000, 10000, 12000, 14000, 16000)
labels_densidade <- c("0 à 500", "501 à 1000", "1001 à 2000", "2001 à 3000", "3001 à 5000", "5001 à 7000", "7001 à 9000", "9001 à 10000", "10001 à 12000", "12001 à 14000", "14001 à 16000")

# Criar o cartograma usando a nova tabela de dados
tm_shape(bairros_populacao_simplificados) +
  tm_polygons("densidade_populacional", title = "Densidade Populacional por Bairro",
              style = "fixed",
              breaks = breaks_densidade,
              labels = labels_densidade,
              palette = "Blues",
              legend.format = list(digits = 0),
              textNA = "Faltantes") +
  tm_borders() +
  tm_layout(title = "Densidade Populacional por Bairro de João Pessoa - CENSO 2022")
tmap_mode("view")