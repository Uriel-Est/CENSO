#pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(stringr)
library(stringi)
library(geobr)

#da mesma forma como fizemos para o cartograma de 2022, vamos padronizar o nome dos bairros e do shp através de mutate
populacao_joao_pessoa <- populacao_joao_pessoa %>%
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))
populacao <- populacao %>%
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))

#unir os dados de 2022 e 2010 para comparação
populacao_comparativa2210 <- populacao_compara %>%
  left_join(populacao_joao_pessoa, by = "Bairros")
str(populacao_comparativa2210)
head(populacao_comparativa2210)

#calcular a diferença populacional
populacao_comparativa2210 <- populacao_comparativa2210 %>%
  mutate(diferenca_populacional = Populacao_2022 - Populacao_2010,
         razao_populacional = Populacao_2022 / Populacao_2010)

#unir dados espaciais e comparativos
bairros_comparativos2210 <- bairros_populacao_simplificados %>%
  left_join(populacao_comparativa2210, by = c("NM_BAIRRO" = "Bairros"))
str(bairros_comparativos2210)

#tmap config
tmap_options(max.categories = 63)
cores <- c("blue", "lightblue", "white", "pink", "red")
breaksr <- c(0.5, 0.75, 0.8, 0.85, 0.9, 1, 1.1, 1.25, 1.5, 2, 2.5, 3)
labelsr <- c("0.5 à 0.75", "0.76 à 0.8", "0.81 à 0.85", "0.86 à 0.9", "0.91 à 1", "1.01 à 1.1", "1.11 à 1.25", "1.26 à 1.5", "1.51 à 2", "2.01 à 2.5", "2.51 à 3")
breaksd <- c(-10000, -5000, -1000, -500, 0, 500, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 50000, 70000)
labelsd <- c("-10000 à -5000", "-5000 à -1000", "-1000 à -500", "-500 à 0", "0 à 500", "501 à 1000", "1001 à 2000", "2001 à 3000", "3001 à 5000", "5001 à 10000", "10001 à 20000", "20001 à 30000", "30001 à 40000", "40001 à 50000", "50001 à 70000")


#cartograma de diferença populacional
tm_shape(bairros_comparativos2210) +
  tm_polygons("diferenca_populacional", title = "Diferença Populacional (2010-2022)",
              style = "fixed",
              breaks = breaksd,
              labels = labelsd,
              palette = cores, midpoint = 0,
              legend.format = list(digits = 0),
              textNA = "Faltantes") +
  tm_borders() +
  tm_layout(title = "Diferença Populacional por Bairro de João Pessoa - CENSO 2010-2022")

#cartograma razão populacional
tm_shape(bairros_comparativos2210) +
  tm_polygons("razao_populacional", title = "Razão Populacional (2010-2022)",
              style = "fixed",
              breaks = breaksr,
              labels = labelsr,
              palette = "Blues", midpoint = 0,
              legend.format = list(digits = 0),
              textNA = "Faltantes") +
  tm_borders() +
  tm_layout(title = "Razão Populacional por Bairro de João Pessoa - CENSO 2010-2022")