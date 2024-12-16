#pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(stringr)
library(stringi)
library(geobr)

file_path_2000 <- "./input/base/Tabela 1522.xlsx"
populacao_2000 <- read_excel(file_path_2000, skip = 1)

names(populacao_2000)

#renomear e selecionar colunas de interesse
populacao_2000 <- populacao_2000 %>%
  rename(Bairros = `Município e Bairro`, populacao_2000 = `Populacao`) %>%
  select(Bairros, populacao_2000)

#transformar a coluna Populacao_2000 em numérica contínua
populacao_2000$populacao_2000 <- as.numeric(populacao_2000$populacao_2000)

populacao_2000 <- populacao_2000 %>%
  filter(Bairros != "Fonte: IBGE - Censo Demográfico")

head(populacao_2000)
  
#padronizar nome dos bairros
populacao <- populacao %>%
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))

# Padronizar nomes dos bairros nos dados de 2000
# Remover a parte " - João Pessoa (PB)" dos nomes dos bairros na tabela de 2000
populacao_2000 <- populacao_2000 %>%
  filter(Bairros != "Fonte: IBGE - Censo Demográfico") %>%  # Excluir a linha com a fonte
  mutate(Bairros = str_replace_all(Bairros, " - João Pessoa \\(PB\\)", "")) %>%  # Remover a parte do nome
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))  # Padronizar

# Verificar os nomes dos bairros após o ajuste
unique(populacao_2000$Bairros)
# Verificar correspondência de nomes após ajustes
setdiff(unique(populacao$Bairros), unique(populacao_2000$Bairros))
setdiff(unique(populacao_2000$Bairros), unique(populacao$Bairros))

# Ajustar os nomes dos bairros na tabela de 2000
populacao_2000 <- populacao_2000 %>%
  mutate(Bairros = ifelse(Bairros == "JOÃO_PESSOA_(PB)", "JOÃO_PESSOA", Bairros),
         Bairros = ifelse(Bairros == "MUÇUMAGRO", "MUCUMAGO", Bairros))

#união dos dados 2000-2022
populacao_comparativa0022 <- populacao %>%
  left_join(populacao_2000, by = "Bairros")

#calcular diferença e razão
populacao_comparativa0022 <- populacao_comparativa0022 %>%
  mutate(diferenca_populacional = Populacao_2022 - populacao_2000,
         razao_populacional = Populacao_2022 / populacao_2000)

#unir dados espaciais e especiais
bairros_comparativos0022 <- bairros_populacao_simplificados %>%
  left_join(populacao_comparativa0022, by = c("NM_BAIRRO" = "Bairros"))

#detalhes para o cartograma
tmap_options(max.categories = 63)
cores <- c("blue", "lightblue", "white", "pink", "red")
breaksr <- c(0, 0.5, 1, 2, 5, 10, 20, 30, 45)
labelsr <- c("0 à 0.5", "0.51 à 1", "1.01 à 2", "2.01 à 5", "5.01 à 10", "10.01 à 20", "20.01 à 30", "30.01 à 45")
breaksd <- c(-10000, -5000, -1000, -500, 0, 500, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 50000, 70000)
labelsd <- c("-10000 à -5000", "-5000 à -1000", "-1000 à -500", "-500 à 0", "0 à 500", "501 à 1000", "1001 à 2000", "2001 à 3000", "3001 à 5000", "5001 à 10000", "10001 à 20000", "20001 à 30000", "30001 à 40000", "40001 à 50000", "50001 à 70000")

#cartograma de diferença populacional
tm_shape(bairros_comparativos0022) +
  tm_polygons("diferenca_populacional", title = "Diferença Populacional (2000-2022)",
              style = "fixed",
              breaks = breaksd,
              labels = labelsd,
              palette = cores, midpoint = 0,
              legend.format = list(digits = 0),
              textNA = "Faltante") +
  tm_borders() +
  tm_layout(title = "Diferença Populacional por Bairro de João Pessoa - CENSO 2000-2022")

tm_shape(bairros_comparativos0022) +
  tm_polygons("razao_populacional", title = "Razão Populacional (2000-2022)",
              style = "fixed",
              breaks = breaksr,
              labels = labelsr,
              palette = "Blues", midpoint = 1,
              legend.format = list(digits = 2),
              textNA = "Faltante") +
  tm_borders() +
  tm_layout(title = "Razão Populacional por Bairro de João Pessoa - CENSO 2000-2022")