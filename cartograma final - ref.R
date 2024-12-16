#pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(stringr)
library(stringi)
library(geobr)

#banco de dados
file_path <- "C:/Users/Uriel Holanda/Documents/txt/UFPB Estatística/CENSO 2010/Censo 2022_dados população por bairros em JP.xlsx"
populacao <- read_excel(file_path, skip = 1)
colnames(populacao)

#renomear as colunas
populacao <- populacao %>%
  rename(Bairros = `Bairros`, Populacao_2022 = `População Total`) %>%
  select(Bairros, Populacao_2022)
str(populacao)
populacao <- populacao %>%
  filter(!is.na(`Populacao_2022`) & !Bairros %in% c(NA, "Fonte: Censos Demográficos"))
#transformar em numeric
populacao$Populacao_2022 <- as.numeric(populacao$Populacao_2022)
str(populacao$Populacao_2022)

#padronizar bairros banco de dados
populacao <- populacao %>%
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))
str(populacao)
#padronizar bairros espaciais
str(bairros_joao_pessoa)
bairros_joao_pessoa <- bairros_joao_pessoa %>%
  mutate(NM_BAIRRO = str_replace_all(str_to_upper(NM_BAIRRO), "[- ]", "_"))

#unir setores censitários
bairros_simplificados <- bairros_joao_pessoa %>%
  group_by(NM_BAIRRO) %>%
  summarize(geometry = st_union(st_make_valid(geometry))) %>%
  ungroup()
str(bairros_simplificados)

#unir os dados
bairros_populacao_simplificados <- bairros_simplificados %>%
  left_join(populacao, by = c("NM_BAIRRO" = "Bairros"))
head(bairros_populacao_simplificados)

#carregar cartograma!
tmap_options(max.categories = 63)
breaks_populacao <- c(500, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 70000)
labels_populacao <- c("500 à 1000", "1001 à 2000", "2001 à 3000", "3001 à 5000", "5001 à 10000", "10001 à 20000", "20001 à 30000", "30001 à 4000", "40001 à 70000")

# Criar o cartograma usando a nova tabela de dados
tm_shape(bairros_populacao_simplificados) +
  tm_polygons("Populacao_2022", title = "População por Bairro",
              style = "fixed",
              breaks = breaks_populacao,
              labels = labels_populacao,
              palette = "Blues",
              legend.format = list(digits = 0),
              textNA = "Faltantes") +
  tm_borders() +
  tm_layout(title = "População por bairro de João Pessoa - CENSO 2022")


