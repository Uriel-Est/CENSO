#pacotes
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(stringr)
library(stringi)
library(geobr)

# Carregar banco de dados
file_path <- "C:/Users/Uriel Holanda/Documents/txt/UFPB Estatística/CENSO 2010/Censo 2022_dados população por bairros em JP.xlsx"
populacao <- read_excel(file_path, skip = 1)

# Renomear e filtrar
populacao <- populacao %>%
  filter(!is.na(Bairros)) %>%
  rename(Bairros = Bairros, Populacao_2010 = `População Total`) %>%
  select(Bairros, Populacao_2010) %>%
  filter(!Bairros %in% c("JOÃO_PESSOA", "FONTE:_CENSOS_DEMOGRÁFICOS."))

# Remover linhas indesejadas e limpar o nome dos bairros
populacao <- populacao %>%
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))

# Carregar shapefile
zip_file <- "C:/Users/Uriel Holanda/Downloads/pb_setores_censitarios.zip"
unzip_dir <- "C:/Users/Uriel Holanda/Downloads/pb_setores_censitarios"
unzip(zip_file, exdir = unzip_dir)
shapefile_path <- file.path(unzip_dir, "25SEE250GC_SIR.shp")
bairros <- st_read(shapefile_path, options = "ENCODING=WINDOWS-1252")

# Filtrar para João Pessoa e simplificar geometria
bairros_joao_pessoa <- bairros %>%
  filter(NM_MUNICIP == "JOÃO PESSOA")

bairros_simplificados <- bairros_joao_pessoa %>%
  group_by(NM_BAIRRO) %>%
  summarize(geometry = st_union(st_make_valid(geometry))) %>%
  ungroup() %>%
  mutate(NM_BAIRRO = str_replace_all(str_to_upper(NM_BAIRRO), "[- ]", "_"))

# Transformar a coluna em numérica contínua
populacao$Populacao_2010 <- as.numeric(populacao$Populacao_2010)

# Unir dados populacionais aos espaciais
bairros_populacao_simplificados <- bairros_simplificados %>%
  left_join(populacao, by = c("NM_BAIRRO" = "Bairros"))

# Verificar junção
head(bairros_populacao_simplificados)
colnames(bairros_populacao_simplificados)

# Configuração do tmap
tmap_options(max.categories = 63)
breaks_populacao <- c(500, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 70000)
labels_populacao <- c("500 à 1000", "1001 à 2000", "2001 à 3000", "3001 à 5000", "5001 à 10000", 
                      "10001 à 20000", "20001 à 30000", "30001 à 40000", "40001 à 70000")

# Criar o cartograma
tm_shape(bairros_populacao_simplificados) +
  tm_polygons("Populacao_2010", title = "População por Bairro",
              style = "fixed",
              breaks = breaks_populacao,
              labels = labels_populacao,
              palette = "Blues",
              legend.format = list(digits = 0),
              textNA = "Faltante") +
  tm_borders() +
  tm_layout(title = "População por bairro de João Pessoa - CENSO 2010")
tmap_mode("view")

