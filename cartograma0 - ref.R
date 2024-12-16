library(dplyr)
library(stringr)
library(readxl)

# Carregar banco de dados
file_path_2000 <- "C:/Users/Uriel Holanda/Documents/txt/UFPB Estatística/CENSO 2010/Tabela 1522.xlsx"
populacao_2000 <- read_excel(file_path_2000, skip = 1)

# Renomear e filtrar usando pipe
populacao_2000 <- populacao_2000 %>%
  filter(!is.na(`Município e Bairro`)) %>%
  rename(Bairros = `Município e Bairro`, Populacao_2000 = Populacao) %>%
  select(Bairros, Populacao_2000) %>%
  filter(!Bairros %in% c("João Pessoa (PB)", "Fonte: IBGE - Censo Demográfico"))

# Remover a parte " - João Pessoa (PB)"
populacao_2000 <- populacao_2000 %>%
  mutate(Bairros = str_replace_all(Bairros, " - João Pessoa \\(PB\\)", "")) %>%
  mutate(Bairros = str_replace_all(str_to_upper(Bairros), "[- ]", "_"))

# Verificação
unique(populacao_2000$Bairros)

# Carregar shapefile e filtrar para João Pessoa
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

# Unir dados populacionais aos espaciais
bairros_populacao_simplificados_2000 <- bairros_simplificados %>%
  left_join(populacao_2000, by = c("NM_BAIRRO" = "Bairros"))

# Verificação final
head(bairros_populacao_simplificados_2000)
unique(bairros_populacao_simplificados_2000$NM_BAIRRO)

# Configuração do tmap
tmap_options(max.categories = 63)
breaks_populacao_2000 <- c(500, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 70000)
labels_populacao_2000 <- c("500 à 1000", "1001 à 2000", "2001 à 3000", "3001 à 5000", "5001 à 10000", 
                           "10001 à 20000", "20001 à 30000", "30001 à 40000", "40001 à 70000")

# Criar o cartograma
tm_shape(bairros_populacao_simplificados_2000) +
  tm_polygons("Populacao_2000", title = "População por Bairro",
              style = "fixed",
              breaks = breaks_populacao_2000,
              labels = labels_populacao_2000,
              palette = "Blues",
              legend.format = list(digits = 0),
              textNA = "Faltante") +
  tm_borders() +
  tm_layout(title = "População por bairro de João Pessoa - CENSO 2000")

# Exibir o cartograma
tmap_mode("view")


