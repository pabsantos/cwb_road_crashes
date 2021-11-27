
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)
library(geobr)
library(tmap)

# Import data -------------------------------------------------------------

crashes <- read_csv("input/TRANSITO_ACIDENTES_FATAIS (GIS.TRANSITO_ACIDENTES_FATAIS)+ (GIS)_Dados migrados.csv")

# Tidy --------------------------------------------------------------------

crashes_sf <- crashes %>% 
  filter(Municipio == "CURITIBA") %>% 
  select(ANO, LAT_SIRGAS, LON_SIRGAS, `Tp Especie`, `Tp Acidente`, Sexo, 
         starts_with("Cs")) %>% 
  rename(TP_VEIC = `Tp Especie`, TP_ACID = `Tp Acidente`, SEX = Sexo, 
         CS_ALC = `Cs Alcool`, CS_DS = `Cs Desrep Sinaliz`, 
         CS_INF = `Cs Infraestrura`, CS_PROIB = `Cs Trans Loc Proib`, 
         CS_VEL = `Cs Velocidade`, CS_INAP = `Cs Trans Loc Impr`) %>%
  st_as_sf(coords = c("LON_SIRGAS", "LAT_SIRGAS")) %>% 
  st_set_crs(4674)

# EDA ---------------------------------------------------------------------

cwb <- read_municipality(code_muni = 4106902, year = 2010)

## Map - all fatal crashes
ggplot() + 
  geom_sf(data = cwb, color = "grey20") +
  geom_sf(data = crashes_sf, alpha = 0.5, aes(color = TP_VEIC)) + 
  theme_void()

## Count per type
crashes_sf %>% 
  st_drop_geometry() %>% 
  count(TP_VEIC, sort = TRUE) %>% 
  mutate(TP_VEIC = fct_reorder(TP_VEIC, n)) %>% 
  ggplot(aes(x = n, y = TP_VEIC, fill = TP_VEIC)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none")

## Time series
crashes_sf %>% 
  st_drop_geometry() %>% 
  count(ANO) %>% 
  ggplot(aes(ANO, n)) + 
  geom_line() +
  scale_x_continuous(breaks = seq(2010,2020,1), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0,NA)) +
  theme_minimal()

# Pedestrians and cyclists ------------------------------------------------

crashes_sf <- crashes_sf %>% 
  filter(TP_VEIC %in% c("A PÉ", "BICICLETA"))

## Map
ggplot() + 
  geom_sf(data = cwb, color = "grey20") +
  geom_sf(data = crashes_sf, alpha = 0.5, aes(color = TP_VEIC)) + 
  theme_void()

# Road crashes per census tract -------------------------------------------

cwb_census <- read_census_tract(code_tract = 4106902, year = 2010)

census_data <- read_csv("input/Basico_PR.csv") %>% 
  select(Cod_setor, V002) %>% 
  rename(code_tract = Cod_setor, POPULATION = V002) %>% 
  mutate(code_tract = as.character(code_tract))

census_crashes <- crashes_sf %>% 
  st_join(cwb_census["code_tract"]) %>% 
  st_drop_geometry() %>% 
  count(code_tract)

cwb_census_crashes <- cwb_census %>% 
  left_join(census_crashes, by = "code_tract") %>%
  left_join(census_data, by = "code_tract") %>% 
  replace_na(list(n = 0)) %>% 
  mutate(MORT_RATE = (n / POPULATION) * 100000)

## Road crashes map
tm_shape(cwb_census_crashes) +
  tm_fill(col = "n", style = "cont") +
  tm_borders(col = "Grey20", lwd = 0.2)

## Mortality rate density
cwb_census_crashes %>% 
  ggplot(aes(x = MORT_RATE)) +
  geom_density()

## Crashes density
cwb_census_crashes %>% 
  ggplot(aes(x = n)) +
  geom_density()

## Population
cwb_census_crashes %>% 
  ggplot(aes(x = POPULATION)) +
  geom_density()

# Road crashes per traffic analysis zones ---------------------------------

taz <- st_read("input/taz.gpkg")

## Crashes per taz
taz_crashes <- crashes_sf %>% 
  st_transform(crs = 31982) %>% 
  st_join(taz["cod_taz"]) %>% 
  st_drop_geometry() %>% 
  count(cod_taz)

## Population per taz
taz_pop <- cwb_census_crashes %>% 
  drop_na(POPULATION) %>% 
  select(POPULATION) %>% 
  st_centroid() %>% 
  st_transform(crs = 31982) %>% 
  st_join(taz["cod_taz"]) %>% 
  st_drop_geometry() %>% 
  group_by(cod_taz) %>% 
  summarise(POPULATION = sum(POPULATION))

cwb_taz_crashes <- taz %>% 
  left_join(taz_crashes, by = "cod_taz") %>% 
  left_join(taz_pop, by = "cod_taz") %>% 
  replace_na(list(n = 0)) %>% 
  mutate(MORT_RATE = (n / POPULATION) * 100000)

## Map - road crashes
qtm(cwb_taz_crashes, fill = "n")

## Population distribution
cwb_taz_crashes %>% 
  ggplot(aes(x = POPULATION)) +
  geom_density()
