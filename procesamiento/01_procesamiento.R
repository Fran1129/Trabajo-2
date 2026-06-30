# Procesamiento de datos Encuesta Casen 2024

# 1. Cargar librerías e instalar paquetes
library(tidyverse)
library(haven)
library(survey)      
library(srvyr)       
library(janitor)

install.packages(c("survey", "srvyr", "janitor"))


# 2. Cargar base de datos original
casen_raw <- read_dta("input/data-orig/casen_2024.dta")


# 3. Filtrar y seleccionar variables relevantes
casen_hog <- casen_raw %>%
  distinct(folio, .keep_all = TRUE) %>%   # 1 respuesta por hogar 
  select(folio, ind_hacina, ind_san, area, region, expr, varstrat, varunit) %>%
  filter(ind_hacina != -88)               # excluir "no sabe"

# 4. Recodificación
casen_hog <- casen_hog %>%
  mutate(
    # H1: hacinado = categorías 2, 3 o 4 (cualquier nivel de hacinamiento)
    hacinado = if_else(ind_hacina %in% c(2, 3, 4), 1, 0),

    # H2: saneamiento deficitario = categoría 2
    saneamiento_deficitario = if_else(ind_san == 2, 1, 0),

    # Zona como factor con etiquetas
    zona = factor(area, levels = c(1, 2), labels = c("Urbano", "Rural"))
  )
