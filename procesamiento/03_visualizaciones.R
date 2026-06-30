# Visualizaciones Encuesta Casen 2024

## Cargar librerías
library(tidyverse)
library(survey)
library(srvyr)

## Cargar datos y análisis previos
source("procesamiento/01_procesamiento.R")
source("procesamiento/02_analisis.R")

## Tema y colores de los graficos 
theme_set(theme_minimal())
colores <- c("#4575b4", "#d73027")


## 8. Visualizaciones

## Gráfico 1: Hipótesis 1 (% hacinamiento según zona rural o urbana)
ggplot(prop_hacinamiento, aes(x = zona, y = prop, fill = zona)) +
  geom_col(width = 0.5, color = "black") + # geom_col en vez de geom_bar
  scale_fill_manual(
    values = c("Urbano" = "#4575b4", "Rural" = "#d73027"),
    name = "Zona"
  ) +
  
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  labs(
    title = "Porcentaje de hogares en situación de hacinamiento",
    subtitle = "Comparación Urbano vs Rural (Casen 2024)",
    y = "Porcentaje (%)",
    x = "Zona Geográfica"
  ) +
  theme_minimal()

## Guardar el grafico 1
g1 <- ggplot(prop_hacinamiento, aes(x = zona, y = prop, fill = zona)) +
  geom_col(width = 0.5, color = "black") + 
  scale_fill_manual(
    values = c("Urbano" = "#4575b4", "Rural" = "#d73027"),
    name = "Zona"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  labs(
    title = "Porcentaje de hogares en situación de hacinamiento",
    subtitle = "Comparación Urbano vs Rural (Casen 2024 - Datos Ponderados)",
    y = "Porcentaje (%)",
    x = "Zona Geográfica"
  ) +
  theme_minimal()

ggsave("output/grafico_hacinamiento.png", plot = g1, width = 8, height = 6, dpi = 300)



## Gráfico 2: Hipótesis 2 (% saneamiento según zona rural o urbana)
prop_saneamiento <- diseno %>%
  group_by(zona) %>%
  summarise(prop = survey_mean(saneamiento_deficitario, na.rm = TRUE)) %>%
  mutate(prop = prop * 100)


g2 <- ggplot(prop_saneamiento, aes(x = zona, y = prop, fill = zona)) +
  geom_col(width = 0.5, color = "black") + 
  scale_fill_manual(
    values = c("Urbano" = "#4575b4", "Rural" = "#d73027"),
    name = "Zona"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + 
  labs(
    title = "Porcentaje de hogares con saneamiento deficitario",
    subtitle = "Comparación Urbano vs Rural (Casen 2024 - Datos Ponderados)",
    y = "Porcentaje (%)",
    x = "Zona Geográfica"
  ) +
  theme_minimal()

print(g2)

## Guardar gráfico 2
ggsave(filename = "output/grafico_saneamiento.png", plot = g2, width = 8, height = 6, dpi = 300)



### Analisis exploratorio del hacinamiento por region ###

## Gráfico 3 (% de hacinamiento por zona y región)
prop_region_hac <- svyby(~hacinado, ~zona + region, diseno, svymean)

ggplot(prop_region_hac, aes(x = factor(region), y = hacinado, fill = zona)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#4575b4", "#d73027"), name = "Zona") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Hacinamiento por región y zona",
    subtitle = "CASEN 2024 - Chile",
    x = "Región",
    y = "Proporción hacinados"
  ) +
  theme_minimal()


prop_region_hac <- svyby(~hacinado, ~zona + region, diseno, svymean)
prop_region_hac <- prop_region_hac %>%
  mutate(region = factor(region, levels = 1:16,
    labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
               "Valparaíso", "O'Higgins", "Maule", "Biobío",
               "Araucanía", "Los Lagos", "Aysén", "Magallanes",
               "Metropolitana", "Los Ríos", "Arica", "Ñuble")))

png("output/grafico_hacinamiento_region.png", width = 1000, height = 600)
ggplot(prop_region_hac, aes(x = region, y = hacinado, fill = zona)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#4575b4", "#d73027"), name = "Zona") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Hacinamiento por región y zona",
       subtitle = "CASEN 2024 - Chile",
       x = "Región", y = "Proporción hacinados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

## Gráfico 4 (% de saneamiento deficitario por zona y región)
prop_region_san <- svyby(~saneamiento_deficitario, ~zona + region, diseno, svymean)
prop_region_san <- prop_region_san %>%
  mutate(region = factor(region, levels = 1:16,
    labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
               "Valparaíso", "O'Higgins", "Maule", "Biobío",
               "Araucanía", "Los Lagos", "Aysén", "Magallanes",
               "Metropolitana", "Los Ríos", "Arica", "Ñuble")))

png("output/grafico_saneamiento_region.png", width = 1000, height = 600)
ggplot(prop_region_san, aes(x = region, y = saneamiento_deficitario, fill = zona)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#4575b4", "#d73027"), name = "Zona") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Saneamiento deficitario por región y zona",
       subtitle = "CASEN 2024 - Chile",
       x = "Región", y = "Proporción saneamiento deficitario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
