# Visualizaciones Encuesta Casen 2024

## Cargar librerías
library(tidyverse)
library(survey)
library(srvyr)

## Cargar datos y análisis previos
source("procesamiento/01_procesamiento.R")
source("procesamiento/02_analisis.R")


## 8. Visualizaciones

## Gráfico 1: Hipótesis 1 (hacinamiento por zona)
ggplot(casen_hog, aes(x = zona, fill = factor(hacinado))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("#4575b4", "#d73027"),
    labels = c("Sin hacinamiento", "Con hacinamiento"),
    name = "Situación"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Hacinamiento por zona urbano/rural",
    subtitle = "CASEN 2024 - Chile",
    y = "Proporción",
    x = "Zona"
  ) +
  theme_minimal()

## Gráfico 2: Hipótesis 2 (saneamiento por zona)
ggplot(casen_hog, aes(x = zona, fill = factor(saneamiento_deficitario))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("#4575b4", "#d73027"),
    labels = c("Aceptable", "Deficitario"),
    name = "Situación"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Saneamiento por zona urbano/rural",
    subtitle = "CASEN 2024 - Chile",
    y = "Proporción",
    x = "Zona"
  ) +
  theme_minimal()

## Gráfico 3: Hipótesis 3 (hacinamiento por zona y región)
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


### Guardar Gráfico 1
png("output/grafico_hacinamiento.png", width = 800, height = 600)
ggplot(casen_hog, aes(x = zona, fill = factor(hacinado))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("#4575b4", "#d73027"),
    labels = c("Sin hacinamiento", "Con hacinamiento"),
    name = "Situación"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Hacinamiento por zona urbano/rural",
       subtitle = "CASEN 2024 - Chile",
       y = "Proporción", x = "Zona") +
  theme_minimal()
dev.off()

### Guardar Gráfico 2
png("output/grafico_saneamiento.png", width = 800, height = 600)
ggplot(casen_hog, aes(x = zona, fill = factor(saneamiento_deficitario))) +
  geom_bar(position = "fill") +
  scale_fill_manual(
    values = c("#4575b4", "#d73027"),
    labels = c("Aceptable", "Deficitario"),
    name = "Situación"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Saneamiento por zona urbano/rural",
       subtitle = "CASEN 2024 - Chile",
       y = "Proporción", x = "Zona") +
  theme_minimal()
dev.off()
# 4. Análisis exploratorio por región

## Hacinamiento por región y zona
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

## Saneamiento por región y zona
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