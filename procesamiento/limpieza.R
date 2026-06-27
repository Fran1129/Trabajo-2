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


# 5. Diseño muestral 
diseno <- casen_hog %>%
  as_survey_design(
    ids = varunit,
    strata = varstrat,
    weights = expr,
    nest = TRUE
  )


# 6. Análisis descriptivo de las variables

## H1: Proporción de hogares hacinados segun zona urbana o rural
prop_hacinamiento <- diseno %>%
  group_by(zona) %>%
  summarise(
    prop = survey_mean(hacinado, na.rm = TRUE),
  ) %>%
  mutate(prop = round(prop * 100, 1))

print(prop_hacinamiento)

## H2: Proporción de saneamiento deficitario segun zona urbana o rural
prop_saneamiento <- diseno %>%
  group_by(zona) %>%
  summarise(
    prop = survey_mean(saneamiento_deficitario, na.rm = TRUE)
  ) %>%
  mutate(prop = round(prop * 100, 1))

print(prop_saneamiento)


# 7. Test de hipótesis 

## H1: Chi-cuadrado variables zona y hacinamiento
test_h1 <- svychisq(~hacinado + zona, design = diseno)
print(test_h1)

## H1: Regresión logística ponderada variables zona y hacinamiento
modelo_h1 <- svyglm(hacinado ~ zona, design = diseno, family = quasibinomial())
summary(modelo_h1)
exp(coef(modelo_h1))  


## H2: Chi-cuadrado variables saneamiento deficitario y zona
test_h2 <- svychisq(~saneamiento_deficitario + zona, design = diseno)
print(test_h2)

## H2: Regresión logística ponderada variables saneamiento deficitario y zona
modelo_h2 <- svyglm(saneamiento_deficitario ~ zona, design = diseno, family = quasibinomial())
summary(modelo_h2)
exp(coef(modelo_h2))  


# 8. Visualizaciones

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

### Guardar Gráfico 3
png("output/grafico_hacinamiento_region.png", width = 800, height = 600)



# 9. Análisis exploratorio por región

## Hacinamiento por zona y región
prop_region_hac <- svyby(~hacinado, ~zona + region, diseno, svymean)
prop_region_hac <- prop_region_hac %>%
  mutate(region = factor(region, levels = 1:16,
    labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
               "Valparaíso", "O'Higgins", "Maule", "Biobío",
               "Araucanía", "Los Lagos", "Aysén", "Magallanes",
               "Metropolitana", "Los Ríos", "Arica", "Ñuble")))

ggplot(prop_region_hac, aes(x = region, y = hacinado, fill = zona)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#4575b4", "#d73027"), name = "Zona") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Hacinamiento por región y zona",
       subtitle = "CASEN 2024 - Chile",
       x = "Región", y = "Proporción hacinados") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Saneamiento por zona y región
prop_region_san <- svyby(~saneamiento_deficitario, ~zona + region, diseno, svymean)
prop_region_san <- prop_region_san %>%
  mutate(region = factor(region, levels = 1:16,
    labels = c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
               "Valparaíso", "O'Higgins", "Maul e", "Biobío",
               "Araucanía", "Los Lagos", "Aysén", "Magallanes",
               "Metropolitana", "Los Ríos", "Arica", "Ñuble")))

ggplot(prop_region_san, aes(x = region, y = saneamiento_deficitario, fill = zona)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#4575b4", "#d73027"), name = "Zona") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Saneamiento deficitario por región y zona",
       subtitle = "CASEN 2024 - Chile",
       x = "Región", y = "Proporción saneamiento deficitario") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

