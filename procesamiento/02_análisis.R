# Análisis Casen 2024

## Cargar librerías
library(tidyverse)
library(survey)
library(srvyr)

## Cargar datos procesados
source("procesamiento/01_procesamiento.R")


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
  mutate(across(c(prop, prop_se), ~ round(.x * 100, 1)))

print(prop_hacinamiento)

## H2: Proporción de saneamiento deficitario segun zona urbana o rural
prop_saneamiento <- diseno %>%
  group_by(zona) %>%
  summarise(
    prop = survey_mean(saneamiento_deficitario, na.rm = TRUE)
  ) %>%
  mutate(across(c(prop, prop_se), ~ round(.x * 100, 1)))

print(prop_saneamiento)


# 7. Test de hipótesis 

## H1: Chi-cuadrado variables zona y hacinamiento
test_h1 <- svychisq(~hacinado + zona, design = diseno)
print(test_h1)

## H1: Regresión logística ponderada variables zona y hacinamiento
modelo_h1 <- svyglm(hacinado ~ zona, design = diseno, family = quasibinomial())
summary(modelo_h1)
exp(cbind(OR = coef(modelo_h1), confint(modelo_h1)))


## H2: Chi-cuadrado variables saneamiento deficitario y zona
test_h2 <- svychisq(~saneamiento_deficitario + zona, design = diseno)
print(test_h2)

## H2: Regresión logística ponderada variables saneamiento deficitario y zona
modelo_h2 <- svyglm(saneamiento_deficitario ~ zona, design = diseno, family = quasibinomial())
summary(modelo_h2)
 exp(cbind(OR = coef(modelo_h2), confint(modelo_h2)))  


### Analisis exploratorio del hacinamiento por region ###

## Modelo ponderado con interaccion zona x region (region como factor)
modelo_h3 <- svyglm(hacinado ~ zona * factor(region),
                    design = diseno, family = quasibinomial())
summary(modelo_h3)

## Test global de la interacción: ¿el efecto de zona difiere entre regiones?
regTermTest(modelo_h3, ~ zona:factor(region))
