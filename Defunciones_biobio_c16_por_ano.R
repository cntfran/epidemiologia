# leer archivo CSV
Defunciones_1990_2022 <- read.csv("/Users/macbookair/Downloads/epidemiologia/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES.csv", sep = ";", fileEncoding = "latin1")  # para tildes y ñ

head(Defunciones_1990_2022)

# filtro x año
datos_por_ano <- subset(Defunciones_1990_2022, AÑO %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

# filtro x cáncer gástrico
Defunciones_C16 <- subset(datos_por_ano, grepl("^C16", DIAG1))

# filtro x biobio
Defunciones_C16_Biobio <- subset(Defunciones_C16, NOMBRE_REGION %in% c("Del Bíobío"))

# gpt dos pq el otro no está funcionando lolz
library(dplyr)
library(tidyr)

# clasificar edades
Defunciones_C16_Biobio <- Defunciones_C16_Biobio %>%
  mutate(Rango_edad = case_when(
    EDAD_CANT <= 25 ~ "25 años o menos",
    EDAD_CANT >= 25 & EDAD_CANT <= 29 ~ "25 a 29",
    EDAD_CANT >= 30 & EDAD_CANT <= 34 ~ "30 a 34",
    EDAD_CANT >= 35 & EDAD_CANT <= 39 ~ "35 a 39",
    EDAD_CANT >= 40 & EDAD_CANT <= 44 ~ "40 a 44",
    EDAD_CANT >= 45 & EDAD_CANT <= 49 ~ "45 a 49",
    EDAD_CANT >= 50 & EDAD_CANT <= 54 ~ "50 a 54",
    EDAD_CANT >= 55 & EDAD_CANT <= 59 ~ "55 a 59",
    EDAD_CANT >= 60 & EDAD_CANT <= 64 ~ "60 a 64",
    EDAD_CANT >= 65 & EDAD_CANT <= 69 ~ "65 a 69",
    EDAD_CANT >= 70 & EDAD_CANT <= 74 ~ "70 a 74",
    EDAD_CANT >= 75 & EDAD_CANT <= 79 ~ "75 a 79",
    EDAD_CANT >= 80 ~ "80 y más"
  ))

# Agrupar por rango y año SIN agrupar por sexo (se cuenta todo junto)
resumen_rangos_ano <- Defunciones_C16_Biobio %>%
  group_by(Rango_edad, AÑO) %>%
  summarise(Total_defunciones = n(), .groups = "drop") %>%
  mutate(Rango_edad = factor(Rango_edad, levels = c(
    "25 años o menos", "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49",
    "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74", "75 a 79", "80 y más"
  ))) %>%
  arrange(Rango_edad, AÑO)

# pivotear
tabla_final_REAL <- resumen_rangos_ano %>%
  pivot_wider(names_from = AÑO, values_from = Total_defunciones, values_fill = 0)

getwd()
setwd("/Users/macbookair/Downloads/epidemiologia")

# excel
library(writexl)
write_xlsx(tabla_final_REAL, "Defunciones_C16_Biobio_por_rango_edad.xlsx")


