# Leer un archivo CSV
Poblacion_2002_2035 <- read.csv("/Users/macbookair/Downloads/epidemiologia/estimaciones-y-proyecciones-2002-2035-comuna-y-área-urbana-y-rural.csv", sep = ";", fileEncoding = "latin1")  # para tildes y ñ

# Especificar los años que quieres conservar
datas_utiles <- paste0("Poblacion.", 2012:2022)

# Especificar otras columnas que quieres conservar (puedes ajustar según necesidad)
otras_columnas <- c("Region", "Nombre.Region", "Provincia", "Nombre.Provincia", 
                    "Comuna", "Nombre.Comuna", "Sexo..1.Hombre.2.Mujer.", 
                    "Area..1.Urbano.2.Rural.", "Grupo.edad")

# Filtrar el data frame para conservar solo esas columnas
Poblacion_2012_2022 <- Poblacion_2002_2035[ , c(otras_columnas, datas_utiles)]

# Agrupanding
Poblacion_2012_2022$Grupo.edad[Poblacion_2012_2022$Grupo.edad %in% 
                                 c("0 Año", "De 1 a 4 años", "De 5 a 9 años", 
                                   "De 10 a 14 años", "De 15 a 19 años", "De 20 a 24 años")] <- "Menores de 25 años"

library(dplyr)
library(tidyr)

# Paso 1: Agrupar "Menores de 25 años"
Poblacion_2012_2022$Grupo.edad[Poblacion_2012_2022$Grupo.edad %in%
                                 c("0 Año", "De 1 a 4 años", "De 5 a 9 años",
                                   "De 10 a 14 años", "De 15 a 19 años", "De 20 a 24 años")] <- "Menores de 25 años"

# Paso 2: Filtrar los años que necesitas
poblacion_filtrada <- Poblacion_2012_2022 %>%
  select(Grupo.edad, Sexo..1.Hombre.2.Mujer., starts_with("Poblacion.20")) %>%
  rename_with(~ gsub("Poblacion.", "", .), starts_with("Poblacion.")) # renombrar columnas a "2012", etc.

# Paso 3: Pivotear a formato largo
poblacion_larga <- poblacion_filtrada %>%
  pivot_longer(cols = `2012`:`2022`, names_to = "Año", values_to = "Poblacion") %>%
  mutate(Sexo = ifelse(Sexo..1.Hombre.2.Mujer. == 1, "hombres", "mujeres"))

# Paso 4: Agrupar y resumir
resumen <- poblacion_larga %>%
  group_by(Año, Grupo.edad, Sexo) %>%
  summarise(Poblacion = sum(Poblacion, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Sexo, values_from = Poblacion) %>%
  mutate(total = hombres + mujeres)

# (Opcional) Ordenar grupo de edad
resumen$Grupo.edad <- factor(resumen$Grupo.edad, levels = unique(resumen$Grupo.edad))

# Mostrar la tabla para un año (ejemplo: 2012)
resumen_2012 <- filter(resumen, Año == "2012")
print(resumen_2012)

resumen_2013 <- filter(resumen, Año == "2013")
print(resumen_2013)

resumen_2014 <- filter(resumen, Año == "2014")
print(resumen_2014)

getwd()
setwd("/Users/macbookair/Downloads/epidemiologia")

# Exportar a Excel
library(writexl)
write_xlsx(resumen, "Resumen_Poblacion_2012_2022.xlsx")

### Voy a hacer ahora la región del Biobío

# Filtro Biobío
datos_biobio <- subset(Poblacion_2002_2035, Region == 8)

# Edades menores a 25 en una pura categoría
datos_biobio$GrupoEdad2 <- ifelse(datos_biobio$Grupo.edad %in% c("0 Año", "De 1 a 4 años", "De 5 a 9 años",
                                                                 "De 10 a 14 años", "De 15 a 19 años", "De 20 a 24 años"),
                                  "menores de 25",
                                  datos_biobio$Grupo.edad)

# Años que me importan
cols_años <- paste0("Poblacion.", 2012:2022)
datos_biobio_reducido <- datos_biobio[, c("GrupoEdad2", "Sexo..1.Hombre.2.Mujer.", cols_años)]

# man whatever this is
library(dplyr)

resumen_biobio <- datos_biobio_reducido %>%
  group_by(GrupoEdad2, Sexo..1.Hombre.2.Mujer.) %>%
  summarise(across(starts_with("Poblacion."), sum, na.rm = TRUE)) %>%
  ungroup()

# Renombrar columnas (en vd es asignar texto a valor numérico)
resumen_biobio$Sexo..1.Hombre.2.Mujer. <- recode(resumen_biobio$Sexo..1.Hombre.2.Mujer.,
                                                 `1` = "hombres", `2` = "mujeres")

# Pivotear ?? la tabla ????
library(tidyr)

resumen_largo <- resumen_biobio %>%
  pivot_longer(cols = starts_with("Poblacion."),
               names_to = "año",
               names_prefix = "Poblacion.",
               values_to = "poblacion") %>%
  pivot_wider(names_from = Sexo..1.Hombre.2.Mujer., values_from = poblacion) %>%
  mutate(total = hombres + mujeres)

# Excel !!
setwd("/Users/macbookair/Downloads/epidemiologia")
library(writexl)
write_xlsx(resumen_largo, "Resumen_Biobio_2012_2022.xlsx")

# 25 05 2025 quiero tablas pero por comuna lolz
# Ver columnas Comuna y Nombre.Comuna para ver qué código corresponde a qué comuna
unique(datos_biobio[, c("Comuna", "Nombre.Comuna")])

# detalle
datos_biobio$Grupo.edad[datos_biobio$Grupo.edad == "80 años y más"] <- "De 80 años y más"

# okei agrupar por edades
datos_biobio$GrupoEdad3 <- ifelse(datos_biobio$Grupo.edad %in% c("0 Año", "De 1 a 4 años", "De 5 a 9 años", "De 10 a 14 años", "De 15 a 19 años", "De 20 a 24 años"), "25 años o menos", datos_biobio$Grupo.edad)

# saco lo que no me interesa
cols_años <- paste0("Poblacion.", 2012:2022)
datos_biobio_comuna_reducido <- datos_biobio[, c("Nombre.Comuna", "GrupoEdad3", "Sexo..1.Hombre.2.Mujer.", cols_años)]

# slayyy ya agrupo comuna, edad, sexo y resumo x año
library(dplyr)

resumen_comunas_bb <- datos_biobio_comuna_reducido %>%
  group_by(Nombre.Comuna, GrupoEdad3, Sexo..1.Hombre.2.Mujer.) %>%
  summarise(across(starts_with("Poblacion."), sum, na.rm = TRUE)) %>%
  ungroup()

# pivoteo
library(tidyr)

resumen_comunas_largo <- resumen_comunas_bb %>%
  mutate(Sexo = recode(Sexo..1.Hombre.2.Mujer., `1` = "hombres", `2` = "mujeres")) %>%
  select(-Sexo..1.Hombre.2.Mujer.) %>%
  pivot_longer(cols = starts_with("Poblacion."),
               names_to = "año",
               names_prefix = "Poblacion.",
               values_to = "poblacion") %>%
  pivot_wider(names_from = Sexo, values_from = poblacion) %>%
  mutate(total = hombres + mujeres)

# queda muy junto me agobie
tablas_por_comuna_bb <- split(resumen_comunas_largo, resumen_comunas_largo$Nombre.Comuna)

# de nuevo...
library(dplyr)
library(tidyr)
library(purrr)
library(writexl)

# agrupo menores de 25 años, renombro a los de 80
datos_biobio <- datos_biobio %>%
  mutate(Grupo.edad = case_when(
    Grupo.edad %in% c("0 Año", "De 1 a 4 años", "De 5 a 9 años", "De 10 a 14 años", "De 15 a 19 años", "De 20 a 24 años") ~ "Menores de 25 años",
    Grupo.edad == "80 años y más" ~ "De 80 años y más",
    TRUE ~ Grupo.edad
  ))

# chao años que no me importan
years <- 2012:2022
years_columnas <- paste0("Poblacion.", years)

# agrupar y resumir para evolución x año
tabla_anual <- datos_biobio %>%
  group_by(Nombre.Comuna, Grupo.edad, Sexo..1.Hombre.2.Mujer.) %>%
  summarise(across(all_of(years_columnas), sum, na.rm = TRUE), .groups = "drop")

# renombrar columna de sexooo
tabla_anual <- tabla_anual %>%
  mutate(Sexo = case_when(
    Sexo..1.Hombre.2.Mujer. == 1 ~ "hombres",
    Sexo..1.Hombre.2.Mujer. == 2 ~ "mujeres",
    TRUE ~ as.character(Sexo..1.Hombre.2.Mujer.)
  )) %>%
  select(-Sexo..1.Hombre.2.Mujer.)

# lista x comuna
tablas_por_comuna_real <- tabla_anual %>%
  group_split(Nombre.Comuna) %>%
  setNames(unique(tabla_anual$Nombre.Comuna))

# tabla final (espero)
tabla_final <- map2_dfr(
  tablas_por_comuna_real,
  names(tablas_por_comuna_real),
  ~ bind_rows(
    tibble(
      Nombre.Comuna = paste("Comuna:", .y),
      Grupo.edad = NA, Sexo = NA,
      !!!setNames(as.list(rep(NA, length(years_columnas))), years_columnas)
    ),
    .x,
    tibble(
      Nombre.Comuna = rep(NA, 2),
      Grupo.edad = NA, Sexo = NA,
      !!!setNames(as.list(rep(NA, length(years_columnas))), years_columnas)
    )
  )
)

# excel x favor ya no estoy soportando
write_xlsx(tabla_final, "Resumen_Evolucion_Biobio_Comunas.xlsx")





