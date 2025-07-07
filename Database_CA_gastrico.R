# Leer un archivo CSV
datos <- read.csv("/Users/macbookair/Downloads/epidemiologia/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES/DEFUNCIONES_FUENTE_DEIS_1990_2022_CIFRAS_OFICIALES.csv", sep = ";", fileEncoding = "latin1")  # para tildes y ñ

head(datos)

# Testinggg
muertes_por_comuna <- table(datos$COMUNA)

# Filtrando por año antes de todo pq una es media weoncita
datos_por_ano <- subset(datos, AÑO %in% c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

# Testing más
barplot(muertes_por_comuna, 
        names.arg = muertes_por_comuna, 
        main = "Cantidad de Muertes por Comuna", 
        col = "pink1", 
        xlab = "Comuna", 
        ylab = "Cantidad de Muertes", 
        las = 2,  # Para rotar las etiquetas del eje X
        border = "mistyrose")

# Filtrar las filas donde la columna REGION sea igual a "X"
datos_filtrados <- subset(datos_por_ano, NOMBRE_REGION == "Del Bíobío")
unique(datos_filtrados$COD_COMUNA)
unique(datos_filtrados$COMUNA)
# Ver las columnas COD_COMUNA y COMUNA para ver qué código corresponde a qué comuna
unique(datos_filtrados[, c("COD_COMUNA", "COMUNA")])

# Creando dataframe manual
comunas_df <- data.frame(
  COD_COMUNA = c(8304, 8101, 8301, 8106, 8110, 8102, 8306, 8305, 8206, 8202, 8109, 8309, 8303, 8104, 8313, 8307, 8302, 8311, 8201, 8203, 8205, 8111, 8207, 8312, 8108, 8204, 8308, 8314, 8103, 8310, 8105, 8112, 8107),  # Los códigos numéricos de las comunas
  COMUNA = c("Laja", "Concepción", "Los Ángeles", "Lota", "Talcahuano", "Coronel", "Nacimiento", "Mulchén", "Los Álamos", "Arauco", "Santa Juana", "Quilleco", "Cabrero", "Florida", "Yumbel", "Negrete", "Antuco", "Santa Bárbara", "Lebu", "Cañete", "Curanilahue", "Tomé", "Tirúa", "Tucapel", "San Pedro de la Paz", "Contulmo", "Quilaco", "Alto Biobío", "Chiguayante", "San Rosendo", "Hualqui", "Hualpén", "Penco")  # Los nombres de las comunas
)


# Crear una tabla con la cantidad de muertes por cada código de comuna
muertes_por_comuna <- table(datos_filtrados$COD_COMUNA)

# Usar match() para mapear los nombres de las comunas a sus códigos
COMUNA <- comunas_df$COMUNA[match(names(muertes_por_comuna), comunas_df$COD_COMUNA)]

# Crear el gráfico de barras con los nombres de las comunas
barplot(muertes_por_comuna, 
        names.arg = COMUNA,  # Usar los nombres de las comunas
        main = "Cantidad de Muertes por Comuna", 
        col = "pink1", 
        xlab = "Comuna", 
        ylab = "Cantidad de Muertes", 
        las = 2,  # Para rotar las etiquetas del eje X
        border = "mistyrose")
# Datos filtrados
head(datos_filtrados)

# Filtrar las muertes cuyo código empieza con "C16"
muertes_C16 <- subset(datos_filtrados, grepl("^C16", DIAG1))

# Crear una tabla con la cantidad de muertes por cada código de comuna
muertesC16_por_comuna <- table(muertes_C16$COD_COMUNA)

# Usar match() para mapear los nombres de las comunas a sus códigos
COMUNA <- comunas_df$COMUNA[match(names(muertes_por_comuna), comunas_df$COD_COMUNA)]

# Ajustar los márgenes (margen inferior más grande)
par(mar = c(2, 2, 2, 2))  # Aquí 8 es el margen inferior (bottom), ajusta según sea necesario

# Crear gráfico barras de muertes x CA gástrico por comuna
barplot(muertesC16_por_comuna, 
        names.arg = COMUNA,  # Usar los nombres de las comunas
        main = "Cantidad de Muertes por Comuna", 
        col = "pink1", 
        xlab = "", 
        ylab = "Cantidad de Muertes", 
        las = 2,  # Para rotar las etiquetas del eje X
        border = "mistyrose")
# Agregar título del eje X más abajo
mtext("Comuna", side = 1, line = 3)  # line = 3 mueve el título hacia abajo

# Exportar muertes_c16 en Bíobío
write.table(muertes_C16, "/Users/macbookair/Downloads/muertes_C16.csv", 
            sep = ";", row.names = FALSE, col.names = TRUE, 
            fileEncoding = "Latin1")

# Defunciones por cáncer gástrico en Chile, periodo 2012 a 2022
cancer_gastrico_chile <- subset(datos_por_ano, grepl("^C16", DIAG1))



