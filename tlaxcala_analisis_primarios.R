#
#Script para evaluación de datos para mejorar políticas públicas.

#By 
# Alejandra Paulina Pérez González
# Patricia Morales Sánchez
# Liz Andrea Flores Montoya

#Librerias --- ---
pacman::p_load("vroom", 
               "dplyr", 
               "ggplot2", 
               "gridExtra")

#Funciones --- ---

#Cambiar S/D a NAs
SD_to_NA <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ na_if(.x, "S/D")))
}

#Eliminar columnas que solo contienen NAs
eliminar_NA <- function(data) {
  data <- data %>%
    select(where(~ !all(is.na(.))))  # Selecciona solo columnas que no contienen solo NAs
  return(data)
}

#Función para gráficos de barras
barplots.f <- function(data, nombre_dataset, carpeta_destino) {
  #Seleccionar solo columnas categóricas (factor o character)
  data_categorica <- data %>% select(where(is.character)) %>% mutate_all(as.factor)  # Convertir a factor para asegurar consistencia
  
  #Crear gráficos de barras para cada columna
  for (col in colnames(data_categorica)) {
    #Crear el gráfico de barras
    p <- ggplot(data_categorica, aes_string(x = col)) +
      geom_bar(fill = "steelblue", color = "black") +
      theme_minimal() +
      coord_flip() +  # Rotar para mejor visualización si hay muchas categorías
      labs(title = paste("Distribución de", col, "en  dataset", nombre_dataset), 
           x = col, y = "Frecuencia")
    
    #Guardar el gráfico en el archivo
    
    archivo_grafico <- file.path(carpeta_destino, paste0("grafico_", col, "_", nombre_dataset, ".png"))
    
    ggsave(archivo_grafico, plot = p, width = 8, height = 6, dpi = 300)
    
      }
}

#Datos --- --- 

#Dataset de las caracteristicas de agresores de mujeres
agresores <- vroom::vroom(file = "Datatonica/AGRESORES TLAXCALA_2022_2024.xlsx - AGRESORES.csv")

#Dataset de servicios estatales y municipales de atencion a violencia a las mujeres
servicios <- vroom::vroom(file = "Datatonica/SERVICIOS DE ATENCIÓN TLAXCALA 2022_2024.csv")

#Descripcion de los casos de violencia a las mujeres
casos <- vroom::vroom(file = "Datatonica/CASOS TLAXCALA 2022_2024.xlsx - CASOS.csv")

#Renombrar columnas con nombres más amigables
colnames(agresores) <- c(
  "id_euv",
  "estado_expediente",
  "fecha_recepcion",
  "estado_caso",
  "edad_agresor",
  "genero_agresor",
  "escolaridad_agresor",
  "estado_civil_agresor",
  "fecha_hechos",
  "estado_hechos",
  "municipio_hechos",
  "estado_domicilio",
  "lugar_hechos",
  "vinculo_victima",
  "conoce_agresor",
  "drogas_durante_agresion",
  "droga_alcohol",
  "droga_indicacion_medica",
  "droga_ilegal",
  "consumo_cotidiano",
  "posee_arma",
  "portaba_arma",
  "arma_chacos",
  "arma_macanas",
  "arma_otra_blanca",
  "arma_objeto_punzo_cortante",
  "arma_machete",
  "arma_proyectil",
  "arma_fuego_corta",
  "arma_fuego_larga",
  "arma_otra_fuego_larga",
  "sin_dato",
  "actividad_hogar",
  "actividad_fuera_hogar",
  "actividad_estudia",
  "actividad_jubilado",
  "actividad_pensionado",
  "actividad_desconocida",
  "actividad_otro",
  "actividad_ilicita",
  "actividad_ninguna",
  "ingresos_sin_dato",
  "ingresos_trab_formal",
  "ingresos_trab_informal",
  "ingresos_rentas",
  "ingresos_remesas",
  "ingresos_pensiones",
  "ingresos_herencia",
  "ingresos_ahorros",
  "ingresos_otro",
  "ingresos_otra_fuente"
)

# Renombrar columnas de casos
colnames(casos) <- c(
  "id_euv",
  "caso",
  "fecha_recepcion",
  "dependencia_expediente",
  "estado_expediente",
  "estado_usuario",
  "dependencia_registro",
  "estado_recepcion",
  "municipio_hechos",
  "fecha_hechos",
  "nacionalidad",
  "pais_nacimiento",
  "estado_nacimiento",
  "vivienda",
  "estado_domicilio",
  "municipio_domicilio",
  "domicilio_localidad",
  "edad",
  "rango_edades",
  "estado_civil",
  "genero",
  "esta_embarazada",
  "convivencia",
  "pertenece_etnia",
  "etnia",
  "habla_espanol",
  "habla_indigena",
  "lengua_indigena",
  "habla_lengua_extranjera",
  "lengua_extranjera",
  "sabe_leer",
  "sabe_escribir",
  "escolaridad",
  "victima_trabaja_hogar",
  "victima_trabaja_fuera_hogar",
  "victima_estudia",
  "victima_jubilada_pensionada",
  "victima_pensionada",
  "victima_actividad_desconocida",
  "victima_otra_actividad",
  "victima_actividad_ilicita",
  "victima_sin_actividad",
  "migrante",
  "estatus_migrante",
  "detalle_estatus_migrante",
  "agresion_dia_festivo",
  "agresion_domicilio",
  "lugar_violencia",
  "descripcion_lugar_victima",
  "conocimiento_autoridad",
  "descripcion_autoridad",
  "violencia_economica",
  "violencia_fisica",
  "violencia_patrimonial",
  "violencia_psicologica",
  "violencia_sexual",
  "otro_tipo_violencia",
  "modalidad_violencia",
  "victima_delincuencia_organizada",
  "num_agresores",
  "vinculo_victima",
  "detalle_vinculo_victima"
)

# Renombrar columnas de servicios
colnames(servicios) <- c(
  "id_euv",
  "estado_usuario",
  "edad_victima",
  "rango_edad",
  "estado_civil_victima",
  "fecha_captura_servicio",
  "escolaridad_victima",
  "observaciones",
  "tipo_servicio",
  "detalle_servicio",
  "descripcion",
  "dependencia_servicio",
  "municipio",
  "violencia_economica",
  "violencia_fisica",
  "violencia_patrimonial",
  "violencia_psicologica",
  "violencia_sexual",
  "otro_tipo_violencia",
  "modalidad_violencia",
  "num_agresores",
  "vinculo_victima",
  "detalle_vinculo_victima",
  "ultima_pareja",
  "no_ultima_pareja",
  "victima_trabaja_hogar",
  "victima_trabaja_fuera_hogar",
  "victima_estudia",
  "victima_jubilada_pensionada",
  "victima_pensionada",
  "victima_actividad_desconocida",
  "victima_otra_actividad",
  "victima_actividad_ilicita",
  "victima_sin_actividad",
  "pertenece_etnia",
  "etnia",
  "migrante",
  "tiene_discapacidad"
)

#Lista 
data <-  list(agresores= agresores, 
              servicios = servicios, 
              casos = casos)

#Calculo muestral --- ---
#En tlaxcala existen 693,083 mujeres (INEGI 2024)

# Parámetros
N <- 693083       #Tamaño de la población
Z <- 1.96         #Valor Z para 95% de confianza
p <- 0.5          #Proporción esperada (éxito)
q <- 1 - p        #Proporción complementaria
e <- 0.05         #Margen de error

#Formula de poblacion finita  ((N * (Z^2) * p * q) /  ((e^2 * (N - 1)) + (Z^2 * p * q))

numerador <- N * (Z^2) * p * q
denominador <- (e^2 * (N - 1)) + (Z^2 * p * q)

n <- numerador / denominador
n

#Muestra minima de 384 mujeres, lo que si se cumple

#Limpieza de datos --- ---

#Tamano original de los datos

lapply(data, dim)

# $agresores
# [1] 23596    51
# 
# $servicios
# [1] 226102     38
# 
# $casos
# [1] 23184    62

#Reemplazar "S/D" por NA
data <- lapply(data, SD_to_NA)

#eliminar columnas sin datos
data <- lapply(data, eliminar_NA)

#Tamano de los datos
lapply(data, dim)

# $agresores
# [1] 23596    51
# 
# $servicios
# [1] 226102     36
# 
# $casos
# [1] 23184    59


#Analisis primario --- ---

#Barplots por columna
barplots.f(data[["agresores"]],  "agresores", "Datatonica/agresores_barplots/")
barplots.f(data[["servicios"]],  "servicios", "Datatonica/servicios_barplots/")
barplots.f(data[["casos"]],  "casos", "Datatonica/casos_barplots/")



