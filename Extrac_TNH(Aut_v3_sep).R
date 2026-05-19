rm(list = ls())

#install.packages("campfin") # Si no tienes este paquete debes instalarlo
library(campfin)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(tidyr)

# 1. Definir carpeta donde están los CSV
  ruta_carpeta <- "C:/Users/hamdi/Desktop/TFM/Datos/Analisis_NH/NH_Teledetección(Modelo2)/Monegros/2020/CEBADA_TARDIA"

# 2. Listar todos los csv
lista_archivos <- list.files(
  path = ruta_carpeta,
  pattern = "\\.csv$",
  full.names = TRUE
)

# 3. Función para procesar cada archivo
procesar_csv <- function(ruta_archivo) {
  
  # Leer archivo con detección automática de delimitador
  datos <- read_delim(
    ruta_archivo,
    delim = NULL,   # <-- aquí está la clave
    locale = locale(encoding = "Latin1"),
    show_col_types = FALSE,
    trim_ws = TRUE
  )
  
  fases_interes <- c("INICIO", "DESARROLLO", "MEDIADOS", "FINAL")
  
  # Validación: si no existe MAIZ, saltar archivo
  if (!"2veza-MAIZ" %in% datos$CULTIVO) {
    message("Archivo sin 2veza-MAIZ", basename(ruta_archivo))
    return(NULL)
  }
  
  datos_maiz <- datos %>%
  filter(CULTIVO == "2veza-MAIZ")
  
  # --- Acumulado por fase ---
  resumen_fases <- datos_maiz %>%
    filter(FASE %in% fases_interes) %>%
    group_by(FASE) %>%
    summarise(
      NH_mm = sum(NH_mm, na.rm = TRUE),
      Eto_mm = sum(Eto_mm, na.rm = TRUE),
      Pe_mm  = sum(Pe_mm,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    complete(
      FASE = fases_interes,
      fill = list(
        NH_mm = NA,
        Eto_mm = NA,
        Pe_mm  = NA
      )
    )
  
  # --- Fila total ---
  fila_total <- resumen_fases %>%
    summarise(
      FASE   = "TOTAL",
      NH_mm = sum(NH_mm, na.rm = TRUE),
      Eto_mm = sum(Eto_mm, na.rm = TRUE),
      Pe_mm  = sum(Pe_mm,  na.rm = TRUE)
    )
  
  resumen <- bind_rows(resumen_fases, fila_total) %>%
    mutate(
      FASE = factor(FASE, levels = c(fases_interes, "TOTAL")),
      nombre_archivo = basename(ruta_archivo)
    ) %>%
    arrange(FASE)
  
  return(resumen)
}

# 4. Aplicar la función
resultado_final <- map_dfr(lista_archivos, procesar_csv)

# 5. Guardar CSV consolidado
write_csv(
  resultado_final,
  "C:/Users/hamdi/Desktop/TFM/Datos/Analisis_NH/Results_NH-TD/GUISANTE/Resumen-guisante(2Zaidin)_total_20(C2).csv"
)

# 6. Ver resultado
print(resultado_final)

