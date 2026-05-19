# App Shiny para Análisis de Necesidades Hídricas - LAIKcA
# Con diseño moderno basado en Water & Crops
# Mantiene toda la lógica original del código de Raquel 
# VERSION CORREGIDA - Soluciona warnings de fechas y many-to-many joins
# NUEVA FUNCIONALIDAD: Filtro por CULTIVO en NH acumuladas

# Librerías necesarias
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(bslib)
library(shinyjs)
library(DT)
library(plotly)
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(stringr)
library(dplyr)
library(purrr)

# Función para detectar separador automáticamente
detectar_separador <- function(archivo) {
  lineas <- readLines(archivo, n = 3)
  primera_linea <- lineas[1]
  separadores <- c(",", ";", "\t", "|")
  conteos <- map_dbl(separadores, ~ str_count(primera_linea, fixed(.x)))
  sep_index <- which.max(conteos)
  if (conteos[sep_index] == 0) return(",")
  return(separadores[sep_index])
}

# Función para detectar y convertir fechas automáticamente (CORREGIDA)
detectar_y_convertir_fechas <- function(datos) {
  
  formatos_fecha <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y",
    "%Y/%m/%d", "%d.%m.%Y", "%Y.%m.%d", "%d/%m/%y", "%m/%d/%y",
    "%d-%m-%y", "%m-%d-%y", "%y/%m/%d", "%y-%m-%d",
    "%B %d, %Y", "%d %B %Y", "%d de %B de %Y"
  )
  
  probar_conversion_fecha <- function(columna, formato) {
    tryCatch({
      valores_no_vacios <- columna[!is.na(columna) & nchar(trimws(columna)) > 0]
      if (length(valores_no_vacios) == 0) return(NULL)
      fechas_convertidas <- as.Date(valores_no_vacios, format = formato)
      exito_rate <- sum(!is.na(fechas_convertidas)) / length(valores_no_vacios)
      if (exito_rate > 0.8) {
        return(as.Date(columna, format = formato))
      } else {
        return(NULL)
      }
    }, error = function(e) NULL)
  }
  
  for (col_name in names(datos)) {
    columna <- datos[[col_name]]
    if (is.character(columna)) {
      fecha_convertida <- FALSE
      for (formato in formatos_fecha) {
        fechas_convertidas <- probar_conversion_fecha(columna, formato)
        if (!is.null(fechas_convertidas)) {
          datos[[col_name]] <- fechas_convertidas
          message(paste("Columna", col_name, "convertida a fecha con formato:", formato))
          fecha_convertida <- TRUE
          break
        }
      }
      if (!fecha_convertida) {
        tryCatch({
          valores_no_vacios <- columna[!is.na(columna) & nchar(trimws(columna)) > 0]
          if (length(valores_no_vacios) > 0) {
            fechas_lubridate <- parse_date_time(valores_no_vacios,
                                                orders = c("ymd","dmy","mdy","ydm","myd","dym",
                                                           "ymd HMS","dmy HMS","mdy HMS"),
                                                quiet = TRUE)
            exito_rate <- sum(!is.na(fechas_lubridate)) / length(valores_no_vacios)
            if (exito_rate > 0.8) {
              fechas_completas <- parse_date_time(columna,
                                                  orders = c("ymd","dmy","mdy","ydm","myd","dym",
                                                             "ymd HMS","dmy HMS","mdy HMS"),
                                                  quiet = TRUE)
              datos[[col_name]] <- as.Date(fechas_completas)
              message(paste("Columna", col_name, "convertida a fecha usando lubridate"))
            }
          }
        }, error = function(e) {})
      }
    }
  }
  return(datos)
}

# Función mejorada para leer CSV
leer_csv_flexible <- function(archivo_path, archivo_name = NULL) {
  separador <- detectar_separador(archivo_path)
  encoding <- "UTF-8"
  tryCatch({
    test_read <- read_lines(archivo_path, n_max = 5)
    if (any(grepl("Ã|Â|Ë", test_read))) encoding <- "latin1"
  }, error = function(e) {})
  
  datos <- tryCatch({
    suppressMessages(
      read_delim(archivo_path, delim = separador,
                 locale = locale(encoding = encoding),
                 show_col_types = FALSE, trim_ws = TRUE, skip_empty_rows = TRUE)
    )
  }, error = function(e) {
    for (sep_alt in c(",", ";", "\t")) {
      if (sep_alt != separador) {
        tryCatch({
          return(suppressMessages(
            read_delim(archivo_path, delim = sep_alt,
                       locale = locale(encoding = encoding),
                       show_col_types = FALSE, trim_ws = TRUE, skip_empty_rows = TRUE)
          ))
        }, error = function(e2) {})
      }
    }
    stop(paste("No se pudo leer el archivo:", archivo_name %||% archivo_path))
  })
  
  datos <- limpiar_columnas_vacias(datos)
  datos <- detectar_y_convertir_fechas(datos)
  message(paste("Archivo leído:", nrow(datos), "filas x", ncol(datos), "columnas"))
  return(datos)
}

# Limpiar columnas vacías y problemáticas
limpiar_columnas_vacias <- function(datos) {
  nombres_originales <- names(datos)
  nombres_problematicos <- which(
    is.na(nombres_originales) | nombres_originales == "" |
      str_detect(nombres_originales, "^\\.\\.\\.[0-9]+$") |
      str_detect(nombres_originales, "^X[0-9]+$") |
      str_detect(nombres_originales, "^\\.$") |
      str_detect(nombres_originales, "^[[:space:]]*$")
  )
  
  columnas_vacias <- sapply(datos, function(col) {
    if (is.character(col)) all(is.na(col) | col == "" | str_trim(col) == "")
    else all(is.na(col))
  })
  
  columnas_a_eliminar <- unique(c(nombres_problematicos, which(columnas_vacias)))
  if (length(columnas_a_eliminar) > 0) {
    datos <- datos[, -columnas_a_eliminar, drop = FALSE]
  }
  
  nombres_limpios <- make.names(names(datos), unique = TRUE)
  if (!identical(names(datos), nombres_limpios)) names(datos) <- nombres_limpios
  return(datos)
}

# Función principal de procesamiento
procesar_datos_nh <- function(archivos_comunidades, archivo_relacion) {
  
  Relacion_Cultivo_con_CultivoNH <- leer_csv_flexible(archivo_relacion, "archivo de relación")
  
  columnas_faltantes <- setdiff(c("CULTIVO"), names(Relacion_Cultivo_con_CultivoNH))
  if (length(columnas_faltantes) > 0)
    stop(paste("Faltan columnas en el archivo de relación:", paste(columnas_faltantes, collapse = ", ")))
  
  if (any(duplicated(Relacion_Cultivo_con_CultivoNH$CULTIVO))) {
    warning("Cultivos duplicados en archivo de relación. Se conserva el primero.")
    Relacion_Cultivo_con_CultivoNH <- Relacion_Cultivo_con_CultivoNH %>%
      distinct(CULTIVO, .keep_all = TRUE)
  }
  
  extraer_nombre_comunidad <- function(info_archivo) {
    nombre_base <- tools::file_path_sans_ext(info_archivo$name)
    if (grepl("NHR_", nombre_base)) {
      comunidad <- str_replace(nombre_base, "^NHR_", "")
      comunidad <- str_replace(comunidad, "\\d{4}.*$", "")
    } else {
      comunidad <- nombre_base
    }
    return(comunidad)
  }
  
  lista_datos <- map(1:nrow(archivos_comunidades), function(i) {
    tryCatch({
      archivo_info <- archivos_comunidades[i, ]
      datos <- leer_csv_flexible(archivo_info$datapath, archivo_info$name)
      
      posibles_nombres_cultivo <- c("CULTIVO","Cultivo","cultivo","CROP","Crop")
      col_cultivo <- NULL
      for (nombre_posible in posibles_nombres_cultivo) {
        if (nombre_posible %in% names(datos)) { col_cultivo <- nombre_posible; break }
      }
      if (is.null(col_cultivo)) { warning(paste("No se encontró CULTIVO en", archivo_info$name)); return(NULL) }
      if (col_cultivo != "CULTIVO") datos <- datos %>% rename(CULTIVO = !!col_cultivo)
      
      datos$Comunidad <- extraer_nombre_comunidad(archivo_info)
      if (nrow(datos) == 0) { warning(paste("Sin datos:", archivo_info$name)); return(NULL) }
      return(datos)
    }, error = function(e) {
      warning(paste("Error:", archivo_info$name, "-", e$message)); return(NULL)
    })
  })
  
  lista_datos <- lista_datos[!sapply(lista_datos, is.null)]
  if (length(lista_datos) == 0) stop("No se pudieron leer los archivos CSV")
  
  columnas_comunes <- Reduce(intersect, map(lista_datos, names))
  lista_datos <- map(lista_datos, ~ .x %>% select(all_of(columnas_comunes)))
  
  Ano_completo <- bind_rows(lista_datos)
  
  # ---- CORRECCIÓN CLAVE: forzar Fecha a Date después del bind_rows ----
  if ("Fecha" %in% names(Ano_completo)) {
    Ano_completo <- Ano_completo %>%
      mutate(Fecha = as.Date(as.character(Fecha)))
  }
  
  Ano_completo <- Ano_completo %>%
    mutate(CulTivoAntiguo = CULTIVO) %>%
    as_tibble()
  
  Ano_completo$CULTIVO <- str_replace(Ano_completo$CULTIVO, "1", "")
  Ano_completo$CULTIVO <- str_replace(Ano_completo$CULTIVO, "2", "")
  Ano_completo$CULTIVO <- toupper(Ano_completo$CULTIVO)
  Ano_completo <- mutate(Ano_completo, clave = seq(1:nrow(Ano_completo)))
  
  cultivos_sin_relacion <- setdiff(unique(Ano_completo$CULTIVO), unique(Relacion_Cultivo_con_CultivoNH$CULTIVO))
  if (length(cultivos_sin_relacion) > 0)
    warning(paste("Cultivos sin relación:", paste(cultivos_sin_relacion, collapse = ", ")))
  
  Ano_completo <- Ano_completo %>%
    left_join(Relacion_Cultivo_con_CultivoNH, by = "CULTIVO", relationship = "many-to-one")
  
  Ano_completo <- Ano_completo %>%
    mutate(OrdenDoble = if_else(!is.na(str_extract(CulTivoAntiguo, "\\d+")),
                                str_extract(CulTivoAntiguo, "\\d+"), "0"))
  
  columnas_necesarias_calculo <- c("Kc","Pe_mm","NHn_mm","NHn_m3")
  for (col in setdiff(columnas_necesarias_calculo, names(Ano_completo))) {
    Ano_completo[[col]] <- 0
    warning(paste("Columna", col, "creada con valores 0"))
  }
  
  Ano_completo <- Ano_completo %>%
    arrange(CULTIVO, Comunidad, clave) %>%
    group_by(CULTIVO, Comunidad) %>%
    mutate(Cambio_Kc = if_else(Kc > 0 & lag(Kc, default = 0) == 0, 1, 0)) %>%
    mutate(
      Cont_Agua_I_mm = if_else(
        Cambio_Kc == 1 & OrdenDoble != "2",
        rollapply(Pe_mm, width = 30, FUN = sum, align = "right", fill = NA, na.rm = TRUE),
        if_else(
          Cambio_Kc == 1 & OrdenDoble == "2",
          rollapply(Pe_mm, width = 4, FUN = sum, align = "right", fill = NA, na.rm = TRUE),
          NA_real_
        )
      )
    ) %>%
    ungroup()
  
  Ano_completo <- Ano_completo %>%
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  Ano_SinVacios <- Ano_completo %>%
    mutate(Kc_mas4 = lead(Kc, 4)) %>%
    mutate(EnActivo = if_else(Kc > 0, 1, if_else(Kc_mas4 > 0 & OrdenDoble == "2", 1, 0))) %>%
    dplyr::filter(EnActivo == 1)
  
  if (!"ID_LAIKcA" %in% names(Ano_SinVacios)) {
    warning("Columna ID_LAIKcA no encontrada, usando CULTIVO")
    Ano_SinVacios$ID_LAIKcA <- Ano_SinVacios$CULTIVO
  }
  
  Ano_Final <- Ano_SinVacios %>%
    group_by(Comunidad, ID_LAIKcA) %>%
    mutate(NHn_mmAc = cumsum(NHn_mm)) %>%
    mutate(MaxNHn_mmAc = cummax(NHn_mmAc)) %>%
    mutate(NHn_m3_0HastaMax = ifelse(MaxNHn_mmAc > NHn_mmAc, 0, NHn_m3)) %>%
    mutate(NHn_m3Ac = cumsum(NHn_m3_0HastaMax)) %>%
    mutate(Prev_Balance_mm = NHn_mm - Cont_Agua_I_mm) %>%
    mutate(Balance_mm = cumsum(Prev_Balance_mm)) %>%
    mutate(MaxBalance_mm = cummax(Balance_mm)) %>%
    mutate(Prev_Balance_m3 = Prev_Balance_mm * (NHn_m3 / NHn_mm)) %>%
    mutate(Prev_Balance_m3_0HastaMax = ifelse(MaxBalance_mm > Balance_mm, 0, Prev_Balance_m3)) %>%
    mutate(Balance_m3 = cumsum(Prev_Balance_m3_0HastaMax)) %>%
    ungroup()
  
  # ---- CORRECCIÓN CLAVE: garantizar Date en Ano_Final ----
  if ("Fecha" %in% names(Ano_Final)) {
    Ano_Final <- Ano_Final %>% mutate(Fecha = as.Date(Fecha))
  }
  
  TablaResumen <- Ano_Final %>%
    group_by(Comunidad, CULTIVO) %>%
    summarise(NHn_mmAcumuladas = max(NHn_mmAc), .groups = "drop")
  
  message("Procesamiento completado exitosamente")
  return(list(datos_completos = Ano_Final, tabla_resumen = TablaResumen, datos_originales = Ano_completo))
}

# CSS personalizado
custom_css <- "
  .main-header {
    background: linear-gradient(135deg, #2E8B57, #228B22);
    color: white; padding: 20px; border-radius: 10px;
    margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  }
  .info-card {
    background: linear-gradient(135deg, #f8f9fa, #e9ecef);
    border-left: 4px solid #28a745; padding: 15px;
    margin: 10px 0; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  .upload-section {
    background: white; padding: 20px; border-radius: 10px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1); margin-bottom: 20px;
  }
  .process-section {
    background: linear-gradient(135deg, #007bff, #0056b3);
    color: white; padding: 20px; border-radius: 10px;
    text-align: center; margin: 20px 0;
  }
  .download-section {
    background: linear-gradient(135deg, #28a745, #155724);
    color: white; padding: 20px; border-radius: 10px; margin: 20px 0;
  }
  .status-indicator {
    display: inline-block; width: 12px; height: 12px;
    border-radius: 50%; margin-right: 8px;
  }
  .status-ready { background-color: #28a745; }
  .status-waiting { background-color: #ffc107; }
  .status-error { background-color: #dc3545; }
  .status-processing { background-color: #17a2b8; }
  .feature-icon { font-size: 24px; color: #28a745; margin-right: 10px; }
  .nav-tabs .nav-link.active {
    background-color: #28a745 !important;
    border-color: #28a745 !important; color: white !important;
  }
  .nav-tabs .nav-link {
    color: #28a745; border: 1px solid #28a745;
    margin-right: 5px; border-radius: 8px 8px 0 0;
  }
  .nav-tabs .nav-link:hover { background-color: #e8f5e8; }
  .processing-indicator {
    background: #e3f2fd; border: 1px solid #2196f3;
    border-radius: 8px; padding: 15px; margin: 10px 0;
    text-align: center; animation: pulse 2s infinite;
  }
  @keyframes pulse { 0% { opacity: 1; } 50% { opacity: 0.7; } 100% { opacity: 1; } }
  .success-message {
    background: #d4edda; border: 1px solid #c3e6cb;
    color: #155724; padding: 12px; border-radius: 8px; margin: 10px 0;
  }
  .error-message {
    background: #f8d7da; border: 1px solid #f5c6cb;
    color: #721c24; padding: 12px; border-radius: 8px; margin: 10px 0;
  }
"

# UI
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version = 5, bg = "#f8f9fa", fg = "#212529",
    primary = "#28a745", secondary = "#6c757d",
    success = "#28a745", info = "#17a2b8",
    warning = "#ffc107", danger = "#dc3545",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins", wght = "600")
  ),
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet",
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  
  div(class = "main-header",
      fluidRow(
        column(8,
               h1(HTML('<i class="fas fa-tint"></i> Análisis de Necesidades Hídricas - LAIKcA'),
                  style = "margin: 0; font-weight: 600;"),
               h4("Sistema Automatizado de Procesamiento de Datos de Comunidades de Regantes",
                  style = "margin: 5px 0 0 0; opacity: 0.9; font-weight: 300;"),
               p("Aplicación desarrollada en el marco del proyecto LAIKcA (2025 v2.1)",
                 style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.8;")
        ),
        column(4,
               div(style = "text-align: right;",
                   div(style = "width: 120px; height: 120px; background: rgba(255,255,255,0.2); border-radius: 10px; margin: 0 0 10px auto; display: flex; align-items: center; justify-content: center;",
                       HTML('<i class="fas fa-chart-area fa-3x" style="opacity: 0.7;"></i>')
                   ),
                   br(),
                   div(style = "display: flex; align-items: center; justify-content: flex-end;",
                       div(style = "margin-left: 15px; text-align: left;",
                           p("Desarrollado por:", style = "margin: 0; font-size: 12px; opacity: 0.8;"),
                           p("PhD candidate. Alexey Valero-Jorge; PhD candidate Irene Martín Brull; PhD.Ma. Auxiliadora Casterad y PhD. Raquel Salvador Esteban",
                             style = "margin: 0; font-size: 14px; font-weight: 500;")
                       )
                   )
               )
        )
      )
  ),
  
  fluidRow(
    column(12,
           div(class = "info-card",
               h5(HTML('<i class="fas fa-info-circle feature-icon"></i>Sistema de Análisis de Necesidades Hídricas')),
               p(HTML("<strong>Mejoras en esta versión:</strong> Procesamiento automático con balance hídrico, contenido inicial de agua y NH acumuladas. <strong>CORREGIDO:</strong> Error transform_date() resuelto. <strong>NUEVO:</strong> Filtro por cultivo en NH acumuladas."))
           )
    )
  ),
  
  fluidRow(
    column(4,
           div(class = "upload-section",
               h4(HTML('<i class="fas fa-upload"></i> Carga de Datos'),
                  style = "color: #495057; margin-bottom: 20px;"),
               div(id = "file_status",
                   div(style = "margin-bottom: 15px;",
                       span(class = "status-indicator status-waiting", id = "comunidades_status"),
                       strong("Archivos de Comunidades"),
                       br(),
                       tags$small("Formato: CSV con datos NHR por comunidad", style = "color: #6c757d;")
                   ),
                   div(style = "margin-bottom: 15px;",
                       span(class = "status-indicator status-waiting", id = "relacion_status"),
                       strong("Archivo de Relación de Cultivos"),
                       br(),
                       tags$small("Archivo con ID_LAIKcA, DOBLE_UNICO y GRUPO", style = "color: #6c757d;")
                   )
               ),
               div(id = "processing_indicator", class = "processing-indicator",
                   style = "display: none;",
                   HTML('<i class="fas fa-spinner fa-spin"></i> Procesando datos...'),
                   br(),
                   tags$small("Aplicando algoritmos de balance hídrico...")
               ),
               fileInput("archivos_comunidades", label = NULL,
                         buttonLabel = HTML('<i class="fas fa-folder-open"></i> Seleccionar CSVs Comunidades'),
                         placeholder = "Ningún archivo seleccionado",
                         multiple = TRUE, accept = ".csv"),
               fileInput("archivo_relacion", label = NULL,
                         buttonLabel = HTML('<i class="fas fa-table"></i> Relación de Cultivos'),
                         placeholder = "Ningún archivo seleccionado", accept = ".csv")
           ),
           
           div(class = "process-section",
               h4(HTML('<i class="fas fa-cogs"></i> Procesamiento')),
               p("Procesa automáticamente todas las comunidades.", style = "margin-bottom: 20px; opacity: 0.9;"),
               actionButton("procesar", HTML('<i class="fas fa-play"></i> Procesar Datos'),
                            class = "btn-light btn-lg", style = "width: 100%; font-weight: 600;")
           ),
           
           div(class = "download-section",
               h4(HTML('<i class="fas fa-download"></i> Resultados')),
               div(style = "margin-bottom: 20px;",
                   h6("Datos Procesados:", style = "margin-bottom: 10px; opacity: 0.9;"),
                   downloadButton("descargar_completo",
                                  HTML('<i class="fas fa-database"></i> Datos Completos'),
                                  class = "btn-light btn-sm", style = "width: 100%; margin-bottom: 5px;"),
                   downloadButton("descargar_resumen",
                                  HTML('<i class="fas fa-chart-bar"></i> Tabla Resumen'),
                                  class = "btn-light btn-sm", style = "width: 100%; margin-bottom: 5px;"),
                   downloadButton("descargar_originales",
                                  HTML('<i class="fas fa-file-csv"></i> Datos Originales'),
                                  class = "btn-light btn-sm", style = "width: 100%;")
               )
           )
    ),
    
    column(8,
           tabsetPanel(
             id = "main_tabs", type = "tabs",
             
             tabPanel(
               title = HTML('<i class="fas fa-table"></i> Datos Procesados'),
               value = "datos", br(),
               div(
                 h4("Resultados del Procesamiento Completo", style = "color: #495057;"),
                 p("Datos finales con balance hídrico, contenido inicial de agua y necesidades acumuladas.",
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 DTOutput("tabla_datos_completos", height = "600px")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-chart-line"></i> NH Acumuladas'),
               value = "nh_acumuladas", br(),
               div(
                 h4("Necesidades Hídricas Acumuladas por Comunidad", style = "color: #495057;"),
                 fluidRow(
                   column(3, selectInput("comunidad_filtro", "Filtrar por Comunidad:",
                                         choices = NULL, selected = NULL, multiple = TRUE)),
                   column(3, selectInput("grupo_filtro", "Filtrar por Grupo:",
                                         choices = NULL, selected = NULL, multiple = TRUE)),
                   column(3, selectInput("cultivo_filtro", "Filtrar por Cultivo:",
                                         choices = NULL, selected = NULL, multiple = TRUE)),
                   column(3, checkboxInput("mostrar_todas_comunidades", "Mostrar todas", TRUE))
                 ),
                 plotlyOutput("grafico_nh_acumuladas", height = "500px")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-balance-scale"></i> Balance Hídrico'),
               value = "balance", br(),
               div(
                 h4("Balance de Agua en el Suelo", style = "color: #495057;"),
                 p("Evolución del balance hídrico considerando contenido inicial y almacenamiento.",
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 plotlyOutput("grafico_balance", height = "500px")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-chart-bar"></i> Resumen'),
               value = "resumen", br(),
               div(
                 h4("Resumen por Comunidad y Cultivo", style = "color: #495057;"),
                 p("Tabla resumen con necesidades hídricas máximas acumuladas por cultivo.",
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 DTOutput("tabla_resumen")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-water"></i> Análisis m³'),
               value = "analisis_m3", br(),
               div(
                 h4("Análisis de Caudales en m³", style = "color: #495057;"),
                 p("Necesidades hídricas y balance expresados en metros cúbicos.",
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 fluidRow(
                   column(6, plotlyOutput("grafico_nh_m3", height = "400px")),
                   column(6, plotlyOutput("grafico_balance_m3", height = "400px"))
                 )
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-info-circle"></i> Metodología'),
               value = "metodologia", br(),
               div(
                 h4("Metodología Aplicada", style = "color: #495057;"),
                 div(class = "success-message",
                     h6(HTML('<i class="fas fa-check-circle"></i> Correcciones Implementadas')),
                     tags$ul(
                       tags$li(HTML("<strong>ERROR RESUELTO:</strong> transform_date() — as.Date() forzado en bind_rows() y en Ano_Final")),
                       tags$li(HTML("<strong>Fechas:</strong> Detección automática con filtrado de valores vacíos")),
                       tags$li(HTML("<strong>Joins:</strong> many-to-one con eliminación de duplicados")),
                       tags$li(HTML("<strong>Columnas vacías:</strong> Detección y eliminación automática")),
                       tags$li(HTML("<strong>NUEVO:</strong> Filtro por CULTIVO en NH acumuladas"))
                     )
                 )
               )
             )
           )
    )
  ),
  
  br(),
  div(style = "background: #e9ecef; padding: 20px; border-radius: 10px; margin-top: 30px;",
      fluidRow(
        column(4,
               h6(HTML('<i class="fas fa-cogs"></i> Procesamiento:'), style = "color: #495057;"),
               tags$ul(
                 tags$li("Detección automática de separadores CSV"),
                 tags$li("Extracción de nombres de comunidades"),
                 tags$li("Lógica completa de balance hídrico"),
                 tags$li(HTML("<strong>CORREGIDO:</strong> Fechas siempre como Date"))
               )
        ),
        column(4,
               h6(HTML('<i class="fas fa-chart-line"></i> Algoritmos:'), style = "color: #495057;"),
               tags$ul(
                 tags$li("Balance hídrico con almacenamiento"),
                 tags$li("Contenido inicial de agua en suelo"),
                 tags$li("Necesidades hídricas acumuladas"),
                 tags$li("Gestión de dobles cultivos")
               )
        ),
        column(4,
               h6(HTML('<i class="fas fa-download"></i> Resultados:'), style = "color: #495057;"),
               tags$ul(
                 tags$li("Datos completos procesados"),
                 tags$li("Tablas resumen por cultivo"),
                 tags$li("Visualizaciones interactivas"),
                 tags$li("Exportación en formato CSV")
               )
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  datos_procesados <- reactiveVal(NULL)
  archivos_comunidades_loaded <- reactiveVal(FALSE)
  archivo_relacion_loaded <- reactiveVal(FALSE)
  processing <- reactiveVal(FALSE)
  
  observeEvent(input$archivos_comunidades, {
    if (!is.null(input$archivos_comunidades)) {
      archivos_comunidades_loaded(TRUE)
      addClass("comunidades_status", "status-ready")
      removeClass("comunidades_status", "status-waiting")
    }
  })
  
  observeEvent(input$archivo_relacion, {
    if (!is.null(input$archivo_relacion)) {
      archivo_relacion_loaded(TRUE)
      addClass("relacion_status", "status-ready")
      removeClass("relacion_status", "status-waiting")
    }
  })
  
  observeEvent(input$procesar, {
    req(input$archivos_comunidades, input$archivo_relacion)
    processing(TRUE)
    show("processing_indicator")
    shinyjs::disable("procesar")
    
    withProgress(message = 'Procesando datos...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Leyendo archivos...")
        resultados <- suppressWarnings(suppressMessages(
          procesar_datos_nh(input$archivos_comunidades, input$archivo_relacion$datapath)
        ))
        incProgress(0.6, detail = "Calculando balances hídricos...")
        datos_procesados(resultados)
        incProgress(0.8, detail = "Preparando visualizaciones...")
        
        comunidades <- unique(resultados$datos_completos$Comunidad)
        grupos <- unique(resultados$datos_completos$GRUPO)
        cultivos <- unique(resultados$datos_completos$CULTIVO)
        updateSelectInput(session, "comunidad_filtro", choices = comunidades)
        updateSelectInput(session, "grupo_filtro", choices = grupos)
        updateSelectInput(session, "cultivo_filtro", choices = cultivos)
        
        incProgress(1.0, detail = "¡Completado!")
        showNotification(HTML('<i class="fas fa-check-circle"></i> ¡Datos procesados exitosamente!'),
                         type = "message", duration = 5, closeButton = TRUE)
      }, error = function(e) {
        showNotification(HTML(paste('<i class="fas fa-exclamation-triangle"></i> Error:', e$message)),
                         type = "error", duration = 10, closeButton = TRUE)
      }, finally = {
        processing(FALSE)
        hide("processing_indicator")
        shinyjs::enable("procesar")
      })
    })
  })
  
  output$tabla_datos_completos <- renderDT({
    req(datos_procesados())
    datatable(
      datos_procesados()$datos_completos,
      options = list(pageLength = 25, autoWidth = TRUE, scrollX = TRUE,
                     dom = 'Bfrtip', buttons = c('copy','csv'),
                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
      extensions = 'Buttons', class = 'cell-border stripe hover'
    ) %>% formatRound(columns = c('NHn_mmAc','Balance_mm','NHn_m3Ac','Balance_m3'), digits = 2)
  })
  
  # ============================================================
  # CORRECCIÓN PRINCIPAL: as.Date(Fecha) en los 4 gráficos
  # ============================================================
  
  output$grafico_nh_acumuladas <- renderPlotly({
    req(datos_procesados())
    datos <- datos_procesados()$datos_completos
    
    if (!input$mostrar_todas_comunidades) {
      if (!is.null(input$comunidad_filtro) && length(input$comunidad_filtro) > 0)
        datos <- datos %>% dplyr::filter(Comunidad %in% input$comunidad_filtro)
      if (!is.null(input$grupo_filtro) && length(input$grupo_filtro) > 0)
        datos <- datos %>% dplyr::filter(GRUPO %in% input$grupo_filtro)
      if (!is.null(input$cultivo_filtro) && length(input$cultivo_filtro) > 0)
        datos <- datos %>% dplyr::filter(CULTIVO %in% input$cultivo_filtro)
    }
    
    # ---- CORRECCIÓN: forcer le type Date ----
    datos <- datos %>% mutate(Fecha = as.Date(Fecha))
    
    p <- ggplot(datos, aes(x = Fecha, y = NHn_mmAc, color = GRUPO)) +
      geom_line(alpha = 0.8, size = 0.7) +
      geom_point(size = 0.8, alpha = 0.6) +
      geom_hline(yintercept = 0, color = "orange", linetype = "dashed", size = 0.8, alpha = 0.8) +
      facet_wrap(~Comunidad, scales = "free_y") +
      labs(title = "Necesidades Hídricas Netas Acumuladas",
           x = "Fecha", y = "NH Acumuladas (mm)", color = "Grupo de Cultivo") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
            strip.text = element_text(color = "#495057", face = "bold")) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "8 weeks") +
      scale_color_viridis_d(option = "mako", alpha = 0.8)
    
    ggplotly(p, tooltip = c("x","y","colour")) %>% layout(hovermode = "closest")
  })
  
  output$grafico_balance <- renderPlotly({
    req(datos_procesados())
    datos <- datos_procesados()$datos_completos
    
    if (!is.null(input$comunidad_filtro) && length(input$comunidad_filtro) > 0 && !input$mostrar_todas_comunidades)
      datos <- datos %>% dplyr::filter(Comunidad %in% input$comunidad_filtro)
    
    # ---- CORRECCIÓN: forcer le type Date ----
    datos <- datos %>% mutate(Fecha = as.Date(Fecha))
    
    p <- ggplot(datos, aes(x = Fecha)) +
      geom_line(aes(y = Balance_mm, color = "Balance Hídrico"), alpha = 0.8, size = 0.7) +
      geom_line(aes(y = MaxBalance_mm, color = "Balance Máximo"), alpha = 0.6, linetype = "dashed", size = 0.7) +
      facet_wrap(~Comunidad, scales = "free_y") +
      labs(title = "Balance de Agua en el Suelo",
           x = "Fecha", y = "Balance (mm)", color = "Tipo de Balance") +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "8 weeks") +
      scale_color_manual(values = c("Balance Hídrico" = "#007bff", "Balance Máximo" = "#28a745")) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
            strip.text = element_text(color = "#495057", face = "bold"))
    
    ggplotly(p, tooltip = c("x","y","colour")) %>% layout(hovermode = "closest")
  })
  
  output$tabla_resumen <- renderDT({
    req(datos_procesados())
    datatable(
      datos_procesados()$tabla_resumen,
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE,
                     dom = 'Bfrtip', buttons = c('copy','csv'),
                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
      extensions = 'Buttons', class = 'cell-border stripe hover'
    ) %>% formatRound(columns = 'NHn_mmAcumuladas', digits = 2)
  })
  
  output$grafico_nh_m3 <- renderPlotly({
    req(datos_procesados())
    datos <- datos_procesados()$datos_completos
    
    if (!is.null(input$comunidad_filtro) && length(input$comunidad_filtro) > 0 && !input$mostrar_todas_comunidades)
      datos <- datos %>% dplyr::filter(Comunidad %in% input$comunidad_filtro)
    
    # ---- CORRECCIÓN: forcer le type Date ----
    datos <- datos %>% mutate(Fecha = as.Date(Fecha))
    
    p <- ggplot(datos, aes(x = Fecha, y = NHn_m3Ac, color = GRUPO)) +
      geom_line(alpha = 0.8, size = 0.7) +
      geom_point(size = 0.8, alpha = 0.6) +
      facet_wrap(~Comunidad, scales = "free_y") +
      labs(title = "NH Acumuladas en m³",
           x = "Fecha", y = "NH Acumuladas (m³)", color = "Grupo") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
            strip.text = element_text(color = "#495057", face = "bold")) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "8 weeks") +
      scale_color_viridis_d(option = "mako", alpha = 0.8)
    
    ggplotly(p, tooltip = c("x","y","colour")) %>% layout(hovermode = "closest")
  })
  
  output$grafico_balance_m3 <- renderPlotly({
    req(datos_procesados())
    datos <- datos_procesados()$datos_completos
    
    if (!is.null(input$comunidad_filtro) && length(input$comunidad_filtro) > 0 && !input$mostrar_todas_comunidades)
      datos <- datos %>% dplyr::filter(Comunidad %in% input$comunidad_filtro)
    
    # ---- CORRECCIÓN: forcer le type Date ----
    datos <- datos %>% mutate(Fecha = as.Date(Fecha))
    
    p <- ggplot(datos, aes(x = Fecha, y = Balance_m3, color = Comunidad)) +
      geom_line(alpha = 0.8, size = 0.7) +
      geom_point(size = 0.8, alpha = 0.6) +
      labs(title = "Balance Hídrico en m³",
           x = "Fecha", y = "Balance (m³)", color = "Comunidad") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "8 weeks") +
      scale_color_viridis_d(option = "turbo", alpha = 0.8)
    
    ggplotly(p, tooltip = c("x","y","colour")) %>% layout(hovermode = "closest")
  })
  
  # Descargas
  output$descargar_completo <- downloadHandler(
    filename = function() paste0("NH_", Sys.Date(), ".csv"),
    content = function(file) { req(datos_procesados()); write_csv(datos_procesados()$datos_completos, file) }
  )
  output$descargar_resumen <- downloadHandler(
    filename = function() paste0("resumen_NH_", Sys.Date(), ".csv"),
    content = function(file) { req(datos_procesados()); write_csv(datos_procesados()$tabla_resumen, file) }
  )
  output$descargar_originales <- downloadHandler(
    filename = function() paste0("datos_originales_NH_", Sys.Date(), ".csv"),
    content = function(file) { req(datos_procesados()); write_csv(datos_procesados()$datos_originales, file) }
  )
  
  observe({
    if (is.null(datos_procesados())) {
      shinyjs::disable("descargar_completo")
      shinyjs::disable("descargar_resumen")
      shinyjs::disable("descargar_originales")
    } else {
      shinyjs::enable("descargar_completo")
      shinyjs::enable("descargar_resumen")
      shinyjs::enable("descargar_originales")
    }
  })
  
  observe({
    showNotification(
      HTML('<i class="fas fa-info-circle"></i> Bienvenido al sistema NH - LAIKcA. Carga tus archivos CSV para comenzar.'),
      type = "message", duration = 8, closeButton = TRUE
    )
  })
}

shinyApp(ui = ui, server = server)

