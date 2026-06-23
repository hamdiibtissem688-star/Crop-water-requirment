# Instalación de paquetes
# Lista de paquetes necesarios
packages <- c("shiny", "shinythemes", "dplyr", "DT", "ggplot2", "plotly", "tidyverse", "rsconnect", "shinydashboard", "shinyWidgets", "bslib", "shinyjs")

# Función para instalar paquetes si no están instalados
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Instalar y cargar todos los paquetes necesarios
sapply(packages, install_if_missing)

library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(tidyverse)
library(rsconnect)
library(shinyWidgets)
library(bslib)
library(shinyjs)

# FUNCIÓN 1: Detectar separador automáticamente
detect_separator <- function(file_path, n_lines = 5) {
  # Leer las primeras líneas del archivo
  lines <- readLines(file_path, n = n_lines, warn = FALSE)
  
  # Posibles separadores
  separators <- c(";", ",", "\t", "|")
  separator_counts <- sapply(separators, function(sep) {
    mean(sapply(lines, function(line) length(unlist(strsplit(line, sep, fixed = TRUE)))))
  })
  
  # Retornar el separador que produce más columnas consistentemente
  return(names(separator_counts)[which.max(separator_counts)])
}

# FUNCIÓN 2: Detectar y convertir formato de fecha automáticamente
detect_and_convert_dates <- function(df) {
  # Patrones de fecha comunes
  date_patterns <- c(
    "%Y-%m-%d",    # 2024-01-15
    "%d/%m/%Y",    # 15/01/2024
    "%m/%d/%Y",    # 01/15/2024
    "%d-%m-%Y",    # 15-01-2024
    "%m-%d-%Y",    # 01-15-2024
    "%Y/%m/%d",    # 2024/01/15
    "%d.%m.%Y",    # 15.01.2024
    "%Y.%m.%d",    # 2024.01.15
    "%d-%b-%Y",    # 15-Jan-2024
    "%b-%d-%Y"     # Jan-15-2024
  )
  
  # Función para intentar convertir fecha
  try_date_conversion <- function(x, patterns) {
    for (pattern in patterns) {
      result <- try(as.Date(x, format = pattern), silent = TRUE)
      if (!inherits(result, "try-error") && !all(is.na(result))) {
        return(result)
      }
    }
    return(x)  # Si no se puede convertir, retornar original
  }
  
  # Detectar columnas que parecen fechas por nombre
  potential_date_cols <- grep("fecha|date|dia|day", names(df), ignore.case = TRUE)
  
  # Convertir columnas de fecha
  for (col_idx in potential_date_cols) {
    if (is.character(df[[col_idx]]) || is.factor(df[[col_idx]])) {
      converted <- try_date_conversion(as.character(df[[col_idx]]), date_patterns)
      if (inherits(converted, "Date")) {
        df[[col_idx]] <- converted
      }
    }
  }
  
  return(df)
}

# FUNCIÓN 3: Calcular DOY (Day of Year) desde fecha
calculate_doy <- function(date_vector) {
  return(as.numeric(format(date_vector, "%j")))
}

# FUNCIÓN 4: Unir datos de Kc con datos meteorológicos usando DOY
join_kc_meteo <- function(kc_data, meteo_data) {
  # Verificar que el archivo de Kc tiene columna DOY
  if (!"DOY" %in% names(kc_data)) {
    stop("El archivo de Kc debe contener una columna 'DOY' (Day of Year)")
  }
  
  # Verificar que el archivo meteorológico tiene columna Fecha
  if (!"Fecha" %in% names(meteo_data)) {
    stop("El archivo meteorológico debe contener una columna 'Fecha'")
  }
  
  # Calcular DOY en los datos meteorológicos
  meteo_data$DOY <- calculate_doy(meteo_data$Fecha)
  
  # Unir los datos por DOY - CAMBIO IMPORTANTE: usar all.x = TRUE para mantener todas las fechas meteo
  merged_data <- merge(meteo_data, kc_data, by = "DOY", all.x = TRUE)
  
  # Reordenar columnas para que Fecha esté primero
  col_order <- c("Fecha", "DOY", setdiff(names(merged_data), c("Fecha", "DOY")))
  merged_data <- merged_data[, col_order]
  
  return(merged_data)
}

# FUNCIÓN 5 MODIFICADA: Calcular necesidades hídricas (CON RELLENO DE CEROS) - NUEVA: CON NOMBRE PERSONALIZADO
calculate_water_needs <- function(merged_data, area_ha = NULL, custom_crop_name = NULL) {
  # Identificar columnas de cultivos (excluyendo columnas meteorológicas y DOY)
  meteo_cols <- c("Fecha", "DOY", "Ano", "Dia", "P_mm", "Pe_mm", "Eto_mm")
  crop_cols <- setdiff(names(merged_data), meteo_cols)
  
  # Crear lista para almacenar resultados
  results_list <- list()
  
  for (crop in crop_cols) {
    if (is.numeric(merged_data[[crop]]) || all(is.na(merged_data[[crop]]))) {
      # Solo filtrar por datos meteorológicos válidos
      valid_meteo_rows <- !is.na(merged_data$Eto_mm) & !is.na(merged_data$Pe_mm)
      
      if (sum(valid_meteo_rows) > 0) {
        crop_data <- merged_data[valid_meteo_rows, ]
        
        # Obtener valores de Kc
        kc_values <- crop_data[[crop]]
        
        # Crear vector de NH inicializado en 0
        nh_mm <- rep(0, nrow(crop_data))
        
        # Solo calcular NH donde Kc existe y es válido (coincidencia de DOY)
        valid_kc_indices <- !is.na(kc_values) & kc_values > 0 & kc_values <= 3
        
        # Calcular NH solo para índices válidos (mantiene valores negativos)
        nh_mm[valid_kc_indices] <- (kc_values[valid_kc_indices] * crop_data$Eto_mm[valid_kc_indices]) - crop_data$Pe_mm[valid_kc_indices]
        
        # Actualizar kc_values para mostrar 0 donde no hay coincidencia
        kc_values[!valid_kc_indices] <- 0
        
        # Calcular NH en m³ si se proporciona área
        if (!is.null(area_ha) && !is.na(area_ha)) {
          nh_m3 <- nh_mm * area_ha * 10000 / 1000
        } else {
          nh_m3 <- NA
        }
        
        # NUEVO: Usar nombre personalizado si se proporciona
        crop_display_name <- if (!is.null(custom_crop_name) && nzchar(custom_crop_name)) {
          custom_crop_name
        } else {
          crop
        }
        
        # Crear dataframe temporal
        temp_df <- data.frame(
          Fecha = crop_data$Fecha,
          DOY = crop_data$DOY,
          CULTIVO = crop_display_name,  # CAMBIO: usar nombre personalizado
          Kc = round(kc_values, 3),
          Eto_mm = crop_data$Eto_mm,
          Pe_mm = crop_data$Pe_mm,
          NH_mm = round(nh_mm, 2),
          NH_m3 = if(!is.null(area_ha)) round(nh_m3, 2) else NA,
          stringsAsFactors = FALSE
        )
        
        results_list[[crop]] <- temp_df
      }
    }
  }
  
  # Combinar todos los resultados
  if (length(results_list) > 0) {
    final_results <- do.call(rbind, results_list)
    rownames(final_results) <- NULL
    return(final_results)
  } else {
    return(data.frame())
  }
}

# CSS personalizado para mejorar la apariencia
custom_css <- "
  .main-header {
    background: linear-gradient(135deg, #2E8B57, #228B22);
    color: white;
    padding: 20px;
    border-radius: 10px;
    margin-bottom: 20px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  }
  
  .info-card {
    background: linear-gradient(135deg, #f8f9fa, #e9ecef);
    border-left: 4px solid #28a745;
    padding: 15px;
    margin: 10px 0;
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .upload-section {
    background: white;
    padding: 20px;
    border-radius: 10px;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }
  
  .process-section {
    background: linear-gradient(135deg, #007bff, #0056b3);
    color: white;
    padding: 20px;
    border-radius: 10px;
    text-align: center;
    margin: 20px 0;
  }
  
  .download-section {
    background: linear-gradient(135deg, #28a745, #155724);
    color: white;
    padding: 20px;
    border-radius: 10px;
    margin: 20px 0;
  }
  
  .status-indicator {
    display: inline-block;
    width: 12px;
    height: 12px;
    border-radius: 50%;
    margin-right: 8px;
  }
  
  .status-ready { background-color: #28a745; }
  .status-waiting { background-color: #ffc107; }
  .status-error { background-color: #dc3545; }
  .status-processing { background-color: #17a2b8; }
  
  .feature-icon {
    font-size: 24px;
    color: #28a745;
    margin-right: 10px;
  }
  
  .nav-tabs .nav-link.active {
    background-color: #28a745 !important;
    border-color: #28a745 !important;
    color: white !important;
  }
  
  .nav-tabs .nav-link {
    color: #28a745;
    border: 1px solid #28a745;
    margin-right: 5px;
    border-radius: 8px 8px 0 0;
  }
  
  .nav-tabs .nav-link:hover {
    background-color: #e8f5e8;
  }
  
  .area-input {
    background: rgba(255,255,255,0.9);
    border: 2px solid #28a745;
    border-radius: 8px;
    padding: 10px;
    margin: 10px 0;
  }
  
  .crop-name-input {
    background: rgba(255,255,255,0.9);
    border: 2px solid #17a2b8;
    border-radius: 8px;
    padding: 10px;
    margin: 10px 0;
  }
  
  .processing-indicator {
    background: #e3f2fd;
    border: 1px solid #2196f3;
    border-radius: 8px;
    padding: 15px;
    margin: 10px 0;
    text-align: center;
    animation: pulse 2s infinite;
  }
  
  @keyframes pulse {
    0% { opacity: 1; }
    50% { opacity: 0.7; }
    100% { opacity: 1; }
  }
"

# Definición de la interfaz de usuario
ui <- fluidPage(
  # Usar shinyjs
  useShinyjs(),
  
  # Tema moderno
  theme = bs_theme(
    version = 5,
    bg = "#f8f9fa",
    fg = "#212529",
    primary = "#28a745",
    secondary = "#6c757d",
    success = "#28a745",
    info = "#17a2b8",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins", wght = "600")
  ),
  
  # CSS personalizado
  tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  
  # Header principal
  div(class = "main-header",
      fluidRow(
        column(8,
               h1(HTML('<i class="fas fa-tint"></i> Water & Crops'), 
                  style = "margin: 0; font-weight: 600;"),
               h4("Sistema de Cálculo de NH con Curvas Kc Estimadas a partir de Datos Espectrales", 
                  style = "margin: 5px 0 0 0; opacity: 0.9; font-weight: 300;"),
               p("Aplicación desarrollada en el marco del proyecto LAIKcA (2025 v2.1)", 
                 style = "margin: 10px 0 0 0; font-size: 14px; opacity: 0.8;")
        ),
        column(4,
               div(style = "text-align: right;",
                   # Placeholder para imagen - reemplaza con tu imagen
                   div(style = "width: 120px; height: 120px; background: rgba(255,255,255,0.2); border-radius: 10px; margin: 0 0 10px auto; display: flex; align-items: center; justify-content: center;",
                       HTML('<i class="fas fa-water fa-3x" style="opacity: 0.7;"></i>')
                   ),
                   br(),
                   div(style = "display: flex; align-items: center; justify-content: flex-end;",
                       # Placeholder para logo CITA - reemplaza con tu imagen
                       div(style = "width: 120px; height: 60px; background: rgba(255,255,255,0.2); border-radius: 5px; display: flex; align-items: center; justify-content: center;",
                           HTML('<i class="fas fa-university" style="opacity: 0.7;"></i>')
                       ),
                       div(style = "margin-left: 15px; text-align: left;",
                           p("Desarrollado por:", style = "margin: 0; font-size: 12px; opacity: 0.8;"),
                           p("PhD candidate. Alexey Valero-Jorge; PhD candidate Irene Martín Brull; PhD.Ma. Auxiliadora Casterad y PhD. Raquel Salvador Esteban", style = "margin: 0; font-size: 14px; font-weight: 500;")
                       )
                   )
               )
        )
      )
  ),
  
  # Información sobre la aplicación
  fluidRow(
    column(12,
           div(class = "info-card",
               h5(HTML('<i class="fas fa-info-circle feature-icon"></i>¿Qué hace esta aplicación?')),
               p("Esta herramienta calcula las necesidades hídricas de cultivos utilizando curvas de coeficientes Kc derivadas de productos de la misión Sentinel-2 (por DOY) y datos meteorológicos. Ahora incluye TODAS las fechas meteorológicas, asignando Kc=0 cuando no hay datos de cultivo disponibles.")
           )
    )
  ),
  
  # Layout principal
  fluidRow(
    # Panel lateral
    column(4,
           # Sección de carga de archivos
           div(class = "upload-section",
               h4(HTML('<i class="fas fa-upload"></i> Carga de Datos'), 
                  style = "color: #495057; margin-bottom: 20px;"),
               
               # Estado de los archivos - usando elementos HTML simples
               div(id = "file_status",
                   div(style = "margin-bottom: 15px;",
                       span(class = "status-indicator status-waiting", id = "kc_status"),
                       strong("Archivo de Curvas Kc"),
                       br(),
                       tags$small("Formato: CSV con DOY y columnas de Kc por cultivo", style = "color: #6c757d;")
                   ),
                   div(style = "margin-bottom: 15px;",
                       span(class = "status-indicator status-waiting", id = "meteo_status"),
                       strong("Archivo Meteorológico"),
                       br(),
                       tags$small("Formato: CSV con Fecha, Eto_mm, Pe_mm", style = "color: #6c757d;")
                   )
               ),
               
               # Indicador de procesamiento (inicialmente oculto)
               div(id = "processing_indicator", 
                   class = "processing-indicator",
                   style = "display: none;",
                   HTML('<i class="fas fa-spinner fa-spin"></i> Procesando datos...'),
                   br(),
                   tags$small("Por favor espere mientras se procesan los archivos")
               ),
               
               # Inputs de archivos
               fileInput("kc_file", 
                         label = NULL,
                         buttonLabel = HTML('<i class="fas fa-chart-line"></i> Seleccionar Curvas Kc'),
                         placeholder = "Ningún archivo seleccionado",
                         accept = ".csv"),
               
               fileInput("meteo_file", 
                         label = NULL,
                         buttonLabel = HTML('<i class="fas fa-cloud-sun"></i> Seleccionar Meteorología'),
                         placeholder = "Ningún archivo seleccionado",
                         accept = ".csv"),
               
               # NUEVA SECCIÓN: Input para nombre de cultivo personalizado
               div(class = "crop-name-input",
                   h6(HTML('<i class="fas fa-seedling"></i> Nombre del Cultivo (Opcional)'), 
                      style = "color: #495057; margin-bottom: 10px;"),
                   textInput("crop_name", 
                             label = "Nombre personalizado:",
                             value = "",
                             placeholder = "Ej: Tomate, Maíz, Olivo..."),
                   tags$small("Si se especifica, aparecerá en la columna CULTIVO de los resultados", style = "color: #6c757d;")
               ),
               
               # Input para área (opcional)
               div(class = "area-input",
                   h6(HTML('<i class="fas fa-expand-arrows-alt"></i> Área del Cultivo (Opcional)'), 
                      style = "color: #495057; margin-bottom: 10px;"),
                   numericInput("area_ha", 
                                label = "Área en hectáreas:",
                                value = NULL,
                                min = 0.01,
                                step = 0.1),
                   tags$small("Si se especifica, se calcularán las NH en m³", style = "color: #6c757d;")
               )
           ),
           
           # Sección de procesamiento
           div(class = "process-section",
               h4(HTML('<i class="fas fa-cogs"></i> Procesamiento')),
               p("Una vez cargados ambos archivos, procese los datos para generar los análisis.", 
                 style = "margin-bottom: 20px; opacity: 0.9;"),
               actionButton("process", 
                            HTML('<i class="fas fa-play"></i> Procesar Datos'), 
                            class = "btn-light btn-lg",
                            style = "width: 100%; font-weight: 600;")
           ),
           
           # Sección de descargas
           div(class = "download-section",
               h4(HTML('<i class="fas fa-download"></i> Descargas')),
               
               div(style = "margin-bottom: 20px;",
                   h6("Datos:", style = "margin-bottom: 10px; opacity: 0.9;"),
                   downloadButton("download_data", 
                                  HTML('<i class="fas fa-table"></i> Resultados Completos'), 
                                  class = "btn-light btn-sm",
                                  style = "width: 100%; margin-bottom: 5px;"),
                   downloadButton("download_kc_table", 
                                  HTML('<i class="fas fa-chart-line"></i> Tabla Kc por Fecha'), 
                                  class = "btn-light btn-sm",
                                  style = "width: 100%; margin-bottom: 5px;"),
                   downloadButton("download_nh_table", 
                                  HTML('<i class="fas fa-tint"></i> Tabla NH por Fecha'), 
                                  class = "btn-light btn-sm",
                                  style = "width: 100%; margin-bottom: 15px;")
               ),
               
               div(
                 h6("Gráficos:", style = "margin-bottom: 10px; opacity: 0.9;"),
                 fluidRow(
                   column(6, numericInput("plot_width", "Ancho:", value = 6000, min = 500, step = 100)),
                   column(6, numericInput("plot_height", "Alto:", value = 2500, min = 500, step = 100))
                 ),
                 numericInput("plot_dpi", "DPI:", value = 300, min = 50, step = 50),
                 downloadButton("download_plot_nh", 
                                HTML('<i class="fas fa-image"></i> Gráfico NH'), 
                                class = "btn-light btn-sm",
                                style = "width: 100%; margin-bottom: 5px;"),
                 downloadButton("download_plot_kc", 
                                HTML('<i class="fas fa-image"></i> Gráfico Kc'), 
                                class = "btn-light btn-sm",
                                style = "width: 100%;")
               )
           )
    ),
    
    # Panel principal
    column(8,
           # Pestañas de resultados
           tabsetPanel(
             id = "main_tabs",
             type = "tabs",
             
             # NUEVA PESTAÑA: Configuración de Cultivo
             tabPanel(
               title = HTML('<i class="fas fa-edit"></i> Configuración'),
               value = "config",
               br(),
               div(
                 h4("Configuración del Cultivo", style = "color: #495057;"),
                 p("Personaliza el nombre que aparecerá en la columna CULTIVO de los resultados finales.", 
                   style = "color: #6c757d; margin-bottom: 30px;"),
                 
                 # Tarjeta de configuración
                 div(style = "background: white; padding: 30px; border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                     div(style = "text-align: center; margin-bottom: 30px;",
                         HTML('<i class="fas fa-seedling fa-4x" style="color: #28a745; margin-bottom: 20px;"></i>'),
                         h5("Nombre del Cultivo", style = "color: #495057; margin-bottom: 20px;")
                     ),
                     
                     fluidRow(
                       column(8, offset = 2,
                              textInput("crop_name_main", 
                                        label = NULL,
                                        value = "",
                                        placeholder = "Ingresa el nombre de tu cultivo aquí...",
                                        width = "100%"),
                              
                              div(style = "margin-top: 20px; padding: 15px; background: #f8f9fa; border-radius: 8px;",
                                  h6("Ejemplos de nombres:", style = "color: #495057; margin-bottom: 10px;"),
                                  div(style = "display: flex; flex-wrap: wrap; gap: 10px;",
                                      tags$button("Tomate", class = "btn btn-outline-success btn-sm", 
                                                  onclick = "document.getElementById('crop_name_main').value = 'Tomate'"),
                                      tags$button("Maíz", class = "btn btn-outline-success btn-sm",
                                                  onclick = "document.getElementById('crop_name_main').value = 'Maíz'"),
                                      tags$button("Olivo", class = "btn btn-outline-success btn-sm",
                                                  onclick = "document.getElementById('crop_name_main').value = 'Olivo'"),
                                      tags$button("Trigo", class = "btn btn-outline-success btn-sm",
                                                  onclick = "document.getElementById('crop_name_main').value = 'Trigo'"),
                                      tags$button("Viñedo", class = "btn btn-outline-success btn-sm",
                                                  onclick = "document.getElementById('crop_name_main').value = 'Viñedo'")
                                  )
                              ),
                              
                              div(style = "margin-top: 20px;",
                                  h6("Vista previa:", style = "color: #495057;"),
                                  div(style = "padding: 10px; background: #e8f5e8; border-radius: 5px; font-family: monospace;",
                                      "CULTIVO: ", 
                                      span(id = "crop_preview", style = "font-weight: bold; color: #28a745;", "Sin especificar")
                                  )
                              )
                       )
                     )
                 ),
                 
                 # Información adicional
                 div(style = "margin-top: 30px; padding: 20px; background: linear-gradient(135deg, #e3f2fd, #f3e5f5); border-radius: 10px;",
                     h6(HTML('<i class="fas fa-info-circle"></i> Información:'), style = "color: #495057; margin-bottom: 15px;"),
                     tags$ul(
                       tags$li("Si no especificas un nombre, se usará el nombre de la columna del archivo Kc"),
                       tags$li("El nombre aparecerá en todos los resultados, gráficos y tablas"),
                       tags$li("Puedes cambiar el nombre en cualquier momento antes de procesar"),
                       tags$li("Los cambios se aplicarán al procesar los datos nuevamente")
                     )
                 )
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-table"></i> Resultados'),
               value = "resultados",
               br(),
               div(
                 h4("Resultados del Procesamiento", style = "color: #495057;"),
                 p("Tabla completa con todos los cálculos de necesidades hídricas por cultivo y fecha. Ahora incluye todas las fechas meteorológicas.", 
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 DTOutput("results")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-chart-area"></i> NH (mm)'),
               value = "nh_plot",
               br(),
               div(
                 h4("Necesidades Hídricas en mm", style = "color: #495057;"),
                 p("Visualización temporal de las necesidades hídricas por cultivo expresadas en milímetros.", 
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 plotlyOutput("plot_nh_mm", height = "500px")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-seedling"></i> Curvas Kc'),
               value = "kc_plot",
               br(),
               div(
                 h4("Curvas de Coeficientes de Cultivo", style = "color: #495057;"),
                 p("Visualización de las curvas de Kc utilizadas en los cálculos.", 
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 plotlyOutput("plot_kc", height = "500px")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-table"></i> Tabla Kc'),
               value = "kc_table",
               br(),
               div(
                 h4("Tabla de Coeficientes Kc", style = "color: #495057;"),
                 p("Valores de Kc organizados por fecha y cultivo.", 
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 DTOutput("kc_table")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-tint"></i> Tabla NH'),
               value = "nh_table",
               br(),
               div(
                 h4("Tabla de Necesidades Hídricas", style = "color: #495057;"),
                 p("Necesidades hídricas organizadas por fecha y cultivo.", 
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 DTOutput("nh_table")
               )
             ),
             
             tabPanel(
               title = HTML('<i class="fas fa-water"></i> NH (m³)'),
               value = "nh_m3",
               br(),
               div(
                 h4("Necesidades Hídricas en m³", style = "color: #495057;"),
                 p("Visualización de las necesidades hídricas totales expresadas en metros cúbicos (requiere especificar área).", 
                   style = "color: #6c757d; margin-bottom: 20px;"),
                 # CORREGIDO: Usar uiOutput en lugar de conditionalPanel
                 uiOutput("m3_content")
               )
             )
           )
    )
  ),
  
  # Footer informativo
  br(),
  div(style = "background: #e9ecef; padding: 20px; border-radius: 10px; margin-top: 30px;",
      fluidRow(
        column(4,
               h6(HTML('<i class="fas fa-lightbulb"></i> Características:'), style = "color: #495057;"),
               tags$ul(
                 tags$li("Uso de curvas Kc pre-estimadas"),
                 tags$li("Detección automática de separadores CSV"),
                 tags$li("Conversión automática de formatos de fecha"),
                 tags$li("Incluye TODAS las fechas meteorológicas"),
                 tags$li("Rellena con Kc=0 cuando no hay datos de cultivo"),
                 tags$li("Cálculos NH en mm y m³"),
                 tags$li("Visualizaciones interactivas"),
                 tags$li("Nombre de cultivo personalizable")
               )
        ),
        column(4,
               h6(HTML('<i class="fas fa-file-csv"></i> Formatos Requeridos:'), style = "color: #495057;"),
               tags$ul(
                 tags$li("Archivo Kc: DOY + columnas por cultivo"),
                 tags$li("Archivo Meteo: Fecha, Eto_mm, Pe_mm"),
                 tags$li("Separadores: , ; | \\t"),
                 tags$li("El DOY se calcula automáticamente desde Fecha")
               )
        ),
        column(4,
               h6(HTML('<i class="fas fa-chart-bar"></i> Análisis Incluidos:'), style = "color: #495057;"),
               tags$ul(
                 tags$li("Series temporales de NH"),
                 tags$li("Visualización de curvas Kc"),
                 tags$li("Tablas pivotadas por fecha"),
                 tags$li("Exportación completa de resultados")
               )
        )
      )
  ),
  
  # JavaScript para actualizar la vista previa del nombre del cultivo
  tags$script(HTML("
    $(document).ready(function() {
      // Sincronizar los dos inputs de nombre de cultivo
      $('#crop_name').on('input', function() {
        var value = $(this).val();
        $('#crop_name_main').val(value);
        updatePreview(value);
      });
      
      $('#crop_name_main').on('input', function() {
        var value = $(this).val();
        $('#crop_name').val(value);
        updatePreview(value);
      });
      
      function updatePreview(value) {
        if (value && value.trim() !== '') {
          $('#crop_preview').text(value.trim());
        } else {
          $('#crop_preview').text('Sin especificar');
        }
      }
    });
  "))
)

# Definición del servidor
server <- function(input, output, session) {
  # Valores reactivos
  resultados <- reactiveVal()
  kc_data <- reactiveVal()
  meteo_data <- reactiveVal()
  
  # Variables reactivas para estado de archivos
  kc_loaded <- reactiveVal(FALSE)
  meteo_loaded <- reactiveVal(FALSE)
  processing <- reactiveVal(FALSE)
  
  # Actualizar indicadores de estado de archivos usando shinyjs
  observeEvent(input$kc_file, {
    if (!is.null(input$kc_file)) {
      kc_loaded(TRUE)
      addClass("kc_status", "status-ready")
      removeClass("kc_status", "status-waiting")
    }
  })
  
  observeEvent(input$meteo_file, {
    if (!is.null(input$meteo_file)) {
      meteo_loaded(TRUE)
      addClass("meteo_status", "status-ready")
      removeClass("meteo_status", "status-waiting")
    }
  })
  
  # CORREGIDO: Función reactiva para verificar si hay área
  has_area <- reactive({
    !is.null(input$area_ha) && !is.na(input$area_ha) && input$area_ha > 0
  })
  
  # CORREGIDO: Contenido condicional para la pestaña m³
  output$m3_content <- renderUI({
    if (!has_area()) {
      div(class = "alert alert-warning",
          HTML('<i class="fas fa-exclamation-triangle"></i> Para visualizar NH en m³, especifica el área en hectáreas en el panel lateral.')
      )
    } else {
      plotlyOutput("plot_nh_m3", height = "500px")
    }
  })
  
  # Procesamiento principal
  observeEvent(input$process, {
    req(input$kc_file, input$meteo_file)
    
    # Mostrar indicador de procesamiento
    processing(TRUE)
    show("processing_indicator")
    
    # Deshabilitar botón durante procesamiento
    shinyjs::disable("process")
    
    tryCatch({
      # Detectar separadores automáticamente y leer los archivos CSV
      kc_sep <- detect_separator(input$kc_file$datapath)
      meteo_sep <- detect_separator(input$meteo_file$datapath)
      
      kc_raw <- read.csv(input$kc_file$datapath, sep = kc_sep, header = TRUE, stringsAsFactors = FALSE)
      meteo_raw <- read.csv(input$meteo_file$datapath, sep = meteo_sep, header = TRUE, stringsAsFactors = FALSE)
      
      # Detectar y convertir formatos de fecha automáticamente (solo para datos meteorológicos)
      meteo_processed <- detect_and_convert_dates(meteo_raw)
      
      # Para el archivo Kc, no procesamos fechas ya que usa DOY
      kc_processed <- kc_raw
      
      # Validar columnas requeridas
      if (!"DOY" %in% names(kc_processed)) {
        stop("El archivo de Kc debe contener una columna 'DOY' (Day of Year)")
      }
      
      required_meteo_cols <- c("Fecha", "Eto_mm", "Pe_mm")
      missing_cols <- setdiff(required_meteo_cols, names(meteo_processed))
      if (length(missing_cols) > 0) {
        stop(paste("El archivo meteorológico debe contener las columnas:", paste(missing_cols, collapse = ", ")))
      }
      
      # Guardar datos procesados
      kc_data(kc_processed)
      meteo_data(meteo_processed)
      
      # Unir datos
      merged_data <- join_kc_meteo(kc_processed, meteo_processed)
      
      # Calcular necesidades hídricas CON NOMBRE PERSONALIZADO
      area_value <- if (!is.null(input$area_ha) && !is.na(input$area_ha)) input$area_ha else NULL
      
      # NUEVA: Obtener el nombre personalizado del cultivo
      custom_name <- if (!is.null(input$crop_name_main) && nzchar(trimws(input$crop_name_main))) {
        trimws(input$crop_name_main)
      } else if (!is.null(input$crop_name) && nzchar(trimws(input$crop_name))) {
        trimws(input$crop_name)
      } else {
        NULL
      }
      
      results <- calculate_water_needs(merged_data, area_value, custom_name)
      
      # Guardar resultados
      resultados(results)
      
      # Mostrar notificación de éxito
      showNotification(
        "¡Datos procesados exitosamente!",
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      # Mostrar error
      showNotification(
        paste("Error al procesar los datos:", e$message),
        type = "error",
        duration = 10
      )
    }, finally = {
      # Ocultar indicador de procesamiento y rehabilitar botón
      processing(FALSE)
      hide("processing_indicator")
      shinyjs::enable("process")
    })
  })
  
  # Tabla de resultados completos
  output$results <- renderDT({
    req(resultados())
    datatable(
      resultados(), 
      options = list(
        pageLength = 25, 
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover'
    ) %>% formatRound(columns = c('Kc', 'Eto_mm', 'Pe_mm', 'NH_mm', 'NH_m3'), digits = 2)
  })
  
  # Gráfico de NH en mm - MODIFICADO para mostrar también valores Kc=0 #########
  output$plot_nh_mm <- renderPlotly({
    req(resultados())
    
    # CAMBIO: Ahora incluir también los datos con Kc=0 (mostrarán NH=0 o solo Pe)
    data_filtered <- resultados() %>%        
      filter(!is.na(NH_mm) & !is.na(Kc))  # Solo filtrar por valores no nulos
    
    if (nrow(data_filtered) == 0) {
      # Mostrar mensaje cuando no hay datos
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, 
                 label = "No hay datos válidos para mostrar", 
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "NECESIDADES HÍDRICAS POR CULTIVO - Sin datos")
      
      return(ggplotly(p))
    }
    
    # Filtrar primero
    data_filtered <- data_filtered %>% filter(Kc != 0)
    
    p <- ggplot(data_filtered, aes(x = Fecha, y =  NH_mm, color = CULTIVO)) +
      geom_line(size = 0.8, alpha = 0.8) +
      geom_point(size = 1.5, alpha = 0.7) +
      labs(title = "NECESIDADES HÍDRICAS POR CULTIVO",
           subtitle = paste("Total registros:", nrow(data_filtered), "(incluyendo períodos sin cultivo)"),
           x = "Fecha",
           y = "Necesidad Hídrica (mm)",
           color = "Cultivo") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p) %>%
      layout(
        title = list(text = "NECESIDADES HÍDRICAS POR CULTIVO", x = 0.1),
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })
  
  # Gráfico de curvas Kc - MODIFICADO para mostrar también Kc=0
  output$plot_kc <- renderPlotly({
    req(resultados())
    
    # CAMBIO: Incluir también los valores Kc=0 para mostrar períodos sin cultivo
    data_filtered <- resultados() %>% 
      filter(!is.na(Kc) & Kc > 0)  # Solo filtrar valores no nulos
    
    if (nrow(data_filtered) == 0) {
      # Mostrar mensaje cuando no hay datos
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, 
                 label = "No hay datos de Kc para mostrar", 
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "CURVAS DE COEFICIENTES DE CULTIVO (Kc) - Sin datos")
      
      return(ggplotly(p))
    }
    
    p <- ggplot(data_filtered, aes(x = Fecha, y = Kc, color = CULTIVO)) +
      geom_line(size = 0.8, alpha = 0.8) +
      geom_point(size = 1.5, alpha = 0.7) +
      labs(title = "CURVAS DE COEFICIENTES DE CULTIVO (Kc)",
           subtitle = paste("Total registros:", nrow(data_filtered), "(incluyendo períodos con Kc=0)"),
           x = "Fecha",
           y = "Coeficiente de Cultivo (Kc)",
           color = "Cultivo") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      scale_y_continuous(limits = c(0, max(c(data_filtered$Kc, 1), na.rm = TRUE) * 1.1), 
                         breaks = seq(0, 3, by = 0.2)) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p) %>%
      layout(
        title = list(text = "CURVAS DE COEFICIENTES DE CULTIVO (Kc)", x = 0.1),
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })
  
  # Gráfico de NH en m³ - MODIFICADO para mostrar también valores con Kc=0
  output$plot_nh_m3 <- renderPlotly({
    req(resultados())
    req(has_area())
    
    data_filtered <- resultados() %>% 
      filter(!is.na(NH_m3) & !is.na(Kc))  # Solo filtrar valores no nulos
    
    if (nrow(data_filtered) == 0) {
      # Mostrar mensaje cuando no hay datos
      p <- ggplot() + 
        annotate("text", x = 0, y = 0, 
                 label = "No hay datos válidos para mostrar", 
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "NECESIDADES HÍDRICAS EN METROS CÚBICOS - Sin datos")
      
      return(ggplotly(p))
    }
    
    # Filtrar primero
    data_filtered <- data_filtered %>% filter(Kc != 0)
    
    p <- ggplot(data_filtered, aes(x = Fecha, y = NH_m3, color = CULTIVO)) +
      geom_line(size = 0.8, alpha = 0.8) +
      geom_point(size = 1.5, alpha = 0.7) +
      labs(title = "NECESIDADES HÍDRICAS EN METROS CÚBICOS",
           subtitle = paste("Total registros:", nrow(data_filtered), "(incluyendo períodos sin cultivo)"),
           x = "Fecha",
           y = "Necesidad Hídrica (m³)",
           color = "Cultivo") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
    
    ggplotly(p) %>%
      layout(
        title = list(text = "NECESIDADES HÍDRICAS EN METROS CÚBICOS", x = 0.1),
        legend = list(orientation = "h", x = 0.1, y = -0.2)
      )
  })
  
  # TABLA DE KC PIVOTADA - MODIFICADA para incluir Kc=0
  kc_pivot <- reactive({
    req(resultados())
    data_pivot <- resultados() %>%
      filter(!is.na(Kc)) %>%  # Solo filtrar valores no nulos (incluye Kc=0)
      select(DOY, CULTIVO, Kc) %>%
      pivot_wider(names_from = CULTIVO, values_from = Kc, values_fill = NA) %>%
      arrange(DOY)
    
    return(data_pivot)
  })
  
  output$kc_table <- renderDT({
    req(kc_pivot())
    
    # Crear tabla con manejo seguro de columnas
    kc_table_data <- kc_pivot()
    numeric_cols <- sapply(kc_table_data, is.numeric)
    numeric_col_names <- names(kc_table_data)[numeric_cols]
    
    dt <- datatable(
      kc_table_data, 
      options = list(
        pageLength = 25, 
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover'
    )
    
    # Aplicar formato solo a columnas numéricas que existen
    if (length(numeric_col_names) > 0) {
      dt <- dt %>% formatRound(columns = numeric_col_names, digits = 3)
    }
    
    return(dt)
  })
  
  # TABLA DE NH PIVOTADA - MODIFICADA para incluir NH cuando Kc=0
  nh_pivot <- reactive({
    req(resultados())
    data_pivot <- resultados() %>%
      filter(!is.na(NH_mm) & !is.na(Kc)) %>%  # Filtrar valores no nulos (incluye cuando Kc=0)
      select(Fecha, CULTIVO, NH_mm) %>%
      pivot_wider(names_from = CULTIVO, values_from = NH_mm, values_fill = NA) %>%
      arrange(Fecha)
    
    return(data_pivot)
  })
  
  output$nh_table <- renderDT({
    req(nh_pivot())
    
    # Crear tabla con manejo seguro de columnas
    nh_table_data <- nh_pivot()
    numeric_cols <- sapply(nh_table_data, is.numeric)
    numeric_col_names <- names(nh_table_data)[numeric_cols]
    
    dt <- datatable(
      nh_table_data, 
      options = list(
        pageLength = 25, 
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover'
    )
    
    # Aplicar formato solo a columnas numéricas que existen
    if (length(numeric_col_names) > 0) {
      dt <- dt %>% formatRound(columns = numeric_col_names, digits = 2)
    }
    
    return(dt)
  })
  
  # Descarga de resultados completos
  output$download_data <- downloadHandler(
    filename = function() {
      paste("resultados_necesidades_hidricas_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(resultados(), file, row.names = FALSE)
    }
  )
  
  # Descarga de tabla Kc
  output$download_kc_table <- downloadHandler(
    filename = function() {
      paste("tabla_kc_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(kc_pivot(), file, row.names = FALSE)
    }
  )
  
  # Descarga de tabla NH
  output$download_nh_table <- downloadHandler(
    filename = function() {
      paste("tabla_nh_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nh_pivot(), file, row.names = FALSE)
    }
  )
  
  # Descarga de gráfico NH - MODIFICADA
  output$download_plot_nh <- downloadHandler(
    filename = function() {
      paste("grafico_necesidades_hidricas_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(resultados())
      data_filtered <- resultados() %>% 
        filter(!is.na(NH_mm) & !is.na(Kc))  # Incluir también Kc=0
      
      png(file, width = input$plot_width, height = input$plot_height, res = input$plot_dpi)
      print(ggplot(data_filtered, aes(x = Fecha, y = NH_mm, color = CULTIVO)) +
              geom_line(size = 0.8, alpha = 0.8) +
              geom_point(size = 1.5, alpha = 0.7) +
              labs(title = "NECESIDADES HÍDRICAS POR CULTIVO",
                   subtitle = paste("Total registros:", nrow(data_filtered), "(incluyendo períodos sin cultivo)"),
                   x = "Fecha",
                   y = "Necesidad Hídrica (mm)",
                   color = "Cultivo") +
              scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
              theme_classic() +
              theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom",
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white")
              ))
      dev.off()
    }
  )
  
  # Descarga de gráfico Kc - MODIFICADA
  output$download_plot_kc <- downloadHandler(
    filename = function() {
      paste("grafico_curvas_kc_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(resultados())
      data_filtered <- resultados() %>% 
        filter(!is.na(Kc))  # Incluir también Kc=0
      
      png(file, width = input$plot_width, height = input$plot_height, res = input$plot_dpi)
      print(ggplot(data_filtered, aes(x = Fecha, y = Kc, color = CULTIVO)) +
              geom_line(size = 0.8, alpha = 0.8) +
              geom_point(size = 1.5, alpha = 0.7) +
              labs(title = "CURVAS DE COEFICIENTES DE CULTIVO (Kc)",
                   subtitle = paste("Total registros:", nrow(data_filtered), "(incluyendo períodos con Kc=0)"),
                   x = "Fecha",
                   y = "Coeficiente de Cultivo (Kc)",
                   color = "Cultivo") +
              scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
              scale_y_continuous(limits = c(0, max(c(data_filtered$Kc, 1), na.rm = TRUE) * 1.1), 
                                 breaks = seq(0, 3, by = 0.2)) +
              theme_classic() +
              theme(
                plot.title = element_text(size = 16, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom",
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white")
              ))
      dev.off()
    }
  )
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
