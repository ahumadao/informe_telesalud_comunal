####################### Importo librerías ----

pacman::p_load(
  tidyverse,
  dplyr,
  here,
  janitor,
  lubridate,
  summarytools,
  rio,
  ggthemes,
  ggsci,
  readxl,
  ggplot2,
  scales
)

# Función de limpieza reutilizable
limpiar_nombres_centro <- function(df, col_name = NULL) {
  df %>%
    mutate(
      across(where(is.character), ~ str_replace_all(., regex("CESFAM", ignore_case = TRUE), "Centro de Salud Familiar" )),
      across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "Centro de Salud Familiar" )),
      across(where(is.character), ~ str_replace_all(., regex("CECOSF", ignore_case = TRUE), "Centro Comunitario de Salud Familiar")),
      across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "Centro Comunitario de Salud Familiar")),
      across(where(is.character), ~ str_replace_all(., regex("Cerrillos De Nos", ignore_case = TRUE), "Ribera del Maipo")),
      across(where(is.character), ~ str_replace_all(., regex("PSR", ignore_case = TRUE), "Posta de Salud Rural")),
      across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "Posta de Salud Rural")),
      across(where(is.character), ~ str_replace_all(., regex("Cecosf", ignore_case = TRUE), "CECOSF"))
    )
}

####################### Importo los datos ########################################

usuarios <- import('data/Usuarios.xlsx') %>% clean_names()
data <- import('data/base_julio_2025.csv') %>% clean_names()
piv2024 <- import('data/piv2024.xlsx') %>% clean_names()
piv2025 <- import('data/piv2025.xlsx') %>% clean_names()
# Se renombra para el join
deis <- import('data/deis_ssms.xlsx') %>%
  clean_names() %>%
  rename(centro = nombre_oficial, comuna = nombre_comuna, codigo_centro = codigo_vigente)

export(head(data), 'data_enero_head.csv')

####################### Manejo de la base de PIV 2024 y 2025 ##################################

piv_comuna_2025 <- piv2025 %>%
  limpiar_nombres_centro() %>%
  group_by(comuna) %>%
  summarize(piv_2025 = sum(inscritos)) %>%
  ungroup()

piv_comuna_2024 <- piv2024 %>%
  limpiar_nombres_centro() %>%
  group_by(comuna) %>%
  summarize(piv_2024 = sum(inscritos)) %>%
  ungroup()

piv_estab_2025 <- piv2025 %>%
  clean_names() %>%
  limpiar_nombres_centro() %>%
  group_by(comuna, centro, codigo_centro) %>%
  summarise(piv_2025 = sum(inscritos)) %>%
  ungroup()

piv_estab_2024 <- piv2024 %>%
  clean_names() %>%
  # Nota: No usamos limpiar_nombres_centro() aún en piv_estab_2024,
  # ya que los nombres serán sobrescritos por piv_estab_2025.
  group_by(comuna, centro, codigo_centro) %>%
  summarise(piv_2024 = sum(inscritos)) %>%
  ungroup()


# --- INICIO DE LA HOMOLOGACIÓN DE NOMBRES ---

# 1. Crear el diccionario de nombres (Código y Nombre 2025)
diccionario_nombres_2025 <- piv_estab_2025 %>%
  select(codigo_centro, centro_2025 = centro, comuna_2025 = comuna) %>%
  distinct() # Asegurar que solo haya una entrada por código

# 2. Homologar piv_estab_2024:
piv_estab_2024 <- piv_estab_2024 %>%
  # Unir el diccionario de nombres usando el código
  left_join(diccionario_nombres_2025, by = "codigo_centro") %>%
  # Sobrescribir las columnas 'centro' y 'comuna' con los valores de 2025
  mutate(
    centro = coalesce(centro_2025, centro), # Usar el nombre 2025 si existe, sino el 2024
    comuna = coalesce(comuna_2025, comuna) # Usar la comuna 2025 si existe, sino la 2024
  ) %>%
  # Limpieza: eliminar columnas auxiliares y ordenar
  select(-centro_2025, -comuna_2025) %>%
  # Volver a agrupar si la columna 'comuna' cambió (aunque el PIV ya es agregado)
  group_by(comuna, centro, codigo_centro) %>%
  summarise(piv_2024 = sum(piv_2024), .groups = 'drop')

#rm(piv2024)
#rm(piv2025) # Si se descomentan estas líneas, se reduce el uso de memoria.

####################### Procesar base de datos usuarios Telesalud #####################

#RUNs a eliminar
runs_a_eliminar <- c(
  177035793, 192012953, 176638737, 179582295, 182778745,
  178362070, 192646340, 177069094, 177987492, 111111111
)

#Servicios de Salud válidos
servicios_salud_validos <- c(
  "Municipalidad de Paine", "Municipalidad de Pedro Aguirre Cerda",
  "Municipalidad de San Joaquín", "Municipalidad de Lo Espejo",
  "Municipalidad de La Cisterna", "Municipalidad de Buin",
  "Municipalidad de El Bosque", "Municipalidad de San Bernardo",
  "Municipalidad de Calera de Tango", "Municipalidad de La Granja",
  "Municipalidad de San Miguel"
)

# 4. Filtrar base y limpiar nombres
usuarios <- usuarios %>%
  filter(!run %in% runs_a_eliminar) %>%
  filter(servicio_salud %in% servicios_salud_validos) %>%
  limpiar_nombres_centro()

####################### Manejo de la base de telesalud #####################

# La fecha de corte se calcula con el 'data' original, pero la fecha debe ser POSIXct primero
data$fecha_solicitud_posix <- ymd_hms(data$fecha_solicitud, tz = "UTC")

last_sol_date <- max(data$fecha_solicitud_posix, na.rm = TRUE)
first_day_this_month <- floor_date(last_sol_date, unit = "month")
last_day_previous_month <- rollback(first_day_this_month)

data1 <- data %>%
  select(id, cesfam, prioridad, fecha_solicitud, direccion,
         tipo_prestador, motivo_consulta, especificidad,
         estado, fecha_cierre, tipo_cierre, cargo,
         profesion, fecha_agenda, fecha_derivacion_1) %>%
  rename(centro = cesfam) %>%
  mutate(
    ## Gestiono las fechas (uso de floor_date) ##
    fecha_solicitud = ymd_hms(fecha_solicitud, tz = "UTC"), # Asegurar POSIXct
    hora_sol = hour(fecha_solicitud),
    fecha_cierre = ymd_hms(fecha_cierre, tz = "UTC"),
    fecha_agenda = ymd_hms(fecha_agenda, tz = "UTC"),
    fecha_derivacion_1 = ymd_hms(fecha_derivacion_1, tz = "UTC"),
    
    # Se convierte a Date sin formato redundante
    fecha_solicitud = as.Date(fecha_solicitud),
    fecha_cierre = as.Date(fecha_cierre),
    fecha_agenda = as.Date(fecha_agenda),
    fecha_derivacion_1 = as.Date(fecha_derivacion_1),
    
    mes_sol = month(fecha_solicitud),
    ano_sol = year(fecha_solicitud),
    dia_sol = weekdays(fecha_solicitud, abbreviate = FALSE), # Mejor especificar
    mes_cierre = month(fecha_cierre),
    ano_cierre = year(fecha_cierre),
    
    # Uso floor_date para generar las columnas month_year
    month_year_sol = floor_date(fecha_solicitud, "month"),
    month_year_cierre = floor_date(fecha_cierre, "month"),
    month_year_agenda = floor_date(fecha_agenda, "month"),
    month_year_deriv = floor_date(fecha_derivacion_1, "month"),
    
    ## Cambio los nombres para join con base deis (uso de case_when) ##
    centro = case_when(
      centro == 'CESFAM Eduardo Frei Montalva' ~ 'Centro de Salud Familiar Eduardo Frei Montalva',
      centro == 'Centro Comunitario de Salud Familiar Juan Aravena' ~ 'Centro Comunitario de Salud Familiar Dr. Salvador Allende',
      centro == 'Centro de Salud Familiar Juan Pablo II' ~ 'Centro de Salud Familiar Juan Pablo II ( San Bernardo)',
      centro == 'CECOSF Juan Aravena' ~ 'Centro Comunitario de Salud Familiar Dr. Salvador Allende',
      centro == 'CECOSF Eduardo Frei Montalva' ~ 'Centro Comunitario de Salud Familiar Eduardo Frei Montalva',
      centro == 'Centro Comunitario De Salud Familiar Cerrillos De Nos' ~ 'Centro Comunitario de Salud Familiar Ribera del Maipo',
      centro == 'CECOSF Dr. Miguel Enríquez Espinosa' ~ 'Centro Comunitario de Salud Familiar Dr. Miguel Enríquez Espinosa',
      TRUE ~ centro # Mantener el valor original si no coincide
    )
  ) %>%
  # El filtrado al mes anterior se aplica correctamente
  filter(fecha_solicitud < first_day_this_month) %>%
  # El join es CRÍTICO: depende de las correcciones manuales de 'centro'
  left_join(deis %>% select(codigo_centro, comuna, centro), by = "centro") %>%
  select(
    id, comuna, codigo_centro, centro, prioridad, fecha_solicitud,
    tipo_prestador, motivo_consulta, especificidad, estado,
    fecha_cierre, tipo_cierre, cargo, profesion,
    fecha_agenda, fecha_derivacion_1, month_year_sol, month_year_cierre, month_year_agenda,
    month_year_deriv, dia_sol, hora_sol
  )


#rm(data) # Se puede comentar o descomentar según necesidad de memoria
rm(deis)

################### Dataframe por comuna ########################################

# La variable 'data_por_comuna' es una lista filtrada por comuna.
data_por_comuna <- split(data1, data1$comuna)

################### Dataframe por establecimiento ########################################

# La variable 'data_por_establecimiento' es una lista filtrada por establecimiento.
data_por_establecimiento <- split(data1, data1$centro)

#cesfam_piv <- piv_estab %>%
#  rename(`n` = piv_2024
#         ) %>%
#  mutate(
#    across(where(is.character), ~ str_replace_all(., regex("CECOSF", ignore_case = TRUE), "Centro Comunitario de Salud Familiar")),
#    across(where(is.character), ~ str_replace_all(., regex("CESFAM", ignore_case = TRUE), "Centro de Salud Familiar")),
#    across(where(is.character), ~ str_replace_all(., regex("PSR", ignore_case = TRUE), "Posta de Salud Rural"))
#    ) %>%
#  select(`centro`,`codigo_centro`,`comuna`,`n`)

#export(cesfam_piv,"cesfam_piv.csv")

#### Filtrar por comuna y especificidad en casos que pidan la info filtrada ####
#data_pac <- data %>%
#  left_join(deis %>% select(comuna, centro), by = c("cesfam" = "centro")) %>%
#  filter(comuna == "Pedro Aguirre Cerda",
#         fecha_solicitud > as.Date('2025-01-01'),
#         fecha_solicitud < as.Date('2025-07-27'),
#         especificidad %in% c("Control salud mental","Problemas de Salud Mental")) %>%
#  select(-comuna)
#write_xlsx(data_pac, "datapac.xlsx")