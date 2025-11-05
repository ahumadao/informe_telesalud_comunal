
# 0. Importar librerías ----

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
  arsenal,
  knitr,
  scales,
  kableExtra,
  arsenal,
  stringr
)

# 1. Importar archivo de procesamiento inicial ----

# 2. Definir comuna ----


n <- 2


comuna1 <- data_por_comuna[[n]] %>%
  #filter( str_detect( centro, "Raúl Cuevas")) %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario De Salud Familia", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
         across(where(is.character), ~ str_replace_all(., regex("calera de tango", ignore_case = TRUE), "Calera De Tango")),
         fecha_solicitud = ymd(fecha_solicitud),
         fecha_cierre = ymd(fecha_cierre), 
         fecha_agenda = ymd(fecha_agenda)
  )

# 3. Definir variables generales ----

piv_comuna1 <- piv_comuna %>%
  filter(comuna == comuna1$comuna[n])

last_sol_date <- max(as.Date(comuna1$fecha_solicitud, format='%d-%m-%Y %H:%M'))
first_day_this_month <- floor_date(last_sol_date, unit = "month")
last_day_previous_month <- rollback(first_day_this_month)

anno <- last_day_previous_month %m-% months(12)
mesesadelante_6 <-last_day_previous_month %m+% months(6)
mesesatras_12 <- last_day_previous_month %m-% months(12)
comuna_en_uso <- comuna1$comuna[n]


comuna1$dias_al_cierre <- as.numeric(difftime(comuna1$fecha_cierre, comuna1$fecha_solicitud, units = "days"))
comuna1$dias_espera <- as.numeric(difftime(last_day_previous_month, comuna1$fecha_solicitud, , units = "days"))

start_date <- last_day_previous_month %m-% months(12)
end_date <- as.Date(first_day_this_month)

# [revisar] 4. Tasa por PIV de solicitudes  mensuales por establecimiento y comuna  ----

####  Tasa por PIV solicitudes mensuales por establecimiento (históricas) 
tasa_solicitudes_mensual_establecimiento <- comuna1 %>%
  group_by(month_year_sol, codigo_centro) %>%
  summarize(n_sol = n(), .groups = 'drop') %>%  # Explicitly drop grouping
  rename(date = month_year_sol) %>%
  filter(date < as.Date(first_day_this_month)) %>% 
  left_join(
    piv_estab %>% select(codigo_centro, piv_2024, centro, comuna),  # Select only the columns you need
    by = 'codigo_centro') %>% 
  mutate(sol_per_capita = (n_sol / piv_2024) * 1000,
         date = ymd(date)) %>%
  ungroup()

#### Tasa por PIV mensual para la comuna 
tasa_solicitudes_mensual_comuna <- tasa_solicitudes_mensual_establecimiento %>%
  filter(date < as.Date(first_day_this_month)) %>%
  group_by(date,comuna) %>%
  summarize(total_sol = sum(n_sol)) %>%
  ungroup()


##### N° cierres  mensuales establecimiento , histórico. 

n_cierres_mensual_establecimiento <- comuna1 %>%
  filter(!is.na(fecha_cierre)) %>% 
  group_by(month_year_cierre, codigo_centro, centro, comuna) %>%
  summarize(n_cierre = n()) %>%
  rename(date = month_year_cierre) %>% 
  filter(date < as.Date(first_day_this_month)) %>%
  ungroup() %>%
  mutate(date = ymd(date))

##### N° cierres  mensuales comuna, histórico.

n_cierres_mensual_comuna <- comuna1 %>%
  filter(!is.na(fecha_cierre)) %>% 
  group_by(month_year_cierre ,comuna) %>%
  summarize(n_cierre = n()) %>%
  rename(date = month_year_cierre) %>% 
  filter(date < as.Date(first_day_this_month)) %>%
  ungroup() %>%
  mutate(date = ymd(date))


##### Proporción de cierres por solicitud, establecimiento, histórico.
proporcion_cierre_por_solicitud_mensual_establecimiento <- tasa_solicitudes_mensual_establecimiento %>%
  left_join(n_cierres_mensual_establecimiento %>% 
              select(codigo_centro, date, n_cierre), 
            by = c('codigo_centro', 'date')) %>%
  ungroup() %>%  # Desagrupa si hay agrupamientos previos
  mutate(proportion = round((n_cierre / n_sol), 2)) %>%
  select(-piv_2024, -sol_per_capita) %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31'))

##### Proporción de cierres por solicitud, comuna, histórico.

proporcion_cierre_por_solicitud_mensual_comuna <- tasa_solicitudes_mensual_comuna %>%
  left_join(n_cierres_mensual_comuna %>% select(date, comuna, n_cierre), by = c('date','comuna')) %>%
  mutate(proportion = round((n_cierre / total_sol),2)) %>% 
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  filter(date > as.Date('2022-12-31'))


# [revisar] 5. solicitudes mensuales por prestador ----
# 
# mensual_sol_prestador_comuna <- comuna1 %>% 
#   group_by(month_year_sol, centro, tipo_prestador) %>%
#   summarize(n_sol = n()) %>%
#   rename(date = month_year_sol) %>% 
#   filter(date < as.Date(first_day_this_month))
# 
# mensual_cierre_prestador_comuna <- comuna1 %>%
#   filter(!is.na(fecha_cierre)) %>% 
#   group_by(month_year_cierre, centro,tipo_prestador) %>%
#   summarize(n_cierre = n()) %>%
#   rename(date = month_year_cierre) %>% 
#   filter(date < as.Date(first_day_this_month))
# 
# proporcion_cierre_por_solicitud_prestador_mensual_establecimiento <- mensual_sol_prestador_comuna %>%
#   left_join(mensual_cierre_prestador_comuna, by = c("date", "centro", 'tipo_prestador')) %>%
#   mutate(proportion = n_cierre / n_sol) %>%
#   mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
#   filter(date > as.Date('2022-12-31'))
# 
# rm(mensual_cierre_prestador_comuna,mensual_sol_prestador_comuna)
# 


# 6. Tiempo de espera ----
#función comma
comma <- function(x, big.mark = ".", decimal.mark = ",", ...) {
  format(as.numeric(x), big.mark = big.mark, decimal.mark = decimal.mark, ...)
}

# promedios tiempo de espera último año comuna y estbalecimientos. 

promedio_tiempo_espera_comuna <- comuna1 %>%
  filter(estado == 'Cerrada', fecha_cierre > mesesatras_12) %>%
  group_by(estado) %>%
  summarise(promedio = comma(round(mean(dias_al_cierre, na.rm = TRUE), 2)))

promedio_tiempo_espera_establecimiento <- comuna1 %>%
  filter(estado == 'Cerrada', fecha_solicitud > last_day_previous_month %m-% months(12)) %>%
  group_by(centro) %>%
  summarise(promedio = round(mean(dias_al_cierre, na.rm = TRUE), 2)) %>%
  arrange(desc(promedio)) %>% 
  mutate(promedio = comma(promedio)) %>% 
  select(c('centro','promedio')) %>%
  rename(`Promedio días de espera` = promedio,
         Centro = centro) 

promedio_dias_al_cierre_mensual_establecimiento <- comuna1 %>%
  filter(estado == 'Cerrada') %>%
  group_by(month_year_sol, centro) %>%
  summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE),2)) %>%
  rename(date = month_year_sol) %>%
  filter(date < as.Date(first_day_this_month), 
         date > last_day_previous_month %m-% months(12)) %>%
  mutate(date = ymd(date))

promedio_dias_al_cierre_mensual_comuna<- comuna1 %>%
  filter(estado == 'Cerrada') %>%
  group_by(month_year_sol) %>%
  summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE),2)) %>%
  rename(date = month_year_sol) %>%
  filter(date < as.Date(first_day_this_month)) %>%
  filter(date > last_day_previous_month %m-% months(12))


#tiempo de espera por prioridad ----


# Function to calculate mean days to close by priority
calculate_mean_days <- function(data, priority, start_date, end_date) {
  data %>%
    filter(estado == 'Cerrada',
           prioridad == priority,
           as.Date(month_year_cierre) > as.Date(start_date),
           as.Date(month_year_cierre) < as.Date(end_date),
    ) %>%
    group_by(centro) %>%
    summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE), 2)) %>%
    rename(!!paste0("mean_days_prior", priority) := mean_days)
}

# Calculate mean days for each priority
promedio_dias_al_cierre_prior1_establecimiento <- calculate_mean_days(comuna1, 1, start_date, end_date)
promedio_dias_al_cierre_prior2_establecimiento <- calculate_mean_days(comuna1, 2, start_date, end_date)
promedio_dias_al_cierre_prior3_establecimiento <- calculate_mean_days(comuna1, 3, start_date, end_date)

# Combine the mean days data for each priority
promedio_dias_al_cierre_prioridad_establecimiento <- promedio_dias_al_cierre_prior1_establecimiento %>%
  left_join(promedio_dias_al_cierre_prior2_establecimiento, by = 'centro') %>%
  left_join(promedio_dias_al_cierre_prior3_establecimiento, by = 'centro')

# Reshape the data to long format
promedio_dias_al_cierre_prioridad_establecimiento2 <- promedio_dias_al_cierre_prioridad_establecimiento %>%
  pivot_longer(cols = starts_with("mean_days_prior"),
               names_to = "Priority",
               names_prefix = "mean_days_prior",
               values_to = "Mean_Days") %>%
  mutate(Priority = as.numeric(Priority))


#Tiempo de espera por prestador ----

calculate_mean_days_prest <- function(data, prest, start_date, end_date) {
  data %>%
    filter(estado == 'Cerrada',
           tipo_prestador == prest,
           month_year_cierre > as.Date(start_date),
           month_year_cierre < as.Date(end_date)) %>%
    group_by(centro) %>%
    summarize(mean_days = round(mean(dias_al_cierre, na.rm = TRUE), 2)) %>%
    rename(!!paste0(prest) := mean_days)
}

# Calculate mean days for each priority
promedio_diasalcierre_med <- calculate_mean_days_prest(comuna1, 'Medicina', start_date, end_date)
promedio_diasalcierre_mat <- calculate_mean_days_prest(comuna1, 'Matrona', start_date, end_date)
promedio_diasalcierre_dent <- calculate_mean_days_prest(comuna1, 'Dental', start_date, end_date)

#Calculate mean days for each priority Otros prestadores
promedio_diasalcierre_aso <- calculate_mean_days_prest(comuna1, 'Asistente Social', start_date, end_date)
promedio_diasalcierre_kin <- calculate_mean_days_prest(comuna1, 'Kinesiología', start_date, end_date)
promedio_diasalcierre_enf <- calculate_mean_days_prest(comuna1, 'Enfermería', start_date, end_date)
promedio_diasalcierre_nut <- calculate_mean_days_prest(comuna1, 'Nutrición', start_date, end_date)
promedio_diasalcierre_psi <- calculate_mean_days_prest(comuna1, 'Psicología', start_date, end_date)
promedio_diasalcierre_to <- calculate_mean_days_prest(comuna1, 'Terapia Ocupacional', start_date, end_date)
promedio_diasalcierre_ten <- calculate_mean_days_prest(comuna1, 'Técnico en enfermería', start_date, end_date)


# Combine the mean days data for each priority
promedio_diasalcierre_todoprest <- promedio_diasalcierre_med %>%
  left_join(promedio_diasalcierre_mat, by = 'centro') %>%
  left_join(promedio_diasalcierre_dent, by = 'centro')

# Combine the mean days data for each priority otros prestadores
promedio_diasalcierre_otrosprest <- promedio_diasalcierre_aso %>%
  left_join(promedio_diasalcierre_kin, by = 'centro') %>%
  left_join(promedio_diasalcierre_enf, by = 'centro') %>% 
  left_join(promedio_diasalcierre_nut, by = 'centro') %>%
  left_join(promedio_diasalcierre_psi, by = 'centro') %>%
  left_join(promedio_diasalcierre_to, by = 'centro') %>%
  left_join(promedio_diasalcierre_ten, by = 'centro')

# Reshape the data to long format
promedio_diasalcierre_prest_long <- promedio_diasalcierre_todoprest %>%
  pivot_longer(cols = c('Medicina','Matrona', 'Dental'),
               names_to = "Prestador",
               values_to = "Mean_Days")

# Reshape the data to long format otros prestadores
promedio_diasalcierre_otrosprest_long <- promedio_diasalcierre_otrosprest %>%
  pivot_longer(cols = c('Asistente Social', 'Kinesiología', 'Enfermería', 'Nutrición', 'Psicología', 'Terapia Ocupacional','Técnico en enfermería'),
               names_to = "Prestador",
               values_to = "Mean_Days")


promedio_espera_prestador_comuna <- comuna1 %>%
  filter(estado == 'Cerrada', 
         fecha_cierre > as.Date(start_date),
         fecha_cierre < end_date) %>%
  group_by(tipo_prestador) %>%
  summarise(`Días de espera`= comma(round(mean(dias_al_cierre, na.rm = TRUE), 2)))

promedio_espera_prioridad_comuna <- comuna1 %>%
  filter(estado == 'Cerrada',
         prioridad != 4,
         fecha_cierre < as.Date(first_day_this_month), 
         fecha_cierre > last_day_previous_month %m-% months(12)) %>%
  group_by(prioridad) %>%
  summarise(`Días de espera` = comma(round(mean(dias_al_cierre, na.rm = TRUE), 2)))


#7.  Total de solicitudes por centro ----

total_sol_centro <- comuna1 %>%
  group_by(centro, codigo_centro) %>%
  summarize(`Total de solicitudes` = n(),
            max_date = max(fecha_solicitud, na.rm = TRUE),  # 👈 Última fecha de solicitud
            min_date = min(fecha_solicitud, na.rm = TRUE),  # 👈 Primera fecha de solicitud
            months_diff = interval(min_date, max_date) %/% months(1) + 1) %>% # 👈 Se añade +1 para incluir el mes inicial %>%
  ungroup() %>%
  left_join(piv_estab %>% select(codigo_centro,piv_2024),by='codigo_centro') %>%
  mutate(piv_2024 = as.numeric(gsub("\\.", "", piv_2024))) %>%
  mutate(sol_1000_month = round(`Total de solicitudes`/piv_2024/months_diff *1000,2)) %>% 
  select(centro,`Total de solicitudes`, piv_2024, sol_1000_month) %>%
  mutate(
    `Total de solicitudes` = comma(`Total de solicitudes`),
    piv_2024 = comma(piv_2024),
    sol_1000_month = comma(sol_1000_month)
  )


total_sol_centro <- total_sol_centro %>% arrange(centro) #para ordenar la información por orden alfabético según el nombre del centro (CESFAM/CECOSF)


total_sol_centro_bruto <- comuna1 %>%
  filter(fecha_solicitud >= anno & fecha_solicitud <= last_day_previous_month) %>% 
  group_by(centro, codigo_centro) %>%
  summarize(`Solicitudes ultimos 12 meses` = n(),
            max_date = last_day_previous_month,
            min_date = anno,
            months_diff = interval(min_date, max_date) %/% months(1)) %>%
  ungroup() %>%
  left_join(piv_estab %>% select(codigo_centro,piv_2024),by='codigo_centro') %>%
  mutate(piv_2024 = as.numeric(gsub("\\.", "", piv_2024))) %>%
  mutate(sol_1000_month = round(`Solicitudes ultimos 12 meses`/piv_2024/months_diff *1000,2)) %>% 
  select(centro,`Solicitudes ultimos 12 meses`, piv_2024, sol_1000_month) 

total_sol_centro_bruto <- total_sol_centro_bruto %>% arrange(centro) #para ordenar la información por orden alfabético según el nombre del centro (CESFAM/CECOSF)

# 8. Proporción de solicitudes por estado ----
##Pendientes##

proporcion_pendientes_centro_ano <- comuna1 %>%
  filter(estado == 'Pendiente',
         fecha_solicitud > anno) %>% 
  group_by(centro, estado) %>%
  summarize(n_sol = n()) %>%
  left_join(total_sol_centro_bruto, by = 'centro') %>%
  mutate(
    prop_sol = round(n_sol/`Solicitudes ultimos 12 meses`*100, 2)) %>%
  select('centro','n_sol','Solicitudes ultimos 12 meses', 'prop_sol') %>%
  rename(
    Centro = centro,
    `Solicitudes pendientes` = n_sol,
    `Total de solicitudes` = `Solicitudes ultimos 12 meses`,
    `Proporción de solicitudes pendientes` = prop_sol) %>%
  mutate(
    `Solicitudes pendientes` = comma(`Solicitudes pendientes`),
    `Total de solicitudes` = comma(`Total de solicitudes`),
    `Proporción de solicitudes pendientes` = comma(`Proporción de solicitudes pendientes`)
  )



# proporcion_pendientes_comuna$`Total de solicitudes` <- as.numeric(gsub("\\,", "", proporcion_pendientes_comuna$`Total de solicitudes`))
# proporcion_pendientes_comuna$`Total de solicitudes` <- comma(as.numeric(proporcion_pendientes_comuna$`Total de solicitudes`))


total_sol <- nrow(comuna1)

# Group by 'Estado' and calculate the number of 'Pendiente' and total rows within each group
proporcion_pendientes_comuna <- comuna1 %>%
  filter(estado == 'Pendiente') %>% 
  group_by(estado, comuna) %>%
  summarize(
    n_sol = sum(estado == 'Pendiente')  # Count the number of 'Pendiente' states
  ) %>%
  mutate(
    total_sol = total_sol,               # Add the total number of rows in the entire dataset
    proportion = n_sol / total_sol       # Calculate the proportion of 'Pendiente' states
  )


# [USO] Tipos de cierre ultimo año ####### ----

tipos_de_cierre <- comuna1 %>%
  filter((fecha_cierre > anno), !is.na(fecha_cierre), fecha_cierre < first_day_this_month) %>%
  group_by(centro, tipo_cierre) %>%
  summarize(cierres = n()) %>%
  left_join(n_cierres_mensual_establecimiento %>%
              filter(date > anno,
                     date < first_day_this_month) %>%
              group_by(centro) %>% 
              summarise(total_cierres = sum(n_cierre)), 
            by='centro') %>% 
  mutate(prop_tipo_cierre = round(cierres/total_cierres*100 , 2))

top_values <- tipos_de_cierre %>%
  group_by(centro) %>%
  arrange(centro, desc(prop_tipo_cierre)) %>%
  slice_head(n = 3) %>%
  ungroup()

tipos_de_cierre <- tipos_de_cierre %>%
  left_join(top_values %>% select(centro, tipo_cierre, prop_tipo_cierre) %>% mutate(top = TRUE), 
            by = c("centro", "tipo_cierre", "prop_tipo_cierre")) %>%
  mutate(top = ifelse(is.na(top), FALSE, top))



# [USO] Fechas agendamiento ----

comuna1$dias_sol_agenda <- as.numeric(difftime(comuna1$fecha_agenda, comuna1$fecha_solicitud, units = "days"))
comuna1$dias_cierre_agenda <- as.numeric(difftime(comuna1$fecha_agenda, comuna1$fecha_cierre, units = "days"))

# [USO] SCA por estab ----

tabla_dias_sol_cierre_agenda_establecimiento <- comuna1 %>% 
  filter(
    estado == 'Cerrada',
    fecha_cierre > mesesatras_12,
    fecha_agenda < mesesadelante_6,
    fecha_agenda > mesesatras_12,
    dias_cierre_agenda != 0,
    prioridad != 4,
    tipo_cierre %in% c('Agendado para atención presencial', 
                       'Agendado para atención por telemedicina', 
                       'Agendado para orden de examen'), 
  )%>%
  group_by(`centro`) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
  )


# [USO] SCA por tipo de cierre ----

tabla_dias_sol_cierre_agenda_establecimiento_tipo_cierre <- comuna1 %>% 
  filter(
    estado == 'Cerrada',
    fecha_cierre > mesesatras_12,
    fecha_agenda < mesesadelante_6,
    fecha_agenda > mesesatras_12,
    dias_cierre_agenda != 0,
    prioridad != 4,
    tipo_cierre %in% c('Agendado para atención presencial', 
                       'Agendado para atención por telemedicina', 
                       'Agendado para orden de examen'), 
  )%>%
  group_by(tipo_cierre) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
  )


# [USO] SCA por prioridad ----

tabla_dias_sol_cierre_agenda_establecimiento_prioridad <- comuna1 %>% 
  filter(
    estado == 'Cerrada',
    fecha_cierre > mesesatras_12,
    fecha_agenda < mesesadelante_6,
    fecha_agenda > mesesatras_12,
    dias_cierre_agenda != 0,
    prioridad != 4,
    tipo_cierre %in% c('Agendado para atención presencial', 
                       'Agendado para atención por telemedicina', 
                       'Agendado para orden de examen'), 
  )%>%
  group_by(prioridad) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
         Prioridad = prioridad
  )

# [USO] SCA por prestador ----

tabla_dias_sol_cierre_agenda_establecimiento_prestador <- comuna1 %>% 
  filter(
    estado == 'Cerrada',
    fecha_cierre > mesesatras_12,
    fecha_agenda < mesesadelante_6,
    fecha_agenda > mesesatras_12,
    dias_cierre_agenda != 0,
    prioridad != 4,
    tipo_cierre %in% c('Agendado para atención presencial', 
                       'Agendado para atención por telemedicina', 
                       'Agendado para orden de examen'), 
  )%>%
  group_by(tipo_prestador) %>%
  summarise(
    media_dias_sol_cierre = comma(round(mean(dias_al_cierre, na.rm = TRUE),2)),
    media_dias_sol_agenda = comma(round(mean(dias_sol_agenda, na.rm = TRUE),2)),
    media_dias_cierre_agenda = comma(round(mean(dias_cierre_agenda, na.rm = TRUE),2))
  ) %>% 
  rename(`Media sol. al cierre` = 'media_dias_sol_cierre',
         `Media sol. al agend.` = 'media_dias_sol_agenda',
         `Media cierre. al agend.` = 'media_dias_cierre_agenda',
         `Prestador` = tipo_prestador
  )



#[USO] Proporcion de cierres/solocitud comunal ultimo año ----

n_solicitudes_ano_comuna <- comuna1 %>%
  filter(
    fecha_solicitud > mesesatras_12,
    fecha_solicitud < first_day_this_month
  ) %>%
  group_by(comuna) %>%
  summarize(total = n()) %>%
  ungroup()

n_cierres_ano_comuna <- comuna1 %>%
  filter(
    fecha_cierre > mesesatras_12,
    fecha_cierre < first_day_this_month,
    fecha_solicitud < first_day_this_month,
    !is.na(fecha_cierre)
  ) %>%
  group_by(comuna) %>%
  summarize(total = n()) %>%
  ungroup()

proporcion_cierre_por_solicitud_ano_comuna<- n_solicitudes_ano_comuna %>%
  left_join(n_cierres_ano_comuna, by= 'comuna') %>%
  rename(total_solicitudes = 'total.x',
         total_cierres = 'total.y') %>%
  mutate(cie_sol = round(total_cierres/total_solicitudes,2))

rm(n_solicitudes_ano_establecimiento,n_cierres_ano_establecimiento )


#[USO] Proporcion de cierres/solicitud por establecimiento ultimo año ----

n_solicitudes_ano_establecimiento <- comuna1 %>%
  filter(
    fecha_solicitud > mesesatras_12,
    fecha_solicitud < first_day_this_month
  ) %>%
  group_by(month_year_sol,centro) %>%
  summarize(total = n()) %>%
  ungroup()

n_cierres_ano_establecimiento <- comuna1 %>%
  filter(
    fecha_cierre > mesesatras_12,
    fecha_cierre < first_day_this_month,
    fecha_solicitud < first_day_this_month,
    !is.na(fecha_cierre)
  ) %>%
  group_by(month_year_cierre, centro) %>%
  summarize(total = n()) %>%
  ungroup()

proporcion_cierre_por_solicitud_ano_establecimiento<- n_solicitudes_ano_establecimiento %>%
  left_join(n_cierres_ano_establecimiento, by= c('centro', 'month_year_sol'='month_year_cierre')) %>%
  rename(total_solicitudes = 'total.x',
         total_cierres = 'total.y') %>%
  mutate(cie_sol = round(total_cierres/total_solicitudes,2))

rm(n_solicitudes_ano_establecimiento,n_cierres_ano_establecimiento )


#[USO] Proporcion de cierres/solicitud por prestador y establecimiento ultimo año ----

n_solicitudes_ano_prestador_establecimiento <- comuna1 %>%
  filter(
    fecha_solicitud > mesesatras_12,
    fecha_solicitud < first_day_this_month
  ) %>%
  group_by(centro,tipo_prestador) %>%
  summarize(total = n()) %>%
  ungroup()

n_cierres_ano_prestador_establecimiento <- comuna1 %>%
  filter(
    fecha_cierre > mesesatras_12,
    fecha_cierre < first_day_this_month,
    fecha_solicitud < first_day_this_month,
    !is.na(fecha_cierre)
  ) %>%
  group_by(centro,tipo_prestador) %>%
  summarize(total = n()) %>%
  ungroup()

proporcion_cierre_por_solicitud_ano_prestador_establecimiento<- n_solicitudes_ano_prestador_establecimiento %>%
  left_join(n_cierres_ano_prestador_establecimiento, by= c('centro','tipo_prestador')) %>%
  rename(total_solicitudes = 'total.x',
         total_cierres = 'total.y') %>%
  mutate(cie_sol = round(total_cierres/total_solicitudes,2))



#[USO] proporcion mensual por prestador por establecimiento  ##########################################


#[USO] Proporcion de cierres/solicitud mensual por prestador y establecimiento ultimo año ----

n_solicitudes_mensual_prestador_establecimiento <- comuna1 %>%
  filter(
    fecha_solicitud > mesesatras_12,
    fecha_solicitud < first_day_this_month
  ) %>%
  group_by(month_year_sol, centro,tipo_prestador) %>%
  summarize(total = n()) %>%
  ungroup()

n_cierres_mensual_prestador_establecimiento <- comuna1 %>%
  filter(
    fecha_cierre > mesesatras_12,
    fecha_cierre < first_day_this_month,
    fecha_solicitud < first_day_this_month,
    !is.na(fecha_cierre)
  ) %>%
  group_by(month_year_cierre, centro,tipo_prestador) %>%
  summarize(total = n()) %>%
  ungroup()

proporcion_cierre_por_solicitud_mensual_prestador_establecimiento<- n_solicitudes_mensual_prestador_establecimiento %>%
  left_join(n_cierres_mensual_prestador_establecimiento, by= c('month_year_sol'='month_year_cierre','centro','tipo_prestador')) %>%
  rename(total_solicitudes = 'total.x',
         total_cierres = 'total.y') %>%
  mutate(cie_sol = round(total_cierres/total_solicitudes,2)) %>%
  rename(date = month_year_sol)



z_proporcion_cierre_por_solicitud_medico_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Medicina')

z_proporcion_cierre_por_solicitud_dental_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Dental')

z_proporcion_cierre_por_solicitud_matrona_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Matrona')

z_proporcion_cierre_por_solicitud_asistente_social_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Asistente Social')

z_proporcion_cierre_por_solicitud_enfermeria_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Enfermería')

z_proporcion_cierre_por_solicitud_kinesiologia_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Kinesiología')

z_proporcion_cierre_por_solicitud_nutricion_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Nutrición')

z_proporcion_cierre_por_solicitud_psicologia_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Psicología')

z_proporcion_cierre_por_solicitud_tens_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Técnico en enfermería')

z_proporcion_cierre_por_solicitud_to_mensual_establecimiento <- proporcion_cierre_por_solicitud_mensual_prestador_establecimiento %>%
  mutate(cie_sol = ifelse(is.na(cie_sol), 0, cie_sol)) %>%
  filter(date > mesesatras_12) %>% 
  filter(tipo_prestador == 'Terapia Ocupacional')

proporcion_pendientes_centro_ano <- proporcion_pendientes_centro_ano %>% arrange(desc(`Proporción de solicitudes pendientes`))


