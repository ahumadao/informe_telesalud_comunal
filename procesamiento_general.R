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
  scales)

####################### Importo los datos ########################################

data <- import('data/base_enero_2025.csv') %>% clean_names()
piv <- import('data/piv2024.xlsx') %>% clean_names()
deis <- import('data/deis_ssms.xlsx') %>% clean_names() %>% rename(centro = nombre_oficial,comuna = nombre_comuna, codigo_centro = codigo_vigente)

export(head(data),'data_enero_head.csv')

####################### Manejo de la base de PIV 2024 ##################################
piv_comuna <- piv %>%
  group_by(comuna) %>%
  summarize(piv_2024 = sum(inscritos)) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario De Salud Familia", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
         across(where(is.character), ~ str_replace_all(., regex("Cecosf", ignore_case = TRUE), "CECOSF"))
  )

piv_estab <- piv %>%
  clean_names() %>% 
  group_by(comuna, centro, codigo_centro) %>%
  summarise(piv_2024 = sum(inscritos)) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario De Salud Familia", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR"))
  )


#rm(piv)

####################### Manejo de la base de telesalud #####################

last_sol_date <- max(as.Date(data$fecha_solicitud, format='%d-%m-%Y %H:%M'))
first_day_this_month <- floor_date(last_sol_date, unit = "month")
last_day_previous_month <- rollback(first_day_this_month)

data1 <- data %>%
  select(id,cesfam,prioridad,fecha_solicitud,direccion,
          tipo_prestador,motivo_consulta,especificidad,
          estado, fecha_cierre, tipo_cierre, cargo,
          profesion, fecha_agenda) %>%
  rename(centro = cesfam) %>% 
  mutate(
    ##Gestiono las fechas##
    fecha_solicitud = as.Date(fecha_solicitud, format='%d-%m-%Y %H:%M'),
    fecha_cierre = as.Date(fecha_cierre, format='%d-%m-%Y %H:%M'),
    fecha_agenda  = as.Date(fecha_agenda, format='%d-%m-%Y %H:%M'),
    mes_sol = month(fecha_solicitud),
    ano_sol = year(fecha_solicitud),
    mes_cierre = month(fecha_cierre),
    ano_cierre = year(fecha_cierre),
    month_agenda = month(fecha_agenda),
    year_agenda = year(fecha_agenda),
    month_year_sol = ymd(paste(ano_sol, mes_sol, "1", sep = "-")),
    month_year_cierre = ymd(paste(ano_cierre, mes_cierre, "1", sep = "-")), 
    month_year_agenda = ymd(paste(year_agenda, month_agenda, "1", sep = "-")), 
    ##Cambio los nombres para join con base deis##
    centro = ifelse(centro == 'CESFAM Eduardo Frei Montalva',
    'Centro de Salud Familiar Eduardo Frei Montalva', centro),
    centro = ifelse(centro == 'Centro Comunitario de Salud Familiar Dr. Salvador Allende',
    'Centro Comunitario de Salud Familiar Juan Aravena', centro),
    centro = ifelse(centro == 'Centro de Salud Familiar Juan Pablo II',
    'Centro de Salud Familiar Juan Pablo II ( San Bernardo)', centro),
    centro = ifelse(centro == 'Centro Comunitario de Salud Familiar Dr. Miguel Enríquez Espinosa',
    'CECOSF Dr. Miguel Enríquez Espinosa', centro)) %>%
  filter(fecha_solicitud < first_day_this_month) %>%
  left_join(deis %>% select(codigo_centro, comuna, centro)) %>%
  select(
    id, comuna, codigo_centro, centro, prioridad, fecha_solicitud,
    tipo_prestador, motivo_consulta, especificidad, estado,
    fecha_cierre, tipo_cierre, cargo, profesion,
    fecha_agenda, month_year_sol, month_year_cierre, month_year_agenda)


rm(data)
rm(deis)


################### Dataframe por comuna ########################################

data_por_comuna <-  split(data1, data1$comuna)

