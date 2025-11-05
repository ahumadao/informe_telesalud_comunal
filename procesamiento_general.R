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

usuarios <- import('data/Usuarios.xlsx') %>%  clean_names()
data <- import('data/base_julio_2025.csv') %>% clean_names()
piv2024 <- import('data/piv2024.xlsx') %>% clean_names()
piv2025 <- import('data/piv2025.xlsx') %>% clean_names()
deis <- import('data/deis_ssms.xlsx') %>% clean_names() %>% rename(centro = nombre_oficial,comuna = nombre_comuna, codigo_centro = codigo_vigente)

export(head(data),'data_enero_head.csv')

####################### Manejo de la base de PIV 2024 y 2025 ##################################
piv_comuna_2025 <- piv2025 %>%
  group_by(comuna) %>%
  summarize(piv_2025 = sum(inscritos)) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Cerrillos De Nos", ignore_case = TRUE), "Ribera del Maipo")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
         across(where(is.character), ~ str_replace_all(., regex("Cecosf", ignore_case = TRUE), "CECOSF"))
  )
piv_comuna_2024 <- piv2024 %>%
  group_by(comuna) %>%
  summarize(piv_2024 = sum(inscritos)) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Cerrillos De Nos", ignore_case = TRUE), "Ribera del Maipo")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
         across(where(is.character), ~ str_replace_all(., regex("Cecosf", ignore_case = TRUE), "CECOSF"))
  )

piv_estab_2025 <- piv2025 %>%
  clean_names() %>% 
  group_by(comuna, centro, codigo_centro) %>%
  summarise(piv_2025 = sum(inscritos)) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Cerrillos De Nos", ignore_case = TRUE), "Ribera del Maipo")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
         across(where(is.character), ~ str_replace_all(., regex("Cecosf", ignore_case = TRUE), "CECOSF"))
  )

piv_estab_2024 <- piv2024 %>%
  clean_names() %>% 
  group_by(comuna, centro, codigo_centro) %>%
  summarise(piv_2024 = sum(inscritos)) %>%
  ungroup() %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
         across(where(is.character), ~ str_replace_all(., regex("Cerrillos De Nos", ignore_case = TRUE), "Ribera del Maipo")),
         across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
         across(where(is.character), ~ str_replace_all(., regex("Cecosf", ignore_case = TRUE), "CECOSF"))
  )

#rm(piv2024)
#rm(piv2025)


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

# 4. Filtrar base
usuarios <- usuarios %>%
  filter(!run %in% runs_a_eliminar) %>%
  filter(servicio_salud %in% servicios_salud_validos)

####################### Manejo de la base de telesalud #####################

last_sol_date <- max(as.Date(data$fecha_solicitud, format='%d-%m-%Y %H:%M'))
first_day_this_month <- floor_date(last_sol_date, unit = "month")
last_day_previous_month <- rollback(first_day_this_month)

data1 <- data %>%
  select(id,cesfam,prioridad,fecha_solicitud,direccion,
         tipo_prestador,motivo_consulta,especificidad,
         estado, fecha_cierre, tipo_cierre, cargo,
         profesion, fecha_agenda, fecha_derivacion_1) %>%
  rename(centro = cesfam) %>% 
    mutate(
    ##Gestiono las fechas##
    fecha_solicitud = ymd_hms(fecha_solicitud, tz = "UTC"),
    hora_sol = hour(fecha_solicitud),
    fecha_solicitud = as.Date(fecha_solicitud, format='%d-%m-%Y %H:%M'),
    fecha_cierre = as.Date(fecha_cierre, format='%d-%m-%Y %H:%M'),
    fecha_agenda  = as.Date(fecha_agenda, format='%d-%m-%Y %H:%M'),
    fecha_derivacion_1  = as.Date(fecha_derivacion_1, format='%d-%m-%Y %H:%M'),
    mes_sol = month(fecha_solicitud),
    ano_sol = year(fecha_solicitud),
    dia_sol = weekdays(fecha_solicitud),
    mes_cierre = month(fecha_cierre),
    ano_cierre = year(fecha_cierre),
    month_agenda = month(fecha_agenda),
    year_agenda = year(fecha_agenda),
    mes_deriv = month(fecha_derivacion_1),
    year_deriv = year(fecha_derivacion_1),
    month_year_sol = ymd(paste(ano_sol, mes_sol, "1", sep = "-")),
    month_year_cierre = ymd(paste(ano_cierre, mes_cierre, "1", sep = "-")), 
    month_year_agenda = ymd(paste(year_agenda, month_agenda, "1", sep = "-")),
    month_year_deriv = ymd(paste(year_deriv, mes_deriv, "1", sep = "-")),
    ##Cambio los nombres para join con base deis##
    centro = ifelse(centro == 'CESFAM Eduardo Frei Montalva',
                    'Centro de Salud Familiar Eduardo Frei Montalva', centro),
    centro = ifelse(centro == 'Centro Comunitario de Salud Familiar Juan Aravena',
                    'Centro Comunitario de Salud Familiar Dr. Salvador Allende', centro),
    centro = ifelse(centro == 'Centro de Salud Familiar Juan Pablo II',
                    'Centro de Salud Familiar Juan Pablo II ( San Bernardo)', centro),
    centro = ifelse(centro == 'CECOSF Juan Aravena',
                    'Centro Comunitario de Salud Familiar Dr. Salvador Allende', centro),
    centro = ifelse(centro == 'CECOSF Eduardo Frei Montalva',
                    'Centro Comunitario de Salud Familiar Eduardo Frei Montalva', centro),
    centro = ifelse(centro == 'Centro Comunitario De Salud Familiar Cerrillos De Nos',
                    'Centro Comunitario de Salud Familiar Ribera del Maipo', centro),
    centro = ifelse(centro == 'CECOSF Dr. Miguel Enríquez Espinosa',
                    'Centro Comunitario de Salud Familiar Dr. Miguel Enríquez Espinosa', centro)) %>%
  filter(fecha_solicitud < first_day_this_month) %>%
  left_join(deis %>% select(codigo_centro, comuna, centro)) %>%
  select(
    id, comuna, codigo_centro, centro, prioridad, fecha_solicitud,
    tipo_prestador, motivo_consulta, especificidad, estado,
    fecha_cierre, tipo_cierre, cargo, profesion,
    fecha_agenda, fecha_derivacion_1, month_year_sol, month_year_cierre, month_year_agenda,
    month_year_deriv, dia_sol, hora_sol)



#rm(data)
rm(deis)


################### Dataframe por comuna ########################################

data_por_comuna <-  split(data1, data1$comuna)

################### Dataframe por establecimiento ########################################


data_por_establecimiento <-  split(data1, data1$centro)

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