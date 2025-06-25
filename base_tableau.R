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

data <- import('data/base_junio_2025.csv') %>% clean_names() 
piv <- import('data/piv2025.xlsx') %>% clean_names()
deis <- import('data/deis_ssms.xlsx') %>% clean_names() %>% rename(centro = nombre_oficial,comuna = nombre_comuna, codigo_centro = codigo_vigente)

####################### gestionar bases ###################################################


data2 <- data %>%
  select(cesfam,prioridad,fecha_solicitud,genero,edad,tipo_prestador,motivo_consulta,especificidad,
         estado,fecha_cierre,tipo_cierre,cerrado_por,cargo,profesion,fecha_agenda) %>%
  mutate(
    cesfam = ifelse(cesfam == 'CESFAM Eduardo Frei Montalva',
                    'Centro de Salud Familiar Eduardo Frei Montalva', cesfam),
    cesfam = ifelse(cesfam == 'Centro Comunitario de Salud Familiar Juan Aravena',
                    'Centro Comunitario de Salud Familiar Dr. Salvador Allende', cesfam),
    cesfam = ifelse(cesfam == 'CECOSF Juan Aravena',
                    'Centro Comunitario de Salud Familiar Dr. Salvador Allende', cesfam),
    cesfam = ifelse(cesfam == 'Centro de Salud Familiar Juan Pablo II',
                    'Centro de Salud Familiar Juan Pablo II ( San Bernardo)', cesfam),
    cesfam = ifelse(cesfam == 'CECOSF Dr. Miguel Enríquez Espinosa',
                    'Centro Comunitario de Salud Familiar Dr. Miguel Enríquez Espinosa', cesfam),
    cesfam = ifelse(cesfam == ' Comunitario De Salud Familiar Cerrillos De Nos',
                    'Centro Comunitario de Salud Familiar Ribera del Maipo', cesfam),
    cesfam = ifelse(cesfam == 'CECOSF Eduardo Frei Montalva',
                    'Centro Comunitario de Salud Familiar Eduardo Frei Montalva', cesfam),
    fecha_solicitud = as.Date(fecha_solicitud, format='%d-%m-%Y %H:%M'),
    fecha_cierre = as.Date(fecha_cierre, format='%d-%m-%Y %H:%M'),
    fecha_agenda  = as.Date(fecha_agenda, format='%d-%m-%Y %H:%M')
  )


cesfam <- as.data.frame(unique(data2$cesfam)) %>%
  rename(centro = 'unique(data2$cesfam)') %>%
  left_join(
    deis %>% select(codigo_centro,centro,comuna),
    by = 'centro'
  ) %>%
  left_join(piv %>% select(codigo_centro, inscritos) %>% 
              group_by(codigo_centro) %>%
              summarise(n=sum(inscritos)), by = 'codigo_centro')


data2 <- data2 %>%
  left_join(
    cesfam, by = c('cesfam'='centro')
  ) %>%
  mutate(
    across(where(is.character), ~ str_replace_all(., regex("Centro de Salud Familiar", ignore_case = TRUE), "CESFAM")),
    across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario de Salud Familiar", ignore_case = TRUE), "CECOSF")),
    across(where(is.character), ~ str_replace_all(., regex("Centro Comunitario De Salud Familiar", ignore_case = TRUE), "CECOSF")),
    across(where(is.character), ~ str_replace_all(., regex("Posta de Salud Rural", ignore_case = TRUE), "PSR")),
    across(where(is.character), ~ str_replace_all(., regex("calera de tango", ignore_case = TRUE), "Calera De Tango"))
  )

export(data2,'telesalud_tableau_junio2025.csv')


