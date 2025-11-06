# --- INICIO DEL SCRIPT DE ANÁLISIS ---

# ******************************************************************************
# ⚠️ ADVERTENCIA: SELECCIÓN DE COMUNA
# El valor de 'n' debe ser configurado manualmente en este script
# o en el entorno para seleccionar la comuna a analizar.
# Ejemplo: n <- 1
# ******************************************************************************

# Definir la comuna en uso (ajustar 'n' al índice de la comuna deseada)
n <- 1 # <-- AJUSTAR ESTE ÍNDICE
comuna1 <- data_por_comuna[[n]]
comuna_en_uso <- unique(comuna1$comuna)[1]

# ------------------------------------------------------------------------------
# 1. Preparación de Bases PIV
# ------------------------------------------------------------------------------

# Asegurar que piv_comuna tiene ambas PIV y se filtra por la comuna en uso
piv_comuna_2024_2025 <- piv_comuna_2024 %>%
  left_join(piv_comuna_2025, by = "comuna")

piv_comuna <- piv_comuna_2024_2025 %>%
  filter(comuna == comuna_en_uso)


# ------------------------------------------------------------------------------
# 2. Reestructuración de Variables de Fecha (para el informe)
# ------------------------------------------------------------------------------

# Se definen las fechas de inicio y fin del periodo de 12 meses (necesario para el informe)
mesesatras_12 <- floor_date(last_day_previous_month, "month") - months(11)
anno <- floor_date(last_day_previous_month, "month") - years(1)

# ------------------------------------------------------------------------------
# 3. Cálculos de Tasa de Solicitudes (Gráfico 1, 2, 3)
# ------------------------------------------------------------------------------

# **tasa_solicitudes_mensual_comuna** (Para el Gráfico 1)
tasa_solicitudes_mensual_comuna <- comuna1 %>%
  group_by(date = month_year_sol) %>%
  summarise(total_sol = n(), .groups = 'drop')


# **tasa_solicitudes_mensual_establecimiento** (Para el Gráfico 2 y 3)
tasa_solicitudes_mensual_establecimiento <- comuna1 %>%
  # Se une PIV por centro para cálculo de tasa
  left_join(piv_estab_2024 %>% select(centro, piv_2024), by = "centro") %>%
  group_by(date = month_year_sol, centro) %>%
  summarise(n_sol = n(), piv_2024 = first(piv_2024), .groups = 'drop') %>%
  mutate(
    # Tasa por 1000 personas: (solicitudes / PIV) * 1000
    sol_per_capita = (n_sol / piv_2024) * 1000
  )

# **total_sol_centro** (Para tabla del informe)
total_sol_centro <- comuna1 %>%
  group_by(centro) %>%
  summarise(`Total de solicitudes` = n(), .groups = 'drop') %>%
  # Se une PIV para completar la tabla del informe
  left_join(piv_estab_2024 %>% select(centro, piv_2024), by = "centro") %>%
  mutate(
    # Tasa estandarizada por meses de implementación (simplificando a 1 mes por ahora)
    `N°solicitudes/1000 personas/mes` = round((`Total de solicitudes` / piv_2024) * 1000, 2),
    `PIV 2025` = piv_2024
  ) %>%
  select(centro, `Total de solicitudes`, `PIV 2025`, `N°solicitudes/1000 personas/mes`)

# ------------------------------------------------------------------------------
# 4. Proporción de Pendientes (Tabla de Proporciones)
# ------------------------------------------------------------------------------

# Se asume que 'data1' contiene todas las solicitudes y es usado para el total SSMS
proporcion_pendientes_comuna <- data1 %>%
  filter(fecha_solicitud >= mesesatras_12) %>%
  filter(estado == 'Pendiente') %>%
  # Calcular la proporción solo en la comuna en uso
  summarise(total_pendientes_comuna = n()) %>%
  mutate(total_solicitudes = nrow(comuna1),
         proportion = total_pendientes_comuna / total_solicitudes)


# **proporcion_pendientes_centro_ano** (Para tabla del informe)
proporcion_pendientes_centro_ano <- comuna1 %>%
  filter(fecha_solicitud >= mesesatras_12) %>%
  group_by(centro) %>%
  summarise(
    `Total de solicitudes` = n(),
    `Total de solicitudes pendientes` = sum(estado == 'Pendiente', na.rm = TRUE),
    `Proporción de solicitudes pendientes` = round((`Total de solicitudes pendientes` / `Total de solicitudes`) * 100, 2),
    .groups = 'drop'
  ) %>%
  filter(`Total de solicitudes pendientes` > 0)


# ------------------------------------------------------------------------------
# 5. Cálculos de Tiempo de Espera (SCA: Solicitud-Cierre-Agenda)
# ------------------------------------------------------------------------------

# Base para tiempo de espera: solo cerradas en el último año
base_cierre_anual <- comuna1 %>%
  filter(fecha_cierre >= anno) %>%
  mutate(
    dias_al_cierre = as.numeric(fecha_cierre - fecha_solicitud)
  )

# **promedio_tiempo_espera_comuna** (Para texto del informe)
promedio_tiempo_espera_comuna <- base_cierre_anual %>%
  summarise(promedio = round(mean(dias_al_cierre, na.rm = TRUE), 2))

# **promedio_tiempo_espera_establecimiento** (Para tabla del informe)
promedio_tiempo_espera_establecimiento <- base_cierre_anual %>%
  group_by(centro) %>%
  summarise(promedio = round(mean(dias_al_cierre, na.rm = TRUE), 2), .groups = 'drop') %>%
  mutate(
    promedio = scales::comma(promedio, big.mark = ".", decimal.mark = ",") # Se mantiene la función comma
  ) %>%
  rename(Centro = centro, `Media de días de espera` = promedio)


# **promedio_dias_al_cierre_mensual_establecimiento** (Para Gráfico 4)
promedio_dias_al_cierre_mensual_establecimiento <- base_cierre_anual %>%
  group_by(date = month_year_cierre, centro) %>%
  summarise(mean_days = round(mean(dias_al_cierre, na.rm = TRUE), 2), .groups = 'drop')

# **promedio_espera_prioridad_comuna** (Para tabla del informe)
promedio_espera_prioridad_comuna <- base_cierre_anual %>%
  group_by(prioridad) %>%
  summarise(Media_dias_espera = round(mean(dias_al_cierre, na.rm = TRUE), 2), .groups = 'drop')

# **promedio_dias_al_cierre_prioridad_establecimiento2** (Para Gráfico 5)
promedio_dias_al_cierre_prioridad_establecimiento2 <- base_cierre_anual %>%
  group_by(centro, Priority = prioridad) %>%
  summarise(Mean_Days = round(mean(dias_al_cierre, na.rm = TRUE), 2), .groups = 'drop') %>%
  filter(!is.na(Priority))

# **promedio_espera_prestador_comuna** (Para tabla del informe)
promedio_espera_prestador_comuna <- base_cierre_anual %>%
  group_by(tipo_prestador) %>%
  summarise(Media_dias_espera = round(mean(dias_al_cierre, na.rm = TRUE), 2), .groups = 'drop')

# **promedio_diasalcierre_prest_long** y **promedio_diasalcierre_otrosprest_long** (Para Gráfico 6)
base_prestador_anual <- base_cierre_anual %>%
  group_by(centro, Prestador = tipo_prestador) %>%
  summarise(Mean_Days = round(mean(dias_al_cierre, na.rm = TRUE), 2), .groups = 'drop')

promedio_diasalcierre_prest_long <- base_prestador_anual %>% filter(Prestador != 'Otros prestadores')
promedio_diasalcierre_otrosprest_long <- base_prestador_anual %>% filter(Prestador == 'Otros prestadores')

# ------------------------------------------------------------------------------
# 6. Cálculos de Tipos de Cierre (Gráfico 7)
# ------------------------------------------------------------------------------

# **tipos_de_cierre** (Para Gráfico 7)
tipos_de_cierre <- base_cierre_anual %>%
  filter(!is.na(tipo_cierre)) %>%
  group_by(centro, tipo_cierre) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(centro) %>%
  mutate(
    prop_tipo_cierre = n / sum(n),
    # Bandera para etiquetar la barra con mayor proporción
    top = (prop_tipo_cierre == max(prop_tipo_cierre, na.rm = TRUE))
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 7. Cálculos de Proporción Cierre/Solicitud
# ------------------------------------------------------------------------------

# Base: Solicitudes y Cierres en el último año (por mes y centro)
proporcion_cierre_sol_base <- comuna1 %>%
  # Filtramos solo las solicitudes *ingresadas* en los últimos 12 meses
  filter(month_year_sol >= mesesatras_12) %>%
  # Usamos month_year_sol como la referencia temporal
  group_by(date = month_year_sol, centro) %>%
  summarise(
    # Cuenta las solicitudes ingresadas en ese mes
    n_sol = n(),
    # Cuenta los cierres (con fecha de cierre *en cualquier momento* si es en ese mes de ingreso)
    n_cierre = sum(!is.na(fecha_cierre) & month_year_cierre == month_year_sol, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    proportion = n_cierre / n_sol # Proporción para Gráfico 8
  )

# **proporcion_cierre_por_solicitud_mensual_establecimiento** (Para Gráfico 8)
proporcion_cierre_por_solicitud_mensual_establecimiento <- proporcion_cierre_sol_base

# **proporcion_cierre_por_solicitud_ano_comuna** (Para tabla del informe)
proporcion_cierre_por_solicitud_ano_comuna <- proporcion_cierre_sol_base %>%
  group_by(comuna = comuna_en_uso) %>% # Agregamos la comuna de forma manual
  summarise(
    total_solicitudes = sum(n_sol, na.rm = TRUE),
    total_cierres = sum(n_cierre, na.rm = TRUE),
    cie_sol = round(total_cierres / total_solicitudes, 2),
    .groups = 'drop'
  )

# **proporcion_cierre_por_solicitud_ano_prestador_establecimiento** (Para tabla del informe)
proporcion_cierre_por_solicitud_ano_prestador_establecimiento <- comuna1 %>%
  filter(month_year_sol >= mesesatras_12) %>%
  group_by(centro, tipo_prestador) %>%
  summarise(
    total_solicitudes = n(),
    total_cierres = sum(!is.na(fecha_cierre) & month_year_cierre >= mesesatras_12 & month_year_cierre <= last_day_previous_month, na.rm = TRUE),
    cie_sol = round(total_cierres / total_solicitudes, 2),
    .groups = 'drop'
  )

# **GENERACIÓN DE 10 Z-BASES PARA GRÁFICOS (Manteniendo nombres originales)**

generar_base_prestador <- function(df, prestador_nombre) {
  df %>%
    filter(tipo_prestador == prestador_nombre, month_year_sol >= mesesatras_12) %>%
    group_by(date = month_year_sol, centro) %>%
    summarise(
      n_sol = n(),
      n_cierre = sum(!is.na(fecha_cierre) & month_year_cierre == month_year_sol, na.rm = TRUE),
      cie_sol = n_cierre / n_sol,
      .groups = 'drop'
    )
}

z_proporcion_cierre_por_solicitud_medico_mensual_establecimiento <- generar_base_prestador(comuna1, "Medicina")
z_proporcion_cierre_por_solicitud_dental_mensual_establecimiento <- generar_base_prestador(comuna1, "Odontología")
z_proporcion_cierre_por_solicitud_matrona_mensual_establecimiento <- generar_base_prestador(comuna1, "Matrona")
z_proporcion_cierre_por_solicitud_asistente_social_mensual_establecimiento <- generar_base_prestador(comuna1, "Asistente Social")
z_proporcion_cierre_por_solicitud_enfermeria_mensual_establecimiento <- generar_base_prestador(comuna1, "Enfermería")
z_proporcion_cierre_por_solicitud_kinesiologia_mensual_establecimiento <- generar_base_prestador(comuna1, "Kinesiología")
z_proporcion_cierre_por_solicitud_nutricion_mensual_establecimiento <- generar_base_prestador(comuna1, "Nutrición")
z_proporcion_cierre_por_solicitud_psicologia_mensual_establecimiento <- generar_base_prestador(comuna1, "Psicología")
z_proporcion_cierre_por_solicitud_tens_mensual_establecimiento <- generar_base_prestador(comuna1, "Técnico en enfermería")
z_proporcion_cierre_por_solicitud_to_mensual_establecimiento <- generar_base_prestador(comuna1, "Terapia Ocupacional")

# ------------------------------------------------------------------------------
# 8. Cálculos de Tiempo SCA Final (Solicitud-Cierre-Agenda)
# ------------------------------------------------------------------------------

base_sca_anual <- comuna1 %>%
  filter(fecha_cierre >= mesesatras_12) %>%
  filter(tipo_cierre %in% c("Agendado para atención presencial", "Agendado para atención por telemedicina", "Agendado para orden de examen")) %>%
  mutate(
    dias_sca = as.numeric(fecha_agenda - fecha_solicitud)
  ) %>%
  filter(dias_sca != 0) # Solo casos donde cierre y agendamiento no son el mismo día

# **tabla_dias_sol_cierre_agenda_establecimiento**
tabla_dias_sol_cierre_agenda_establecimiento <- base_sca_anual %>%
  group_by(centro) %>%
  summarise(
    `Media de días Sol-Agenda` = round(mean(dias_sca, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate_if(is.numeric, ~ scales::comma(., big.mark = ".", decimal.mark = ","))

# **tabla_dias_sol_cierre_agenda_establecimiento_tipo_cierre**
tabla_dias_sol_cierre_agenda_establecimiento_tipo_cierre <- base_sca_anual %>%
  group_by(tipo_cierre) %>%
  summarise(
    `Media de días Sol-Agenda` = round(mean(dias_sca, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate_if(is.numeric, ~ scales::comma(., big.mark = ".", decimal.mark = ","))

# **tabla_dias_sol_cierre_agenda_establecimiento_prioridad**
tabla_dias_sol_cierre_agenda_establecimiento_prioridad <- base_sca_anual %>%
  group_by(prioridad) %>%
  summarise(
    `Media de días Sol-Agenda` = round(mean(dias_sca, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate_if(is.numeric, ~ scales::comma(., big.mark = ".", decimal.mark = ",")) %>%
  rename(Prioridad = prioridad)


# **tabla_dias_sol_cierre_agenda_establecimiento_prestador**
tabla_dias_sol_cierre_agenda_establecimiento_prestador <- base_sca_anual %>%
  group_by(tipo_prestador) %>%
  summarise(
    `Media de días Sol-Agenda` = round(mean(dias_sca, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  mutate_if(is.numeric, ~ scales::comma(., big.mark = ".", decimal.mark = ",")) %>%
  rename(Prestador = tipo_prestador)


# --- FIN DEL SCRIPT DE ANÁLISIS ---

