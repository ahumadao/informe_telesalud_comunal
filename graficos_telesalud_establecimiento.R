pacman::p_load(
  tidyverse, 
  here,       
  janitor,    
  lubridate,  
  summarytools,
  rio,
  ggthemes,
  ggsci,
  readxl,
  ggplot2
)

####################### Custom Theme for plots   #######################

custom_theme <- theme_minimal() + 
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 6), 
    legend.title = element_text(size = 7, face = "italic"),
    legend.text = element_text(size = 5),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(), 
    axis.line = element_line(linewidth = 0.3, color = "black"), 
    legend.background = element_rect(fill = "white"), 
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(0.5, "cm"),  # Adjust size of legend keys
    legend.spacing.y = unit(0.01, "cm"),
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20))

#Combinar todos los datos y calcular span adaptativo por prestador
datos_combinados <- bind_rows(
  z_proporcion_cierre_por_solicitud_medico_mensual_establecimiento %>% mutate(Prestador = "Medicina"),
  z_proporcion_cierre_por_solicitud_dental_mensual_establecimiento %>% mutate(Prestador = "Odontología"),
  z_proporcion_cierre_por_solicitud_matrona_mensual_establecimiento %>% mutate(Prestador = "Matrona"),
  z_proporcion_cierre_por_solicitud_asistente_social_mensual_establecimiento %>% mutate(Prestador = "Asistente Social"),
  z_proporcion_cierre_por_solicitud_enfermeria_mensual_establecimiento %>% mutate(Prestador = "Enfermería"),
  z_proporcion_cierre_por_solicitud_kinesiologia_mensual_establecimiento %>% mutate(Prestador = "Kinesiología"),
  z_proporcion_cierre_por_solicitud_nutricion_mensual_establecimiento %>% mutate(Prestador = "Nutrición"),
  z_proporcion_cierre_por_solicitud_psicologia_mensual_establecimiento %>% mutate(Prestador = "Psicología"),
  z_proporcion_cierre_por_solicitud_tens_mensual_establecimiento %>% mutate(Prestador = "Técnico en enfermería"),
  z_proporcion_cierre_por_solicitud_to_mensual_establecimiento %>% mutate(Prestador = "Terapia Ocupacional")
)


# Gráfico 2: Evolución histórica de solicitudes de atención ----

graf_mensual_sol_estab <- ggplot(tasa_solicitudes_mensual_establecimiento, 
                                 aes(x = date, y = n_sol, color = centro, group = centro)) +
  geom_line(linewidth = 0.5, alpha = 0.2 )  +
  geom_smooth(method = "loess", se = FALSE, linewidth=0.5, span = 0.3) +
  labs(x = "Fecha", 
       y = "Cantidad de Solicitudes", 
       title = "Evolución de solicitudes de atención por establecimiento",
       subtitle = paste0('Media suavizada. Plataforma Telesalud. ', establecimiento_en_uso, ' .SSMS. 2021-2025'),
       color = 'Centro') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_sol_estab


# Gráfico 3: Evolución histórica de solicitudes de atención  ----


graf_mensual_sol_estab_suavizado_piv <- ggplot(tasa_solicitudes_mensual_establecimiento, 
                                               aes(x = date, y = round(sol_per_capita,2), color = centro, group = centro)) +
  geom_line(linewidth = 0.5, alpha = 0.2 )  +
  geom_smooth(method = "loess", se = FALSE, linewidth=0.5, span = 0.3) +
  labs(x = "Fecha", y = "Cantidad/1000 personas inscritas", 
       title = "Tasa de solicitudes por cada 1000 personas inscritas.",
       subtitle = paste0('Media suavizada. Por establecimiento. \nPlataforma Telesalud. ', establecimiento_en_uso, ' .SSMS. 2021-2025'),
       color = 'Centro') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

#graf_mensual_sol_estab_suavizado_piv


# Gráfico 4:  Evolucion tiempo de espera por establecimiento ----

graf_mensual_diasalcierre_establecimiento <- ggplot(promedio_dias_al_cierre_mensual_establecimiento, 
                                                    aes(x = date, y=mean_days , color = centro, group = centro)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Fecha", y = "Días de espera (promedio mensual)", 
       title = 'Media mensual de días de espera \n al cierre de solicitudes por establecimiento',
       subtitle = paste0('Plataforma Telesalud. Gráfico suavizado. ',establecimiento_en_uso,'. SSMS. 2025'),
       color = 'Centro') +
  custom_theme +
  scale_colour_tableau("Tableau 20") +
  guides(color = "none")  # Eliminar solo la leyenda de color

graf_mensual_diasalcierre_establecimiento


# Gráfico 5:  Evolucion tiempo de espera según prioridad, por establecimiento ----

graf_promedio_por_prioridad <- ggplot(promedio_dias_al_cierre_prioridad_establecimiento2, 
                                      aes(x = centro, y = Mean_Days, 
                                          fill = factor(Priority))) +
  geom_col(position = position_dodge()) +
  labs(title = "Media de días de espera para el cierre por establecimiento, según prioridad",
       subtitle = paste0('Plataforma Telesalud. ',establecimiento_en_uso,'. SSMS. 2025'),
       x = "Centro",
       y = "Días",
       fill = "Prioridad") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_colour_tableau("Tableau 20")

graf_promedio_por_prioridad

# Gráfico 6:  Evolucion tiempo de espera según prestador, por establecimiento ----

graf_promedio_por_prestador<- ggplot(promedio_diasalcierre_todoprest_long, 
                                     aes(x = centro, y = Mean_Days, 
                                         fill = factor(Prestador))) +
  geom_col(position = position_dodge()) +
  labs(title = "Media de días de espera comunal para el cierre, según prestador",
       subtitle = paste0('Plataforma Telesalud. ',establecimiento_en_uso,'. SSMS. 2025'),
       x = "Comuna",
       y = "Días",
       fill = "Prestador") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_colour_tableau("Tableau 20")

graf_promedio_por_prestador

# Gráfico 7:  Tipos de cierre  ----

graf_tiposdecierre <- ggplot(tipos_de_cierre, aes(x = centro, y = prop_tipo_cierre, 
                                                     fill = factor(tipo_cierre))) +
  geom_col(position = 'dodge2') +  # 'dodge2' para mejor espaciado
  geom_text(data = tipos_de_cierre %>% filter(top == TRUE), 
            aes(label = round(prop_tipo_cierre, 2)), 
            position = position_dodge2(width = 0.9),
            vjust = -0.5,
            size = 2,
            color = 'gray25') +
  labs(title = "Proporción de tipos de cierre por establecimiento",
       subtitle = paste0('Plataforma Telesalud. ', establecimiento_en_uso, '. SSMS. 2025'),
       x = "Centro",
       y = "Proporción",
       fill = "Tipo de cierre") +
  custom_theme + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = 'Set2')



graf_tiposdecierre



####################### Gráfico mensual cierre comuna 

graf_mensual_cierre_comuna <- ggplot(tasa_solicitudes_mensual_establecimiento, aes(x = date), color = centro) +
  geom_line(aes(y = n_cierre, color = Comuna)) +
  labs(x = "Fecha", y = "Cantidad de cierre de solicitudes", 
       title = "Cierres de solicitudes de Telesalud por comuna",
       subtitle = 'Plataforma Telesalud.SSMS. 2021-2025',
       color = 'Comuna') +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_cierre_comuna


# Gráfico 8: Proporción cierre / solicitud mensual por establecimiento ----

graf_mensual_prop_estab <- ggplot(proporcion_cierre_por_solicitud_mensual_establecimiento %>%
                                    #rename(
                                    #date = month_year_sol) %>% 
                                    mutate(
                                      date= ymd(date)), 
                                  aes(x = date, y = proportion), color = centro, group=centro) +
  geom_line(aes(y = proportion, color = centro))+
  geom_smooth(method = "loess", se = FALSE, linewidth=0.3
              , span = 0.7, alpha = 0.2, color = '#8d3f9b')+
  labs(x = "Fecha", y = "Proporción de cierres por solicitud", 
       title = "Proporción de cierres por solicitud,\n por establecimiento",
       subtitle = paste0('Plataforma Telesalud. ',establecimiento_en_uso,'. SSMS. 2025'),
       color = 'Centro') +
  custom_theme + 
  scale_colour_tableau("Tableau 20") +
  ylim(0.5,1.5) +
  guides(color = "none")  # Eliminar solo la leyenda de color



graf_mensual_prop_estab




# Gráfico 9-11: Gráficos proporción por prestador, mensual y por establecimiento ---- 

datos_promedio_prestador <- datos_combinados %>%
  group_by(date, Prestador) %>%
  summarise(cie_sol_promedio = mean(cie_sol, na.rm = TRUE), .groups = 'drop')

graf_mensual_prop_promedio_prestadores <- ggplot(datos_promedio_prestador, 
                                                 aes(x = ymd(date), y = cie_sol_promedio, color = Prestador, group = Prestador)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.7) +
  labs(x = "Fecha", y = "Proporción cierre/solicitudes (promedio)", 
       title = 'Proporción promedio de cierre/solicitudes por prestador',
       subtitle = paste0('Gráfico suavizado. Plataforma Telesalud. ', establecimiento_en_uso, '. SSMS. 2025'),
       color = "Prestador") +
  custom_theme + 
  scale_colour_tableau("Tableau 20")

graf_mensual_prop_promedio_prestadores
#Varios ----
# 
# 
# # Gráfico mensual solicitudes suavizada 
# 
# graf_mensual_sol_comuna_suavizado <- ggplot(mensual_sol_comuna_piv, aes(x = date, y = n_sol, color = Comuna)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Cantidad de Solicitudes", 
#        title = "Solicitudes de Telesalud por comuna (suavizado)",
#        subtitle = paste0('Plataforma Telesalud. ',establecimiento_en_uso,'. SSMS. 2025'),
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# # Print the plot
# graf_mensual_sol_comuna_suavizado
# 
# ####################### Gráfico mensual solicitudes ssms suavizado 
# 
# graf_mensual_sol_ssms_suavizado <- ggplot(mensual_sol_ssms_2124, aes(x = date, y = mean_sol)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Cantidad de solicitudes", 
#        title = "Cantidad de solicitudes promedio por mes. SSMS. (suavizado).",
#        subtitle = 'Plataforma Telesalud. SSMS. 2021-2025.',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_sol_ssms_suavizado
# 
# 
# ####################### Gráfico mensual proporción comuna suavizado 
# 
# graf_mensual_prop_comuna_suavizado <- ggplot(mensual_proportion_comuna, aes(x = date, y=proportion , color = Comuna)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Proporción cierre/solicitudes", 
#        title = 'Proporción de cierres por solicitud',
#        subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2023-2025',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# # Print the plot
# graf_mensual_prop_comuna_suavizado
# 
# 
# 
# ####################### Gráfico mensual espera al cierra comuna suavizado 
# 
# graf_mensual_diasalcierre_comuna <- ggplot(mensual_diasalcierre, aes(x = date, y=mean_days , color = Comuna)) +
#   geom_smooth(method = "loess", se = FALSE) +
#   labs(x = "Fecha", y = "Días de espera (promedio)", 
#        title = 'Media de días de espera al cierre de solicitudes',
#        subtitle = 'Plataforma Telesalud. Gráfico suavizado. SSMS. 2025',
#        color = 'Comuna') +
#   custom_theme +
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_diasalcierre_comuna
# 
# 
# ############## Mensual sol comuna ssms
# 
# graf_mensual_sol_ssms_comuna_cantidad <- ggplot(mensual_sol_ssms_2124, aes(x = date, y=total_sol)) +
#   geom_line(aes(y = mean_sol),linewidth=0.5, color='#4E79A7') +
#   labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
#        title = "Cantidad de solicitudes por mes. SSMS.",
#        subtitle = 'Media suavizada. Plataforma Telesalud. SSMS. 2021-2025',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_sol_ssms_comuna_cantidad
# 
# graf_mensual_sol_ssms_comuna <- ggplot(mensual_sol_ssms_2124, aes(x = date, y=mean_sol)) +
#   geom_line(aes(y = mean_sol),linewidth=0.5, color='#4E79A7') +
#   geom_smooth(method = "loess", se = FALSE, linewidth=0.5, color='#A0CBE8') +
#   labs(x = "Fecha", y = "Cantidad de Solicitudes.", 
#        title = "Media cantidad de solicitudes por mes. SSMS.",
#        subtitle = 'Media suavizada. Plataforma Telesalud. SSMS. 2021-2025',
#        color = 'Comuna') +
#   custom_theme + 
#   scale_colour_tableau("Tableau 20")
# 
# graf_mensual_sol_ssms_historico
# 
# 
# 