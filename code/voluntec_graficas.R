source("voluntec_procesado.R")
source("voluntec_stats.R")

# === PALETAS ====
paleta_likert <- c(
  "0" = "#c0392b", # Rojo oscuro
  "1" = "#e67e22", # Naranja
  "2" = "#f39c12", # Amarillo oscuro
  "3" = "#aed6f1", # Azul muy claro
  "4" = "#3498db", # Azul medio
  "5" = "#21618c"  # Azul oscuro
)

paleta_pastel_moderna <- c(
  "#E07A5F", # Terracota suave
  "#F4A261", # Arena / Naranja apagado
  "#81B29A", # Verde salvia
  "#457B9D", # Azul acero
  "#9D8189", # Malva / Rosa polvo
  "#3D405B"  # Pizarra / Azul noche
)

paleta_bienestar <- c(
  "#E07A5F", # Terracota suave
  "#F4A261", # Arena / Naranja apagado
  "#E9C46A", # Mostaza suave (NUEVO)
  "#81B29A", # Verde salvia
  "#457B9D", # Azul acero
  "#9D8189", # Malva / Rosa polvo
  "#3D405B"  # Pizarra / Azul noche
)

paleta_bienestartec <- c(
  "#E07A5F", # Terracota suave
  "#F4A261", # Arena 
  "#81B29A", # Verde salvia
  "#457B9D", # Azul acero
  "#3D405B"  # Pizarra / Azul noche
)

# === DIAGRAMA DE PONDERACIÓN ====
local({
diagrama_ponderación <- function(data, var_name, titulo_var, nombre_archivo) {
  
  # 1. Calculamos Muestra
  muestra <- data |> 
    count(.data[[var_name]]) |> 
    mutate(
      proporcion = n / sum(n), 
      tipo = "Muestra (Sin pesos)"
    ) |> 
    rename(categoria = all_of(var_name)) |>  # Renombramos temporalmente para unificar
    select(categoria, proporcion, tipo)
  
  # 2. Calculamos Población ponderada
  poblacion <- data |> 
    group_by(.data[[var_name]]) |> 
    summarise(total_w = sum(weight, na.rm = TRUE), .groups = "drop") |> 
    mutate(
      proporcion = total_w / sum(total_w), 
      tipo = "Población (Ponderada)"
    ) |> 
    rename(categoria = all_of(var_name)) |> 
    select(categoria, proporcion, tipo)
  
  # 3. Juntamos y graficamos
  p <- bind_rows(muestra, poblacion) |> 
    ggplot(aes(x = reorder(categoria, proporcion), y = proporcion, fill = tipo)) +
    geom_col(position = "dodge") + 
    coord_flip() + 
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("Comparativa: Muestra vs. Población (", titulo_var, ")", sep = ""),
      subtitle = "Efecto del ajuste de pesos en la distribución",
      x = NULL,
      y = "Proporción",
      fill = "Origen del dato"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 13),
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 10))
    )
  
  # 4. Guardamos el gráfico
  ggsave(nombre_archivo, plot = p, width = 8, height = 6, dpi = 300)
}

# CCAA
diagrama_ponderación(df, "comunidad_autonoma", "CCAA", "gráficas/diag_ponderacion_ccaa.png")

# Edad
diagrama_ponderación(df, "grupo_edad", "Grupos de Edad", "gráficas/diag_ponderacion_edad.png")
})

# === BARPLOTS DEMOGRÁFICAS ====
local({
vars_demo <- c("tamaño_pob", "situacion_lab", "sector_lab")

plot_univariante <- function(data, var_name) {
  data |>
    filter(!is.na(.data[[var_name]])) |>
    ggplot(aes(y = .data[[var_name]], fill = .data[[var_name]])) +
    geom_bar(alpha = 0.85, show.legend = FALSE) +
    scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
    labs(
      title = str_to_title(str_replace_all(var_name, "_", " ")),
      subtitle = "Distribución de frecuencias por categoría",
      x = "Recuento de respuestas",
      y = NULL
    ) +
    scale_x_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      # Fondo del panel suave (no blanco puro)
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      # Líneas de cuadrícula sutiles
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
      # Títulos y etiquetas
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 10)),
      axis.text.y = element_text(size = 13, color = "#34495e")
    )
}

plot_univariante_ord <- function(data, var_name) {
  data |>
    filter(!is.na(.data[[var_name]])) |>
    mutate(across(all_of(var_name), as.factor)) |>
    ggplot(aes(y = fct_rev(fct_infreq(.data[[var_name]])), fill = .data[[var_name]])) +
    geom_bar(alpha = 0.85, show.legend = FALSE) +
    scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
    labs(
      title = str_to_title(str_replace_all(var_name, "_", " ")),
      subtitle = "Distribución de frecuencias por categoría",
      x = "Recuento de respuestas",
      y = NULL
    ) +
    scale_x_continuous(
      labels = scales::label_number(big.mark = ".", decimal.mark = ",")
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      # Fondo del panel suave (no blanco puro)
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      # Líneas de cuadrícula sutiles
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
      # Títulos y etiquetas
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 10)),
      axis.text.y = element_text(size = 13, color = "#34495e")
    )
}

plot_muestra_pob <- plot_univariante(df, vars_demo[1]) + labs(
  title = "Estudio de la muestra: Tamaño de población",
  subtitle = "",
  x = NULL, 
  y = NULL
)
  

plot_muestra_lab <- plot_univariante_ord(df, vars_demo[2]) + labs(
  title = "Estudio de la muestra: Situación laboral",
  subtitle = "",
  x = NULL, 
  y = NULL
)

plot_muestra_sector <- plot_univariante_ord(df, vars_demo[3]) + labs(
  title = "Estudio de la muestra: Sector laboral",
  subtitle = "",
  x = NULL, 
  y = NULL
)

# Tamaño de Población
ggsave("gráficas/tamaño_pob.png", plot_muestra_pob, width = 8, height = 5, bg = "#fdfdfd")

# Situación Laboral
ggsave("gráficas/situacion_lab.png", plot_muestra_lab, width = 8, height = 5, bg = "#fdfdfd")

# Sector Laboral
ggsave("gráficas/sector_lab.png", plot_muestra_sector, width = 9, height = 5, bg = "#fdfdfd")
})

# === DIVERGENTES AFI ====

# 1. DICCIONARIO GLOBAL (Juntamos todas las variables con su nombre de columna original)
diccionario_global <- c(
  # Bloque AFI (Afirmaciones)
  "afi_quedarse" = "Las tecnologías han venido para quedarse",
  "afi_gente_atras" = "La revolución tecnológica está dejando a gente atrás",
  "afi_juventud_adiccion" = "La juventud tiene un problema de adicción a las tecnologías",
  "afi_oportunidades" = "La tecnología genera oportunidades y posibilidades que no existían antes",
  "afi_demasiado_pantallas" = "La gente pasa demasiado tiempo delante de las pantallas",
  "afi_reducir_brechas" = "La tecnología ayuda a reducir las brechas sociales",
  
  # Bloque DEF_VOL (Definiciones de Voluntariado)
  "def_vol_apoyo_admin" = "Es un mecanismo para apoyar a la Administración Pública y cubrir las necesidades para el bienestar social de todas las personas.",
  "def_vol_transfor_social" = "Es un medio para la participación y transformación social.",
  "def_vol_organizacion" = "Es una forma de organización ciudadana que permite ayudar al prójimo y reducir las desigualdades sociales.",
  "def_vol_ocio" = "Es un tipo de ocio que permite ocupar el tiempo libre ayudando a otras personas.",
  "def_vol_competencias" = "Es una forma de preparación para entrar en el mercado laboral o de adquirir competencias."
)

# 2. WRANGLING GENERALIZADO (Atrapa ambas familias de variables a la vez)
df_likert_pct <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado"
  ) |> 
  # Seleccionamos las columnas de ambas baterías
  select(perfil_voluntariado, starts_with("afi_"), starts_with("def_vol_"), weight) |> 
  pivot_longer(
    cols = c(starts_with("afi_"), starts_with("def_vol_")), 
    names_to = "variable", 
    values_to = "respuesta"
  ) |> 
  filter(!is.na(respuesta)) |> 
  mutate(
    respuesta = factor(as.character(respuesta), levels = c("0", "1", "2", "3", "4", "5")),
    
    # Traducimos y ordenamos los perfiles AQUÍ, de una sola vez
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c("Cibervoluntarios", "Voluntario actual", "Voluntario pasado", "Nunca ha sido voluntario"))
  ) |> 
  
  # Matemáticas de porcentajes ponderados
  group_by(variable, perfil_label, respuesta) |> 
  summarise(total_ponderado = sum(weight, na.rm = TRUE), .groups = "drop_last") |> 
  mutate(
    pct = total_ponderado / sum(total_ponderado),
    pct_plot = if_else(respuesta %in% c("0", "1", "2"), -pct, pct)
  ) |> 
  ungroup()

# Partimos en negativos y positivos
df_neg <- df_likert_pct |> filter(respuesta %in% c("0", "1", "2"))
df_pos <- df_likert_pct |> filter(respuesta %in% c("3", "4", "5"))

# 3. FUNCIÓN DE PLOT
plot_likert_individual <- function(cod_var) {
  
  d_neg <- df_neg |> filter(variable == cod_var)
  d_pos <- df_pos |> filter(variable == cod_var)
  
  # --- 2. LÍNEA ESTRATÉGICA PARA EL TÍTULO ---
  titulo_texto <- diccionario_global[cod_var]
  
  # Si la variable empieza por "def_vol_", modificamos la frase
  if (str_starts(cod_var, "def_vol_")) {
    titulo_texto <- paste0("El voluntariado ", tolower(titulo_texto))
  }
  
  # Finalmente, envolvemos todo (sea 'afi' o 'def') entre comillas dobles
  titulo_texto <- paste0('"', titulo_texto, '"')
  
  ggplot() +
    geom_col(data = d_neg, aes(x = pct_plot, y = perfil_label, fill = respuesta), width = 0.5) +
    geom_col(data = d_pos, aes(x = pct_plot, y = perfil_label, fill = respuesta), 
             width = 0.5, position = position_stack(reverse = TRUE)) +
    
    geom_vline(xintercept = 0, color = "gray30", linewidth = 0.8) +
    
    geom_text(data = d_neg, aes(x = pct_plot, y = perfil_label, group = respuesta, 
                                label = if_else(abs(pct) > 0.04, scales::percent(abs(pct), accuracy = 1), "")),
              position = position_stack(vjust = 0.5), size = 3.5, 
              color = if_else(d_neg$respuesta == "0", "white", "black")) +
    
    geom_text(data = d_pos, aes(x = pct_plot, y = perfil_label, group = respuesta, 
                                label = if_else(pct > 0.04, scales::percent(pct, accuracy = 1), "")),
              position = position_stack(vjust = 0.5, reverse = TRUE), size = 3.5, 
              color = if_else(d_pos$respuesta == "5", "white", "black")) +
    
    scale_fill_viridis_d(option = "mako", begin = 0.1, end = 0.9, name = "Valoración",
                         limits = c("0", "1", "2", "3", "4", "5"), guide = guide_legend(nrow = 1)) +
    scale_x_continuous(labels = \(x) scales::percent(abs(x)), limits = c(-1, 1)) +
    
    labs(
      title = str_wrap(titulo_texto, width = 85), 
      subtitle = "Grado de acuerdo segmentado por perfil",
      x = "Porcentaje de la muestra", y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "top", 
          axis.text.y = element_text(face = "bold", size = 11),
          panel.grid.major.y = element_blank())
}


# 4. EXPORTACIÓN MASIVA
# Esto generará de golpe los 11 gráficos (los 6 de 'afi' y los 5 de 'def_vol') 
# con nombres perfectos como "likert_afi_quedarse.png" o "likert_def_vol_ocio.png"
walk(unique(df_neg$variable), \(nombre) {
  p <- plot_likert_individual(nombre)
  archivo <- paste0("gráficas/likert_", nombre, ".png")
  ggsave(archivo, plot = p, width = 10, height = 4.5, dpi = 300)
})

# ==== COMPARATIVA RESP BIENESTAR Y TEC ====
local({
# Transformamos a formato largo para alinear todas las preguntas en una sola columna de valores
df_resp_long <- df |>
  select(starts_with("resp"), any_of("weight")) |>
  pivot_longer(
    cols = starts_with("resp"),
    names_to = "variable",
    values_to = "respuesta"
  ) |>
  filter(!is.na(respuesta)) |>
  mutate(
    # Diferenciamos la naturaleza del bienestar para poder agrupar visualmente las barras
    tipo_bienestar = if_else(str_detect(variable, "tec_"), "Tecnológico", "Social"),
    
    # Aislamos la entidad responsable para el emparejamiento en el eje Y
    actor_crudo = str_remove(variable, "resptec_|resp_"),
    actor = fct_recode(actor_crudo,
                       "Estado / AAPP"    = "estado",
                       "Empresas"         = "empresa",
                       "Tercer Sector"    = "tercer",
                       "Ciudadanía"       = "ciudadania",
                       "Inst. Educativas" = "educativas"),
    # Ordenamos actores y tipo_bienestar de arriba a abajo
    actor = factor(actor, levels = c(
      "Estado / AAPP", 
      "Inst. Educativas", 
      "Ciudadanía", 
      "Empresas", 
      "Tercer Sector"
    )),
    tipo_bienestar = factor(tipo_bienestar, levels = c("Tecnológico", "Social")))

# Agrupamos y calculamos las proporciones relativas para las barras apiladas.
# Se estructuran las respuestas para la divergencia
df_resp_summary <- df_resp_long |>
  group_by(actor, tipo_bienestar, respuesta) |>
  summarise(total = sum(weight), .groups = "drop_last") |>
  mutate(porcentaje = total / sum(total)) |>
  ungroup() |>
  mutate(
    respuesta = factor(respuesta, levels = c("0", "1", "2", "5", "4", "3")),
    pct_divergente = if_else(
      respuesta %in% c("0", "1", "2"), 
      -porcentaje, 
      porcentaje
    ))

# 2. Generación del gráfico con el ajuste en la leyenda
p_resp_divergente <- ggplot(df_resp_summary, aes(x = pct_divergente, y = tipo_bienestar, fill = respuesta)) +
  geom_col(width = 0.8) +
  facet_grid(actor ~ ., switch = "y", scales = "free_y", space = "free_y") +
  # Usamos 'breaks' para obligar a la leyenda a ordenarse lógicamente de 0 a 5, 
  # anulando el desorden intencional que creamos en los levels del factor.
  scale_fill_manual(
    breaks = c("0", "1", "2", "3", "4", "5"),
    values = paleta_pastel_moderna,
    name = "Nivel de Responsabilidad\n(0 = Nada, 5 = Totalmente)"
  ) +
#   cambiando labels a porcentaje y valor absoluto
  scale_x_continuous(
    labels = \(x) percent(abs(x))
  ) +
  labs(
    title = "Responsabilidad percibida sobre el Bienestar: Social vs Tecnológico",
    x = "Proporción de respuestas",
    y = NULL
  ) +
  geom_text(
    aes(
      group = respuesta, # fuerza al texto a agruparse igual que el color
#       etiquetas en porcentaje absoluto, sólo cuando la barra tiene suficiente tamaño
      label = if_else(abs(pct_divergente) > 0.04, percent(abs(pct_divergente), accuracy = 1), ""),
      color = if_else(respuesta %in% c("0", "5"), "white", "black")
    ),
    position = position_stack(vjust = 0.5),
    size = 3,
    show.legend = FALSE
  ) +
scale_color_identity() + # Le dice a ggplot2 que interprete "white" y "black" como colores reales, no como variables
  theme_minimal() +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = 10),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("gráficas/old/z_comparativa_resp.png", plot = p_resp_divergente, width = 10, height = 6, dpi = 300)
})

# ==== DONUT CHART PERFILES ====
local({
  # Extraemos los 4 colores de mako usando el paquete scales (ya cargado)
  colores_mako <- scales::viridis_pal(option = "mako", begin = 0.3, end = 0.8)(4)
  
datos_donut <- tabla_perfiles |> 
  mutate(
    # Creamos una etiqueta limpia con el porcentaje
    etiqueta = paste0(round(porcentaje, 1), "%")
  )

datos_donut |> 
  ggplot(aes(x = 2, y = n, fill = perfil_voluntariado)) +
  # geom_col apila las categorías; el borde blanco separa los sectores
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = if_else(perfil_voluntariado == "no_clasificado", "", etiqueta)),
    position = position_stack(vjust = 0.4),
    color = "#fdfdfd",
    size = 4,
    fontface = "bold"
  ) +
  # Generamos el hueco del donut ampliando el límite inferior del eje X
  xlim(0.5, 2.5) +
  # Añadimos el número total en el centro del hueco
  annotate(
    "text", 
    x = 0.5, 
    y = 0, 
    label = "1301\nPersonas", 
    size = 6, 
    fontface = "bold", 
    color = "#333333"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 12),
  ) +
  labs(
    title = "Distribución de Perfiles de Voluntariado",
    fill = "Categoría"
  ) +
  scale_fill_viridis_d(
    option = "mako", 
    begin = 0.3, 
    end = 0.8,
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntariado Actual (Otros)",
      "voluntario_pasado" = "Voluntariado Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario",
      "no_clasificado"    = "Sin datos"
    )
  ) +
  scale_fill_manual(
    values = c(
      "cibervoluntarios"    = colores_mako[4],
      "voluntario_actual" = colores_mako[3],
      "voluntario_pasado" = colores_mako[2],
      "no_voluntario"     = colores_mako[1],
      "no_clasificado"    = "#b2babb"
    ),
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntariado Actual (Otros)",
      "voluntario_pasado" = "Voluntariado Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario",
      "no_clasificado"    = "Sin datos"
    )
  )

ggsave(
  filename = "gráficas/donut_perfiles.png",
  width = 8, 
  height = 6, 
  dpi = 300
)
})

# ==== DOTPLOT RESP Y RESPTEC X PERFIL VOL ====
local({
# 1. Preparación de datos
local({
df_dumbbell <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado"
  )) |> 
  
  select(
    perfil_voluntariado, 
    weight,
    starts_with("resp_"), 
    starts_with("resptec_")
  ) |> 
  
  group_by(perfil_voluntariado) |> 
  summarise(
    across(
      -weight, 
      \(x) weighted.mean(x, w = weight, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  
  # 4. Transformación Tidy: convertimos a formato largo inteligente.
  # El patrón regex separa el prefijo ("resp" o "resptec") del actor ("estado", etc.)
  # Crea una columna 'actor' y dos columnas numéricas: 'resp' y 'resptec'
  pivot_longer(
    cols = -perfil_voluntariado,
    names_to = c(".value", "actor"),
    names_pattern = "^(resp|resptec)_(.*)$"
  ) |> 
  
  # 5. Limpieza de literales para que el gráfico quede profesional
  mutate(
    actor = case_when(
      actor == "estado" ~ "Estado",
      actor == "empresa" ~ "Empresas",
      actor == "tercer" ~ "Tercer Sector",
      actor == "ciudadania" ~ "Ciudadanía",
      actor == "educativas" ~ "Ins. Educativas",
      TRUE ~ str_to_title(actor)
    ),
    # Convertimos a factor para fijar el orden de aparición en la futura gráfica
    actor = fct_relevel(
      actor, 
      "Estado", "Ins. Educativas", "Ciudadanía", "Empresas", "Tercer Sector"
    ),
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
    )
  )
  
# 2. Visualización: Dot Plot (Solo Responsabilidad General)

p_dot_general <- df_dumbbell |> 
  ggplot(aes(x = actor, group = perfil_voluntariado)) +
  
  # Único punto: Responsabilidad General a COLOR
  geom_point(
    aes(y = resp, fill = perfil_voluntariado),
    shape = 21, # 21 = Círculo con relleno y borde
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white", # Borde blanco sutil
    stroke = 0.5
  ) +
  
  # Escala de colores para los perfiles
  scale_fill_viridis_d(
    option = "mako", begin = 0, end = 0.93,
    name = "Perfil de Voluntariado",
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  # Ajuste de Leyenda (solo necesitamos la de colores/perfiles)
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
  ) +
  
  # Textos y Formato adaptados
  labs(
    title = "Percepción de Responsabilidad sobre el Bienestar por Actor y Perfil",
    subtitle = "¿En qué medida se consideran estos actores responsables de garantizar el bienestar social? ",
    x = NULL,
    y = "Puntuación Media Ponderada (0-5)"
  ) +
  
  scale_y_continuous(breaks = 2:5) +
  coord_cartesian(ylim = c(2.5, 5)) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", size = 11, color = "#34495e"),
    legend.position = "right",
    legend.box = "vertical"
  )

ggsave(
  filename = "gráficas/old/z_dotplot_responsabilidad_general.png", 
  plot = p_dot_general, 
  width = 10, 
  height = 6, 
  dpi = 300
)

# 2. Visualización: Dumbbell Plot (Énfasis Tecnológico)

p_dumbbell <- df_dumbbell |> 
  ggplot(aes(x = actor, group = perfil_voluntariado)) +
  
  # 1. Mancuerna: Línea conectora en GRIS FIJO (fuera del aes)
  geom_linerange(
    aes(ymin = resp, ymax = resptec), 
    color = "gray85", # Gris estático
    position = position_dodge(width = 0.7),
    linewidth = 1.2,
    alpha = 0.5
  ) +
  
  # 2. Punto General: Círculo en GRIS FIJO (fuera del aes)
  geom_point(
    aes(y = resp, shape = "General"), 
    fill = "gray85", # Gris estático
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white",
    stroke = 0.5
  ) +
  
  # 3. Punto Tecnológico: Cuadrado a COLOR (dentro del aes)
  geom_point(
    aes(y = resptec, fill = perfil_voluntariado, shape = "Tecnológico"),
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white",
    stroke = 0.5
  ) +
  
  # 4. Escalas
  scale_fill_viridis_d(
    option = "mako", begin = 0, end = 0.93,
    name = "Perfil de Voluntariado",
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  scale_shape_manual(
    name = "Tipo de Bienestar",
    values = c("General" = 21, "Tecnológico" = 24) 
  ) +
  
  # 5. Ajuste de Leyendas para reflejar el nuevo diseño
  guides(
    # La leyenda de perfiles ahora usa cuadrados (22) porque el color solo aplica a ellos
    fill = guide_legend(override.aes = list(shape = 24, color = "white", size = 4)),
    # La leyenda de formas muestra el círculo gris y el cuadrado oscuro
    shape = guide_legend(override.aes = list(fill = c("gray85", "#34495e"), color = "white", size = 4))
  ) +
  
  # 6. Textos y Formato
  labs(
    title = "Percepción de Responsabilidad sobre el Bienestar Tecnológico",
    subtitle = "¿Cómo difiere la asignación de responsabilidad entre ambos ámbitos?",
    x = NULL,
    y = "Puntuación Media Ponderada (0-5)"
  ) +
  
  scale_y_continuous(breaks = 2:5) +
  coord_cartesian(ylim = c(2.5, 5)) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", size = 11, color = "#34495e"),
    legend.position = "right",
    legend.box = "vertical"
  )

print(p_dumbbell)

ggsave(
  filename = "gráficas/old/z_dumbbell_responsabilidad_gris.png", 
  plot = p_dumbbell, 
  width = 10, 
  height = 6, 
  dpi = 300
)
})
})

# ====
# ==== HEATMAP RESP X PERFIL VOL  ====
local({
# 1. Preparación de Datos: Heatmap de Responsabilidad (resp_)

df_heatmap_resp <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  
  # Seleccionamos las columnas relevantes
  select(perfil_voluntariado, weight, starts_with("resp_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("resp_"),
    names_to = "actor",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de nombres de los actores (quitando el prefijo "resp_")
  mutate(
    actor = str_replace(actor, "resp_", ""),
    actor = case_when(
      actor == "estado" ~ "Estado",
      actor == "empresa" ~ "Empresas",
      actor == "tercer" ~ "Tercer Sector",
      actor == "ciudadania" ~ "Ciudadanía",
      actor == "educativas" ~ "Ins. Educativas",
      TRUE ~ str_to_title(actor)
    ),
    
    # Fijamos el orden de los ejes
    actor = fct_relevel(
      actor, 
      "Estado", "Ins. Educativas", "Ciudadanía", "Empresas", "Tercer Sector"
    ),
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario" 
    )
  ) |> 
  
  # Calculamos la media ponderada para cada celda
  group_by(perfil_voluntariado, actor) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Visualización: Mapa de Calor (Heatmap)

p_heatmap_resp <- df_heatmap_resp |> 
  # fct_rev en Y para que el Voluntariado Tecnológico quede arriba
  ggplot(aes(x = actor, y = fct_rev(perfil_voluntariado), fill = media_ponderada)) +
  
  # Dibujamos las baldosas (tiles) con un pequeño borde blanco para separarlas
  geom_tile(color = "white", linewidth = 1) +
  
  # Añadimos el número exacto dentro de la celda (redondeado a 2 decimales)
  geom_text(
    aes(label = round(media_ponderada, 2)), 
    color = "white", 
    fontface = "bold", 
    size = 4.5
  ) +
  
  # Paleta Mako CONTINUA para rellenar las celdas
  scale_fill_viridis_c(
    option = "mako", 
    begin = 0.2, end = 0.9, # Cortamos los extremos para que el texto blanco siempre se lea bien
    name = "Puntuación\nMedia"
  ) +
  
  # Formateamos las etiquetas del eje Y al vuelo
  scale_y_discrete(
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  labs(
    title = "Percepción de Responsabilidad Social por Actor y Perfil",
    subtitle = "Puntuación media ponderada en escala de 0 a 5",
    x = "Actor",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    # En los heatmaps solemos quitar todas las líneas de la cuadrícula
    panel.grid = element_blank(),
    
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e"),
    axis.text.y = element_text(face = "bold", size = 10, color = "#34495e"),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10)
  )

print(p_heatmap_resp)

ggsave(
  filename = "gráficas/heatmap_responsabilidad.png", 
  plot = p_heatmap_resp, 
  width = 10, 
  height = 6, 
  dpi = 300
)

})

# ==== HEATMAP RESPTEC X PERFIL VOL ====
# 1. Preparación de Datos: Heatmap de Responsabilidad Tecnológica (resptec_)

df_heatmap_resptec <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  
  # Seleccionamos las columnas relevantes usando el prefijo correcto
  select(perfil_voluntariado, weight, starts_with("resptec_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("resptec_"),
    names_to = "actor",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de nombres de los actores (quitando el prefijo "resptec_")
  mutate(
    actor = str_replace(actor, "resptec_", ""),
    actor = case_when(
      actor == "estado" ~ "Estado",
      actor == "empresa" ~ "Empresas",
      actor == "tercer" ~ "Tercer Sector",
      actor == "ciudadania" ~ "Ciudadanía",
      actor == "educativas" ~ "Ins. Educativas",
      TRUE ~ str_to_title(actor)
    ),
    
    # Fijamos el orden de los ejes (manteniendo tu mismo orden)
    actor = fct_relevel(
      actor, 
      "Estado", "Ins. Educativas", "Ciudadanía", "Empresas", "Tercer Sector"
    ),
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario" 
    )
  ) |> 
  
  # Calculamos la media ponderada para cada celda
  group_by(perfil_voluntariado, actor) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Visualización: Mapa de Calor (Heatmap)

p_heatmap_resptec <- df_heatmap_resptec |> 
  # fct_rev en Y para que el Voluntariado Tecnológico quede arriba
  ggplot(aes(x = actor, y = fct_rev(perfil_voluntariado), fill = media_ponderada)) +
  
  # Dibujamos las baldosas (tiles) con un pequeño borde blanco para separarlas
  geom_tile(color = "white", linewidth = 1) +
  
  # Añadimos el número exacto dentro de la celda (redondeado a 2 decimales)
  geom_text(
    aes(label = round(media_ponderada, 2)), 
    color = "white", 
    fontface = "bold", 
    size = 4.5
  ) +
  
  # Paleta Mako CONTINUA para rellenar las celdas
  scale_fill_viridis_c(
    option = "mako", 
    begin = 0.2, end = 0.9, # Cortamos los extremos para que el texto blanco siempre se lea bien
    name = "Puntuación\nMedia"
  ) +
  
  # Formateamos las etiquetas del eje Y al vuelo
  scale_y_discrete(
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  labs(
    title = "Percepción de Responsabilidad en Bienestar Tecnológico por Actor y Perfil",
    subtitle = "Puntuación media ponderada en escala de 0 a 5",
    x = "Actor",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    # En los heatmaps solemos quitar todas las líneas de la cuadrícula
    panel.grid = element_blank(),
    
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e"),
    axis.text.y = element_text(face = "bold", size = 10, color = "#34495e"),
    
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10)
  )

print(p_heatmap_resptec)

ggsave(
  filename = "gráficas/heatmap_responsabilidad_tec.png", 
  plot = p_heatmap_resptec, 
  width = 10, 
  height = 6, 
  dpi = 300
)
# ==== HEATMAP RESP vs RESPTEC X PERFIL VOL  ====
# 0. Ejecutamos el test pareado para tener los datos de significancia
tests_pareados <- test_pareado_generico(df, prefijo1 = "resp_", prefijo2 = "resptec_", var_grupo = "perfil_voluntariado")

# 1. Preparación de Datos: Brecha de Responsabilidad (General vs Tecnológica)
df_diff_resp <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  
  select(
    perfil_voluntariado, 
    weight,
    starts_with("resp_"), 
    starts_with("resptec_")
  ) |> 
  
  group_by(perfil_voluntariado) |> 
  summarise(
    across(
      -weight, 
      \(x) weighted.mean(x, w = weight, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  
  pivot_longer(
    cols = -perfil_voluntariado,
    names_to = c(".value", "actor"),
    names_pattern = "^(resp|resptec)_(.*)$"
  ) |> 
  
  left_join(tests_pareados, by = c("perfil_voluntariado" = "grupo", "actor" = "item")) |> 
  
  mutate(
    diferencia = resp - resptec,
    
    diferencia_plot = if_else(significativo == "ns", NA_real_, diferencia),
    
    # ⬇️ ABSOLUTO PARA MOSTRAR TODO EN POSITIVO
    label_texto = if_else(significativo == "ns", "", sprintf("%.2f", abs(diferencia))),
    
    actor = case_when(
      actor == "estado" ~ "Estado",
      actor == "empresa" ~ "Empresas",
      actor == "tercer" ~ "Tercer Sector",
      actor == "ciudadania" ~ "Ciudadanía",
      actor == "educativas" ~ "Ins. Educativas",
      TRUE ~ str_to_title(actor)
    ),
    actor = fct_relevel(
      actor, 
      "Estado", "Ins. Educativas", "Ciudadanía", "Empresas", "Tercer Sector"
    ),
    
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario Actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario Pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c(
      "Cibervoluntarios", "Voluntario Actual", "Voluntario Pasado", "Nunca ha sido voluntario"
    ))
  )

# 2. Visualización: Heatmap Divergente (Brecha de Responsabilidad)
p_heatmap_diff_resp <- df_diff_resp |> 
  ggplot(aes(x = actor, y = fct_rev(perfil_label), fill = diferencia_plot)) + 
  
  geom_tile(color = "white", linewidth = 1) +
  
  geom_text(
    aes(label = label_texto), 
    color = "gray10", 
    fontface = "bold", 
    size = 4.5
  ) +
  
  scale_fill_gradient2(
    low = "#ed9366",       # Morado (Tecnológico)
    mid = "#f5f5f5",       # Gris claro/neutro
    high = "#22a884",      # Verde Mako (Social)
    midpoint = 0,          
    na.value = "gray90",   
    
    # ⬇️ FORZAMOS LÍMITES SIMÉTRICOS Y ETIQUETAS A LOS EXTREMOS
    limits = function(x) {
      max_val <- max(abs(x), na.rm = TRUE)
      c(-max_val, max_val)
    },
    breaks = function(x) {
      max_val <- max(abs(x), na.rm = TRUE)
      c(-max_val, 0, max_val)
    },
    labels = c("← Más tecnológico", "Equilibrado", "Más social →"),
    
    guide = guide_colorbar(
      title = "Brecha de Responsabilidad",
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 18,        # Alargamos la barra para que se lea bien
      barheight = 1
    )
  ) +
  
  labs(
    title = "Responsabilidad sobre el Bienestar: Social vs Tecnológica",
    subtitle = "Diferencias no significativas (p > 0.05) representadas en gris vacío.",
    x = "Actor evaluado",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.y = element_text(face = "bold", size = 10, color = "#34495e"),
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e"),
    
    legend.position = "bottom", # ⬅️ LEYENDA ABAJO
    legend.title = element_text(face = "bold", size = 11)
  )

print(p_heatmap_diff_resp)

ggsave(
  filename = "gráficas/heatmap_responsabilidad_brecha.png", 
  plot = p_heatmap_diff_resp, 
  width = 10, 
  height = 7, 
  dpi = 300
)

# ====
# ==== DOTPLOT AFI X PERFIL VOL SIGNI ====
# 0. Limpiamos los nombres del test para que coincidan con el gráfico
local({
tests_clean <- tests_afi_perfil |> 
  mutate(
    afirmacion = case_when(
      variable == "afi_quedarse" ~ "Ha venido para quedarse",
      variable == "afi_gente_atras" ~ "Está dejando gente atrás",
      variable == "afi_juventud_adiccion" ~ "La juventud tiene adicción",
      variable == "afi_oportunidades" ~ "Crea nuevas oportunidades",
      variable == "afi_demasiado_pantallas" ~ "Demasiado tiempo en pantallas",
      variable == "afi_reducir_brechas" ~ "Ayuda a reducir brechas",
      TRUE ~ variable
    )
  ) |> 
  select(afirmacion, perfil, significativo)


# 1. Preparación de Datos: Join primero, factores después
df_afi_dot <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  select(perfil_voluntariado, weight, starts_with("afi_")) |> 
  
  pivot_longer(cols = starts_with("afi_"), names_to = "afirmacion", values_to = "puntuacion") |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de literales
  mutate(
    afirmacion = case_when(
      afirmacion == "afi_quedarse" ~ "Ha venido para quedarse",
      afirmacion == "afi_gente_atras" ~ "Está dejando gente atrás",
      afirmacion == "afi_juventud_adiccion" ~ "La juventud tiene adicción",
      afirmacion == "afi_oportunidades" ~ "Crea nuevas oportunidades",
      afirmacion == "afi_demasiado_pantallas" ~ "Demasiado tiempo en pantallas",
      afirmacion == "afi_reducir_brechas" ~ "Ayuda a reducir brechas",
      TRUE ~ afirmacion 
    )
  ) |> 
  
  # Calculamos la media GLOBAL 
  group_by(afirmacion) |> 
  mutate(media_global = weighted.mean(puntuacion, w = weight, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Calculamos la media por perfil (Añadimos media_global al grupo para no perderla en el summarise)
  group_by(perfil_voluntariado, afirmacion, media_global) |> 
  summarise(
    puntuacion_media = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # ⬇️ 1º HACEMOS EL JOIN Y CREAMOS EL COLOR ⬇️
  left_join(tests_clean, by = c("afirmacion", "perfil_voluntariado" = "perfil")) |> 
  mutate(
    fill_punto = case_when(
      perfil_voluntariado == "no_voluntario" ~ "no_voluntario", 
      significativo == "ns" ~ "no_significativo",               
      TRUE ~ as.character(perfil_voluntariado)                  
    )
  ) |> 
  
  # ⬇️ 2º ORDENAMOS LOS FACTORES AL FINAL DEL TODO ⬇️
  mutate(
    afirmacion = fct_reorder(afirmacion, media_global, .desc = TRUE),
    
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
    ),
    
    # También forzamos el orden de fill_punto para asegurar que el position_dodge los ordene visualmente bien
    fill_punto = fct_relevel(
      fill_punto, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios", "no_significativo"
    )
  )


# Extraemos los 4 colores de Mako
colores_mako <- scales::viridis_pal(option = "mako", begin = 0, end = 0.93)(4)


# 2. Visualización: Dot Plot 
p_dot_afi <- df_afi_dot |> 
  ggplot(aes(x = afirmacion, group = perfil_voluntariado)) +
  
  geom_point(
    aes(y = puntuacion_media, fill = fill_punto),
    shape = 21, 
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white", 
    stroke = 0.5
  ) +
  
  scale_fill_manual(
    name = "Perfil de Voluntariado",
    values = c(
      "no_voluntario"     = colores_mako[1],
      "voluntario_pasado" = colores_mako[2],
      "voluntario_actual" = colores_mako[3],
      "cibervoluntarios"    = colores_mako[4],
      "no_significativo"  = "#c0c0c0" # Gris
    ),
    breaks = c("cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"),
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
  ) +
  
  labs(
    title = "Actitudes frente a la tecnología por Perfil de Voluntariado",
    subtitle = "Puntuación media ponderada de grado de acuerdo (puntos grises no difieren significativamente de la población general)",
    x = NULL,
    y = "Puntuación Media Ponderada"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", angle = 45, hjust = 1),
    
    legend.position = "right",
    legend.box = "vertical"
  )

# Mostramos y exportamos
print(p_dot_afi)

ggsave(
  filename = "gráficas/dotplot_afi_perfil_signi.png", 
  plot = p_dot_afi, 
  width = 11, 
  height = 6, 
  dpi = 300
)
})
# ==== DOTPLOT BIENESTAR X PERFIL VOL SIGNI  ====
# 0. Limpiamos los nombres del test para que coincidan con el gráfico
tests_clean <- tests_bienestar_perfil |> 
  mutate(
    dimension = case_when(
      variable == "bienestar_satisfaccion" ~ "Satisfacción general",
      variable == "bienestar_integridad"   ~ "Integridad",
      variable == "bienestar_desarrollo"   ~ "Desarrollo personal",
      variable == "bienestar_libertad"     ~ "Libertad / Autonomía",
      variable == "bienestar_necesidades"  ~ "Necesidades cubiertas",
      variable == "bienestar_pertenencia"  ~ "Sentido de pertenencia",
      variable == "bienestar_agencia"      ~ "Agencia / Control",
      TRUE ~ variable
    )
  ) |> 
  select(dimension, perfil, significativo)


# 1. Preparación de Datos: Join primero, factores después
df_bienestar_dot <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  select(perfil_voluntariado, weight, starts_with("bienestar_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("bienestar_"),
    names_to = "dimension",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de literales
  mutate(
    dimension = case_when(
      dimension == "bienestar_satisfaccion" ~ "Satisfacción general",
      dimension == "bienestar_integridad"   ~ "Integridad",
      dimension == "bienestar_desarrollo"   ~ "Desarrollo personal",
      dimension == "bienestar_libertad"     ~ "Libertad / Autonomía",
      dimension == "bienestar_necesidades"  ~ "Necesidades cubiertas",
      dimension == "bienestar_pertenencia"  ~ "Sentido de pertenencia",
      dimension == "bienestar_agencia"      ~ "Agencia / Control",
      TRUE ~ dimension
    )
  ) |> 
  
  # Calculamos la media GLOBAL 
  group_by(dimension) |> 
  mutate(media_global = weighted.mean(puntuacion, w = weight, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Calculamos la media por perfil (Añadimos media_global al grupo para conservarla)
  group_by(perfil_voluntariado, dimension, media_global) |> 
  summarise(
    puntuacion_media = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # ⬇️ 1º HACEMOS EL JOIN Y CREAMOS EL COLOR ⬇️
  left_join(tests_clean, by = c("dimension", "perfil_voluntariado" = "perfil")) |> 
  mutate(
    fill_punto = case_when(
      perfil_voluntariado == "no_voluntario" ~ "no_voluntario", 
      significativo == "ns" ~ "no_significativo",               
      TRUE ~ as.character(perfil_voluntariado)                  
    )
  ) |> 
  
  # ⬇️ 2º ORDENAMOS LOS FACTORES AL FINAL DEL TODO ⬇️
  mutate(
    dimension = fct_reorder(dimension, media_global, .desc = TRUE),
    
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
    ),
    
    # Forzamos el orden de fill_punto para que position_dodge dibuje los puntos ordenados
    fill_punto = fct_relevel(
      fill_punto, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios", "no_significativo"
    )
  )


# Extraemos los 4 colores de Mako
colores_mako <- scales::viridis_pal(option = "mako", begin = 0, end = 0.93)(4)


# 2. Visualización: Dot Plot (Bienestar Ordenado con %)
p_dot_bienestar <- df_bienestar_dot |> 
  ggplot(aes(x = dimension, group = perfil_voluntariado)) +
  
  geom_point(
    # AHORA USAMOS 'fill_punto'
    aes(y = puntuacion_media, fill = fill_punto),
    shape = 21, 
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white", 
    stroke = 0.5
  ) +
  
  # ESCALA MANUAL (Reemplaza a scale_fill_viridis_d)
  scale_fill_manual(
    name = "Perfil de Voluntariado",
    values = c(
      "no_voluntario"     = colores_mako[1],
      "voluntario_pasado" = colores_mako[2],
      "voluntario_actual" = colores_mako[3],
      "cibervoluntarios"    = colores_mako[4],
      "no_significativo"  = "#c0c0c0" # Gris neutro
    ),
    # Escondemos el gris de la leyenda
    breaks = c("cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"),
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  # Formateamos el eje Y para porcentajes
  scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
  
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
  ) +
  
  labs(
    title = "Dimensiones de Bienestar por Perfil de Voluntariado",
    subtitle = "Proporción que cumple cada dimensión (puntos grises no difieren significativamente de la población general)",
    x = NULL,
    y = "% de acuerdo"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    # Textos del eje X inclinados
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", angle = 45, hjust = 1),
    
    legend.position = "right",
    legend.box = "vertical"
  )

# Mostramos y exportamos
print(p_dot_bienestar)

ggsave(
  filename = "gráficas/dotplot_bienestar_perfil_signi.png", # Añadido el sufijo para no pisar el anterior
  plot = p_dot_bienestar, 
  width = 11, 
  height = 6, 
  dpi = 300
)

# ==== DOTPLOT BIENESTARTEC X PERFIL VOL  ====
local({
  # 0. Limpiamos los nombres del test para que coincidan con el gráfico
  tests_clean_tec <- tests_bienestartec_perfil |> 
    mutate(
      dimension_tec = case_when(
        variable == "bienestartec_competencias" ~ "Autosuficiencia y competencias",
        variable == "bienestartec_al_dia"       ~ "Acceso y actualización tecnológica",
        variable == "bienestartec_no_adicto"    ~ "Uso saludable (sin adicción)",
        variable == "bienestartec_conectado"    ~ "Integración y conexión digital",
        variable == "bienestartec_critico"      ~ "Pensamiento crítico digital",
        TRUE ~ variable
      )
    ) |> 
    select(dimension_tec, perfil, significativo)
  
  
  # 1. Preparación de Datos: Join primero, factores después
  df_bienestartec_dot <- df |> 
    filter(
      !is.na(perfil_voluntariado),
      !perfil_voluntariado %in% c("no_clasificado")
    ) |> 
    select(perfil_voluntariado, weight, starts_with("bienestartec_")) |> 
    
    # Pivotamos a formato largo
    pivot_longer(
      cols = starts_with("bienestartec_"),
      names_to = "dimension_tec",
      values_to = "puntuacion"
    ) |> 
    filter(!is.na(puntuacion)) |> 
    
    # Limpieza de literales
    mutate(
      dimension_tec = case_when(
        dimension_tec == "bienestartec_competencias" ~ "Autosuficiencia y competencias",
        dimension_tec == "bienestartec_al_dia"       ~ "Acceso y actualización tecnológica",
        dimension_tec == "bienestartec_no_adicto"    ~ "Uso saludable (sin adicción)",
        dimension_tec == "bienestartec_conectado"    ~ "Integración y conexión digital",
        dimension_tec == "bienestartec_critico"      ~ "Pensamiento crítico digital",
        TRUE ~ dimension_tec
      )
    ) |> 
    
    # Calculamos la media GLOBAL para ordenar de mayor a menor
    group_by(dimension_tec) |> 
    mutate(media_global = weighted.mean(puntuacion, w = weight, na.rm = TRUE)) |> 
    ungroup() |> 
    
    # Calculamos el porcentaje específico por perfil (añadiendo media_global para no perderla)
    group_by(perfil_voluntariado, dimension_tec, media_global) |> 
    summarise(
      puntuacion_media = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    
    # ⬇️ 1º HACEMOS EL JOIN Y CREAMOS EL COLOR ⬇️
    left_join(tests_clean_tec, by = c("dimension_tec", "perfil_voluntariado" = "perfil")) |> 
    mutate(
      fill_punto = case_when(
        perfil_voluntariado == "no_voluntario" ~ "no_voluntario", 
        significativo == "ns" ~ "no_significativo",               
        TRUE ~ as.character(perfil_voluntariado)                  
      )
    ) |> 
    
    # ⬇️ 2º ORDENAMOS LOS FACTORES AL FINAL DEL TODO ⬇️
    mutate(
      dimension_tec = fct_reorder(dimension_tec, media_global, .desc = TRUE),
      
      perfil_voluntariado = fct_relevel(
        perfil_voluntariado, 
        "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
      ),
      
      # Forzamos el orden de fill_punto para asegurar que position_dodge dibuje en el orden correcto
      fill_punto = fct_relevel(
        fill_punto, 
        "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios", "no_significativo"
      )
    )
  
  
  # Extraemos los 4 colores de Mako
  colores_mako <- scales::viridis_pal(option = "mako", begin = 0, end = 0.93)(4)
  
  
  # 2. Visualización: Dot Plot (Bienestar Tecnológico Ordenado con %)
  p_dot_bienestartec <- df_bienestartec_dot |> 
    ggplot(aes(x = dimension_tec, group = perfil_voluntariado)) +
    
    geom_point(
      # AHORA USAMOS 'fill_punto'
      aes(y = puntuacion_media, fill = fill_punto),
      shape = 21, 
      position = position_dodge(width = 0.7),
      size = 3.5, 
      color = "white", 
      stroke = 0.5
    ) +
    
    # ESCALA MANUAL
    scale_fill_manual(
      name = "Perfil de Voluntariado",
      values = c(
        "no_voluntario"     = colores_mako[1],
        "voluntario_pasado" = colores_mako[2],
        "voluntario_actual" = colores_mako[3],
        "cibervoluntarios"    = colores_mako[4],
        "no_significativo"  = "#c0c0c0" # Gris neutro para no significativos
      ),
      # Ocultamos el gris de la leyenda y mantenemos el orden original
      breaks = c("cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"),
      labels = c(
        "cibervoluntarios"    = "Cibervoluntarios",
        "voluntario_actual" = "Voluntario Actual",
        "voluntario_pasado" = "Voluntario Pasado",
        "no_voluntario"     = "Nunca ha sido voluntario"
      )
    ) +
    
    # Formateamos el eje Y para que muestre porcentajes reales
    scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
    
    guides(
      fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
    ) +
    
    labs(
      title = "Dimensiones de Bienestar Tecnológico por Perfil de Voluntariado",
      subtitle = "Proporción que cumple cada dimensión (puntos grises no difieren significativamente de la población general)",
      x = NULL,
      y = "% de acuerdo"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      
      # Textos del eje X inclinados
      axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", angle = 45, hjust = 1),
      
      legend.position = "right",
      legend.box = "vertical"
    )
  
  # Mostramos y exportamos
  print(p_dot_bienestartec)
  
  ggsave(
    filename = "gráficas/dotplot_bienestartec_signi.png", 
    plot = p_dot_bienestartec, 
    width = 11, 
    height = 6, 
    dpi = 300
  )
})

# ==== DOTPLOT RETOS X PERFIL VOL SIGNI ====

# 1. Limpieza de los tests (asignamos etiquetas amigables)
tests_clean <- tests_retos_perfil |> 
  mutate(
    dimension = case_when(
      variable == "vol_retos_compromiso"    ~ "Falta de compromiso",
      variable == "vol_retos_recursos"      ~ "Escasez de recursos",
      variable == "vol_retos_burocracia"    ~ "Exceso de burocracia",
      variable == "vol_retos_independencia" ~ "Falta independencia económica",
      variable == "vol_retos_nuevas_form"   ~ "Nuevas formas de participación",
      variable == "vol_retos_medios_com"    ~ "Lidiar con medios",
      TRUE ~ variable
    )
  ) |> 
  select(dimension, perfil, significativo)


# 2. Preparación de Datos: Join primero, factores después
df_retos_dot <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  select(perfil_voluntariado, weight, starts_with("vol_retos_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("vol_retos_"),
    names_to = "dimension",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de literales (las mismas etiquetas que en el test)
  mutate(
    dimension = case_when(
      dimension == "vol_retos_compromiso"    ~ "Falta de compromiso",
      dimension == "vol_retos_recursos"      ~ "Escasez de recursos",
      dimension == "vol_retos_burocracia"    ~ "Exceso de burocracia",
      dimension == "vol_retos_independencia" ~ "Falta independencia económica",
      dimension == "vol_retos_nuevas_form"   ~ "Nuevas formas de participación",
      dimension == "vol_retos_medios_com"    ~ "Lidiar con medios",
      TRUE ~ dimension
    )
  ) |> 
  
  # Calculamos la media GLOBAL 
  group_by(dimension) |> 
  mutate(media_global = weighted.mean(puntuacion, w = weight, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Calculamos la media por perfil (Añadimos media_global al grupo para conservarla)
  group_by(perfil_voluntariado, dimension, media_global) |> 
  summarise(
    puntuacion_media = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # ⬇️ 1º HACEMOS EL JOIN Y CREAMOS EL COLOR ⬇️
  left_join(tests_clean, by = c("dimension", "perfil_voluntariado" = "perfil")) |> 
  mutate(
    fill_punto = case_when(
      perfil_voluntariado == "no_voluntario" ~ "no_voluntario", 
      significativo == "ns" ~ "no_significativo",               
      TRUE ~ as.character(perfil_voluntariado)                  
    )
  ) |> 
  
  # ⬇️ 2º ORDENAMOS LOS FACTORES AL FINAL DEL TODO ⬇️
  mutate(
    dimension = fct_reorder(dimension, media_global, .desc = TRUE),
    
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
    ),
    
    # Forzamos el orden de fill_punto para que position_dodge dibuje los puntos ordenados
    fill_punto = fct_relevel(
      fill_punto, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios", "no_significativo"
    )
  )


# Extraemos los 4 colores de Mako
colores_mako <- scales::viridis_pal(option = "mako", begin = 0, end = 0.93)(4)


# 3. Visualización: Dot Plot (Retos Ordenados con %)
p_dot_retos <- df_retos_dot |> 
  ggplot(aes(x = dimension, group = perfil_voluntariado)) +
  
  geom_point(
    # USAMOS 'fill_punto'
    aes(y = puntuacion_media, fill = fill_punto),
    shape = 21, 
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white", 
    stroke = 0.5
  ) +
  
  # ESCALA MANUAL
  scale_fill_manual(
    name = "Perfil de Voluntariado",
    values = c(
      "no_voluntario"     = colores_mako[1],
      "voluntario_pasado" = colores_mako[2],
      "voluntario_actual" = colores_mako[3],
      "cibervoluntarios"    = colores_mako[4],
      "no_significativo"  = "#c0c0c0" # Gris neutro
    ),
    # Escondemos el gris de la leyenda
    breaks = c("cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"),
    labels = c(
      "cibervoluntarios"    = "Cibervoluntarios",
      "voluntario_actual" = "Voluntariado Actual (Otros)",
      "voluntario_pasado" = "Voluntariado Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  # Formateamos el eje Y para porcentajes (he puesto redondeo a 1 decimal para que sea más exacto)
  scale_y_continuous(labels = \(x) paste0(round(x * 100, 1), "%")) +
  
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
  ) +
  
  labs(
    title = "Principales retos del voluntariado hoy en día",
    subtitle = "Proporción de selección (puntos grises no difieren significativamente de la población general)",
    x = NULL,
    y = "% de acuerdo"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    # Textos del eje X inclinados
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", angle = 45, hjust = 1),
    
    legend.position = "right",
    legend.box = "vertical"
  )

# Mostramos y exportamos
print(p_dot_retos)

ggsave(
  filename = "gráficas/dotplot_retos_perfil_signi.png", 
  plot = p_dot_retos, 
  width = 11, 
  height = 6, 
  dpi = 300,
  bg = "white"
)
# ==== DOTPLOT RESP X PERFIL VOL SIGNI ====
# 0. Limpiamos los nombres del test para que coincidan con el gráfico
tests_clean <- tests_resp_perfil |> 
  mutate(
    dimension = case_when(
      variable == "resp_estado"     ~ "Estado",
      variable == "resp_empresa"    ~ "Empresas",
      variable == "resp_tercer"     ~ "Tercer Sector",
      variable == "resp_ciudadania" ~ "Ciudadanía",
      variable == "resp_educativas" ~ "Instituciones educativas",
      TRUE ~ variable
    )
  ) |> 
  select(dimension, perfil, significativo)


# 1. Preparación de Datos: Join primero, factores después
df_resp_dot <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  # Seleccionamos explícitamente las variables para evitar que se cuelen las de resp_tec_
  select(perfil_voluntariado, weight, resp_estado, resp_empresa, resp_tercer, resp_ciudadania, resp_educativas) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("resp_"),
    names_to = "dimension",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de literales
  mutate(
    dimension = case_when(
      dimension == "resp_estado"     ~ "Estado",
      dimension == "resp_empresa"    ~ "Empresas",
      dimension == "resp_tercer"     ~ "Tercer Sector",
      dimension == "resp_ciudadania" ~ "Ciudadanía",
      dimension == "resp_educativas" ~ "Instituciones educativas",
      TRUE ~ dimension
    )
  ) |> 
  
  # Calculamos la media GLOBAL 
  group_by(dimension) |> 
  mutate(media_global = weighted.mean(puntuacion, w = weight, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Calculamos la media por perfil
  group_by(perfil_voluntariado, dimension, media_global) |> 
  summarise(
    puntuacion_media = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # ⬇️ 1º HACEMOS EL JOIN Y CREAMOS EL COLOR ⬇️
  left_join(tests_clean, by = c("dimension", "perfil_voluntariado" = "perfil")) |> 
  mutate(
    fill_punto = case_when(
      perfil_voluntariado == "no_voluntario" ~ "no_voluntario", 
      significativo == "ns" ~ "no_significativo",                
      TRUE ~ as.character(perfil_voluntariado)                  
    )
  ) |> 
  
  # ⬇️ 2º ORDENAMOS LOS FACTORES AL FINAL DEL TODO ⬇️
  mutate(
    dimension = fct_reorder(dimension, media_global, .desc = TRUE),
    
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
    ),
    
    # Forzamos el orden de fill_punto para que position_dodge dibuje los puntos ordenados
    fill_punto = fct_relevel(
      fill_punto, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios", "no_significativo"
    )
  )


# Extraemos los 4 colores de Mako
colores_mako <- scales::viridis_pal(option = "mako", begin = 0, end = 0.93)(4)


# 2. Visualización: Dot Plot (Responsabilidad Ordenada)
p_dot_resp <- df_resp_dot |> 
  ggplot(aes(x = dimension, group = perfil_voluntariado)) +
  
  geom_point(
    aes(y = puntuacion_media, fill = fill_punto),
    shape = 21, 
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white", 
    stroke = 0.5
  ) +
  
  scale_fill_manual(
    name = "Perfil de Voluntariado",
    values = c(
      "no_voluntario"     = colores_mako[1],
      "voluntario_pasado" = colores_mako[2],
      "voluntario_actual" = colores_mako[3],
      "cibervoluntarios"  = colores_mako[4],
      "no_significativo"  = "#c0c0c0" # Gris neutro
    ),
    breaks = c("cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"),
    labels = c(
      "cibervoluntarios"  = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  # La escala de estas variables es de 0 a 5 según tu codebook
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
  ) +
  
  labs(
    title = "Responsabilidad de garantizar el bienestar por Perfil de Voluntariado",
    subtitle = "Puntuación media (escala 0-5) (puntos grises no difieren significativamente de 'Nunca ha sido voluntario')",
    x = NULL,
    y = "Nivel de responsabilidad (0-5)"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", angle = 45, hjust = 1),
    
    legend.position = "right",
    legend.box = "vertical"
  )

# Mostramos y exportamos
print(p_dot_resp)

ggsave(
  filename = "gráficas/dotplot_resp_perfil_signi.png",
  plot = p_dot_resp, 
  width = 11, 
  height = 6, 
  dpi = 300
)
# ==== DOTPLOT RESPTEC X PERFIL VOL SIGNI ====
# 0. Limpiamos los nombres del test para que coincidan con el gráfico
tests_clean <- tests_resptec_perfil |> 
  mutate(
    dimension = case_when(
      variable == "resptec_estado"     ~ "Estado",
      variable == "resptec_empresa"    ~ "Empresas",
      variable == "resptec_tercer"     ~ "Tercer Sector",
      variable == "resptec_ciudadania" ~ "Ciudadanía",
      variable == "resptec_educativas" ~ "Instituciones educativas",
      TRUE ~ variable
    )
  ) |> 
  select(dimension, perfil, significativo)


# 1. Preparación de Datos: Join primero, factores después
df_resptec_dot <- df |> 
  filter(
    !is.na(perfil_voluntariado),
    !perfil_voluntariado %in% c("no_clasificado")
  ) |> 
  select(perfil_voluntariado, weight, starts_with("resptec_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("resptec_"),
    names_to = "dimension",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  # Limpieza de literales
  mutate(
    dimension = case_when(
      dimension == "resptec_estado"     ~ "Estado",
      dimension == "resptec_empresa"    ~ "Empresas",
      dimension == "resptec_tercer"     ~ "Tercer Sector",
      dimension == "resptec_ciudadania" ~ "Ciudadanía",
      dimension == "resptec_educativas" ~ "Instituciones educativas",
      TRUE ~ dimension
    )
  ) |> 
  
  # Calculamos la media GLOBAL 
  group_by(dimension) |> 
  mutate(media_global = weighted.mean(puntuacion, w = weight, na.rm = TRUE)) |> 
  ungroup() |> 
  
  # Calculamos la media por perfil
  group_by(perfil_voluntariado, dimension, media_global) |> 
  summarise(
    puntuacion_media = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # ⬇️ 1º HACEMOS EL JOIN Y CREAMOS EL COLOR ⬇️
  left_join(tests_clean, by = c("dimension", "perfil_voluntariado" = "perfil")) |> 
  mutate(
    fill_punto = case_when(
      perfil_voluntariado == "no_voluntario" ~ "no_voluntario", 
      significativo == "ns" ~ "no_significativo",                
      TRUE ~ as.character(perfil_voluntariado)                  
    )
  ) |> 
  
  # ⬇️ 2º ORDENAMOS LOS FACTORES AL FINAL DEL TODO ⬇️
  mutate(
    dimension = fct_reorder(dimension, media_global, .desc = TRUE),
    
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios" 
    ),
    
    # Forzamos el orden de fill_punto
    fill_punto = fct_relevel(
      fill_punto, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios", "no_significativo"
    )
  )


# Extraemos los 4 colores de Mako
colores_mako <- scales::viridis_pal(option = "mako", begin = 0, end = 0.93)(4)


# 2. Visualización: Dot Plot (Responsabilidad Tecnológica Ordenada)
p_dot_resptec <- df_resptec_dot |> 
  ggplot(aes(x = dimension, group = perfil_voluntariado)) +
  
  geom_point(
    aes(y = puntuacion_media, fill = fill_punto),
    shape = 21, 
    position = position_dodge(width = 0.7),
    size = 3.5, 
    color = "white", 
    stroke = 0.5
  ) +
  
  scale_fill_manual(
    name = "Perfil de Voluntariado",
    values = c(
      "no_voluntario"     = colores_mako[1],
      "voluntario_pasado" = colores_mako[2],
      "voluntario_actual" = colores_mako[3],
      "cibervoluntarios"  = colores_mako[4],
      "no_significativo"  = "#c0c0c0" # Gris neutro
    ),
    breaks = c("cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"),
    labels = c(
      "cibervoluntarios"  = "Cibervoluntarios",
      "voluntario_actual" = "Voluntario Actual",
      "voluntario_pasado" = "Voluntario Pasado",
      "no_voluntario"     = "Nunca ha sido voluntario"
    )
  ) +
  
  scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
  
  guides(
    fill = guide_legend(override.aes = list(shape = 21, color = "white", size = 4))
  ) +
  
  labs(
    title = "Responsabilidad de garantizar el bienestar tecnológico por Perfil",
    subtitle = "Puntuación media (escala 0-5) (puntos grises no difieren significativamente de 'Nunca ha sido voluntario')",
    x = NULL,
    y = "Nivel de responsabilidad (0-5)"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", angle = 45, hjust = 1),
    
    legend.position = "right",
    legend.box = "vertical"
  )

# Mostramos y exportamos
print(p_dot_resptec)

ggsave(
  filename = "gráficas/dotplot_resptec_perfil_signi.png",
  plot = p_dot_resptec, 
  width = 11, 
  height = 6, 
  dpi = 300
)
# ==== DONUT CHART TIPO VOLUNTARIADO ====
local({
datos_tipos_vol <- df |> 
  filter(origen == "panel") |> 
  # Incluimos 'weight' en la selección
  select(contains("volu_tipo"), weight) |> 
  # Suma ponderada: multiplicamos cada respuesta (0 o 1) por su peso
  summarise(
    across(-weight, \(x) sum(x * weight, na.rm = TRUE))
  ) |> 
  # Pasamos de múltiples columnas a formato largo (una fila por tipo)
  pivot_longer(
    cols = everything(), 
    names_to = "tipo", 
    values_to = "total_ponderado"
  ) |> 
  mutate(
    # Limpiamos los nombres de las variables para la leyenda
    tipo = str_remove(tipo, "volu_tipo_"),
    tipo = str_replace_all(tipo, "_", " "),
    tipo = str_to_title(tipo),
    
    # Calculamos el porcentaje sobre el total de "menciones" ponderadas
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100,
    etiqueta = paste0(round(porcentaje, 1), "%")
  ) |> 
  # Ordenamos los factores de mayor a menor para un gráfico estructurado
  arrange(desc(total_ponderado)) |> 
  mutate(tipo = fct_inorder(tipo))


# 2. Visualización: Donut Chart

p_donut_tipos <- datos_tipos_vol |> 
  ggplot(aes(x = 2, y = total_ponderado, fill = tipo)) +
  
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  
  # Añadimos etiquetas, pero solo si el trozo es > 2% para evitar solapamientos
  geom_text(
    aes(label = if_else(porcentaje > 2, etiqueta, "")),
    position = position_stack(vjust = 0.5),
    color = "#fdfdfd",
    size = 4,
    fontface = "bold"
  ) +
  
  # Creamos el hueco del donut
  xlim(0.5, 2.5) +
  
  # Usamos la paleta Mako, ampliando un poco el rango para que los 11 colores se distingan
  scale_fill_viridis_d(option = "mako", begin = 0.9, end = 0.1) +
  
  labs(
    title = "Distribución por Tipo de Voluntariado",
    subtitle = "Porcentaje de participación sobre el total de menciones ponderadas",
    fill = "Tipo de Voluntariado"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 10)),
    legend.position = "right",
    legend.text = element_text(size = 12),
  )

ggsave(
  filename = "gráficas/donut_tipos_voluntariado.png", 
  plot = p_donut_tipos, 
  width = 9, 
  height = 6, 
  dpi = 300
)
})

# ==== BARPLOT VOLU FREQ ====
# 1. Preparación de datos
df_freq <- df |> 
  # Filtramos a quienes sí respondieron la pregunta (quitamos los NA)
  filter(!is.na(volu_freq), origen == "panel") |> 
  
  # Convertimos a factor ordenado de mayor a menor frecuencia temporal
  mutate(
    volu_freq = factor(volu_freq, levels = c(
      "Al menos una vez a la semana",
      "Al menos una vez al mes",
      "Al menos una vez al trimestre",
      "Con menos frecuencia"
    ))
  ) |> 
  
  # Calculamos los totales ponderados por grupo
  group_by(volu_freq) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # Calculamos el porcentaje
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  )

# 2. Visualización: Barplot
p_freq <- ggplot(df_freq, aes(x = volu_freq, y = porcentaje, fill = volu_freq)) +
  
  # Dibujamos las barras
  geom_col(width = 0.6, color = "black", linewidth = 0.5) +
  
  # Añadimos el porcentaje encima de cada barra
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.5, # Sube el texto ligeramente por encima de la barra
    size = 4.5,
    fontface = "bold",
    color = "#2c3e50"
  ) +
  
  # Usamos una paleta secuencial de Viridis (o puedes usar tus propios colores)
  scale_fill_viridis_d(option = "mako", begin = 0.8, end = 0.2, guide = "none") +
  
  # Ampliamos un poco el eje Y para que no se corte el texto de la barra más alta
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  labs(
    title = "Frecuencia de dedicación al voluntariado",
    subtitle = "Distribución de las personas que realizan voluntariado actualmente",
    x = NULL,
    y = "Porcentaje"
  ) +
  
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black")
  )

# Mostramos el plot en R
print(p_freq)

# 3. Guardado
ggsave(
  filename = "gráficas/barplot_frecuencia_voluntariado.png", 
  plot = p_freq, 
  width = 10, 
  height = 6, 
  dpi = 300
)
# ==== BARPLOT ANTIGÜEDAD ====
# 1. Preparación de datos
df_antiguedad <- df |> 
  # Filtramos a quienes sí respondieron (quitamos los NA)
  filter(!is.na(volu_antiguedad), origen == "panel") |> 
  
  # Convertimos a factor ordenado de menor a mayor antigüedad
  mutate(
    volu_antiguedad = factor(volu_antiguedad, levels = c(
      "Menos de 12 meses",
      "1-2 años",
      "Menos de 5 años",
      "6-9 años",
      "10 años o más"
    ))
  ) |> 
  
  # Calculamos los totales ponderados por grupo
  group_by(volu_antiguedad) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  
  # Calculamos el porcentaje
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  )

# 2. Visualización: Barplot
p_antiguedad <- ggplot(df_antiguedad, aes(x = volu_antiguedad, y = porcentaje, fill = volu_antiguedad)) +
  
  # Dibujamos las barras
  geom_col(width = 0.6, color = "black", linewidth = 0.5) +
  
  # Añadimos el porcentaje encima de cada barra
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.5, 
    size = 4.5,
    fontface = "bold",
    color = "#2c3e50"
  ) +
  
  # Paleta secuencial (cambiada a 'viridis' para distinguirla del anterior si van juntos)
  scale_fill_viridis_d(option = "viridis", begin = 0.2, end = 0.9, guide = "none") +
  
  # Ampliamos el eje Y para que respire el texto
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15)) 
  ) +
  
  labs(
    title = "Antigüedad en el voluntariado",
    subtitle = "Tiempo que llevan colaborando las personas voluntarias activas",
    x = NULL,
    y = "Porcentaje"
  ) +
  
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    
    plot.title = element_text(face = "bold", size = 16, color = "#2c3e50"),
    plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black")
  )

# Mostramos el plot
print(p_antiguedad)

# 3. Guardado
ggsave(
  filename = "gráficas/barplot_antiguedad_voluntariado.png", 
  plot = p_antiguedad, 
  width = 10, 
  height = 6, 
  dpi = 300
)
# ==== PIRÁMIDES DE EDAD X PERFIL VOL ====
local({
datos_piramides <- df |> 
  # Filtramos NAs en las variables clave
  filter(!is.na(perfil_voluntariado), !is.na(grupo_edad), !is.na(genero), perfil_voluntariado != "no_clasificado") |> 
  
  # Agrupamos por perfil, edad y género para sumar los pesos
  group_by(perfil_voluntariado, grupo_edad, genero) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop" # Rompemos la agrupación interna
  ) |> 
  
  # Volvemos a agrupar solo por perfil para calcular porcentajes relativos a cada bloque
  group_by(perfil_voluntariado) |> 
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100,
    
    # Truco de la pirámide: Hacemos negativos los valores de los hombres
    pct_plot = if_else(genero == "Hombre", -porcentaje, porcentaje)
  ) |> 
  ungroup() |> 
  
  # Formateo visual para la gráfica
  mutate(
    # Reordenamos las facetas para que tengan sentido lógico
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "cibervoluntarios", "voluntario_actual", "voluntario_pasado", "no_voluntario"
    ),
    # Damos nombres amigables a las etiquetas del panel
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntariado Actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntariado Pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario",
      TRUE ~ as.character(perfil_voluntariado)
    )
  )

# 2. Visualización: Panel de Pirámides de Población

p_panel_piramides <- datos_piramides |> 
  ggplot(aes(x = pct_plot, y = grupo_edad, fill = genero)) +
  
  # Barras horizontales
  geom_col(alpha = 0.85, width = 0.8) +
  
  # Facetado: Un panel por cada perfil de voluntariado
  facet_wrap(~perfil_label) +
  
  # Añadimos una línea vertical sutil en el 0 (el centro de la pirámide)
  geom_vline(xintercept = 0, color = "gray50", linewidth = 0.5) +
  
  # Paleta amigable y corporativa
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.7) +
  
  # Formateamos el eje X para que los números siempre sean absolutos (positivos)
  # y añadimos el símbolo %
  scale_x_continuous(
    labels = \(x) paste0(abs(x), "%")
  ) +
  
  labs(
    title = "Estructura Demográfica por Perfil de Voluntariado",
    subtitle = "Pirámide de edad ponderada (% calculado sobre el total de cada perfil)",
    x = "Porcentaje de la población del perfil",
    y = NULL,
    fill = "Género"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    # Configuramos la cuadrícula
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    # Estilo de los encabezados de los paneles (facets)
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    
    # Textos
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y = element_text(size = 9, color = "#34495e"),
    axis.text.x = element_text(size = 8, color = "#7f8c8d"),
    
    # Leyenda en la parte superior para no robar espacio horizontal
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

ggsave(
  filename = "gráficas/piramides_edad_perfiles.png", 
  plot = p_panel_piramides, 
  width = 11, 
  height = 7, 
  dpi = 300
)
})

# ==== VIOLIN EDAD X PERFIL VOL =====
local({
# 1. Preparación de Datos: Marcas de clase para la Edad

df_violines <- df |> 
  # Filtramos NAs en las variables que vamos a usar
  filter(
    !is.na(grupo_edad), 
    !is.na(perfil_voluntariado),
    perfil_voluntariado != "no_clasificado"
  ) |> 
  mutate(
    # Formateamos y ordenamos los perfiles para el eje X
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntariado\nActual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntariado\nPasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido\nvoluntario"
    ),
    perfil_label = factor(perfil_label, levels = c(
      "Nunca ha sido\nvoluntario",
      "Voluntariado\nPasado",
      "Voluntariado\nActual",
      "Cibervoluntarios"
    ))
  )

# 2. Visualización: Violin Plot Ponderado Suavizado

p_violines_edad <- df_violines |> 
  ggplot(aes(x = perfil_label, y = edad_num, fill = perfil_label, weight = weight)) +
  
  # Capa 1: El violín
  geom_violin(
    color = "white",
    trim = TRUE,     # Corta la curva en los límites reales (21 y 70)
    adjust = 2,
    alpha = 0.7
  ) +
  
  # Paleta Mako (ocultamos leyenda porque el eje X ya lo explica)
  scale_fill_viridis_d(
    option = "mako", 
    begin = 0.2, 
    end = 0.8, 
    guide = "none"
  ) +
  
  # Etiquetas
  labs(
    title = "Distribución de Edad por Perfil de Voluntariado",
    subtitle = "Estimación de densidad basada en marcas de clase ponderadas",
    x = NULL,
    y = "Edad (años)"
  ) +
  
  # Estilo visual corporativo
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(face = "bold", size = 10, color = "#34495e", lineheight = 1.2),
    axis.text.y = element_text(size = 10, color = "#34495e")
  )

# Mostramos y exportamos
print(p_violines_edad)

ggsave(
  filename = "gráficas/violin_edad_estimada_perfiles.png", 
  plot = p_violines_edad, 
  width = 10, 
  height = 6, 
  dpi = 300
)
})

# ==== TAMAÑO DE POBLACIÓN X PERFIL VOL ====
local({
# 1. Preparación de los datos (Wrangling)
datos_apilados_pob <- df |> 
  # Filtramos NAs y categorías no deseadas
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado",
    !is.na(tamaño_pob)
  ) |> 
  
  # Forzamos el orden lógico para el eje Y (Perfil)
  mutate(
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntariado Actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntariado Pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c(
      "Nunca ha sido voluntario",
      "Voluntariado Pasado",
      "Voluntariado Actual",
      "Cibervoluntarios"
    ))
  ) |> 
  
  # PASO 1 (MODIFICADO): Agrupamos PRIMERO por Perfil y luego por Tamaño
  group_by(perfil_label, tamaño_pob) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last" # Deja la agrupación por perfil_label activa
  ) |> 
  
  # PASO 2 (MODIFICADO): Calculamos el % de cada tamaño DENTRO de cada perfil
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()


# 2. Visualización: Barras Apiladas 100%
p_apiladas_pob <- datos_apilados_pob |> 
  ggplot(aes(x = porcentaje, y = fct_rev(perfil_label), fill = tamaño_pob)) +
  
  # Barras apiladas con un borde blanco sutil
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  # Textos centrados en cada fragmento (ocultamos los < 3%)
  geom_text(
    aes(label = if_else(porcentaje > 4, paste0(round(porcentaje, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  
  # Paleta Mako para los estratos de población
  scale_fill_viridis_d(
    option = "mako", begin = 0.1, end = 0.9,
    name = "Tamaño del Municipio"
  ) +
  
  # Formateamos el eje X para que muestre de 0 a 100%
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Tamaño de Población según Perfil de Voluntariado",
    subtitle = "Proporción de estratos de población dentro de cada perfil",
    x = "Porcentaje dentro del perfil de voluntariado",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 10, color = "#34495e"),
    axis.text.x = element_text(size = 9, color = "#7f8c8d"),
    
    # Bajamos la leyenda y la organizamos en 2 filas para que respire
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  # Este comando ayuda a que el orden de los colores en la leyenda 
  # coincida con el orden visual de los bloques en las barras
  guides(fill = guide_legend(reverse = TRUE, nrow = 2))

# 3. Guardado
ggsave(
  filename = "gráficas/barras_apiladas_poblacion_corregido.png", 
  plot = p_apiladas_pob, 
  width = 11, 
  height = 6, 
  dpi = 300
)
      
      
})

# ==== SITUACIÓN LABORAL X PERFIL VOL ====
local({
# 1. Preparación de Datos: Composición Laboral (Ordenada por % Global)

datos_apilados_lab <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado",
    !is.na(situacion_lab)
  ) |> 
  
  mutate(
    # Convertimos a texto explícito por seguridad
    situacion_lab = as.character(situacion_lab), 
    
    # Formateo del perfil
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntariado Actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntariado Pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c(
      "Cibervoluntarios",
      "Voluntariado Actual",
      "Voluntariado Pasado",
      "Nunca ha sido voluntario"
    ))
  ) |> 
  
  # --- TRUCO PARA ORDENAR: Calculamos el peso global de cada situación laboral ---
  group_by(situacion_lab) |> 
  mutate(frecuencia_global = sum(weight, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(
    # fct_reorder ordena la situación laboral de mayor a menor frecuencia global
    situacion_lab = fct_reorder(situacion_lab, frecuencia_global, .desc = FALSE)
  ) |> 
  
  # Calculamos la suma por perfil y situación para el gráfico
  group_by(perfil_label, situacion_lab) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) |> 
  
  # Calculamos el %
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()


# 2. Visualización: Barras Apiladas 100% (Laboral, 7 Niveles)

p_apiladas_lab <- datos_apilados_lab |> 
  ggplot(aes(x = porcentaje, y = perfil_label, fill = situacion_lab)) +
  
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(label = if_else(porcentaje > 3, paste0(round(porcentaje, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  
  # Usamos la paleta de colores sin nombrar. 
  scale_fill_manual(
    values = paleta_bienestar,
    name = "Situación Laboral"
  ) +
  
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Situación Laboral según el Perfil de Voluntariado",
    subtitle = "Composición de cada perfil ordenada por frecuencias globales",
    x = "Porcentaje dentro del perfil de voluntariado",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y = element_text(face = "bold", size = 10, color = "#34495e"),
    axis.text.x = element_text(size = 9, color = "#7f8c8d"),
    
    # Leyenda arriba y permitiendo varias filas para que quepan todos los nombres
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9)
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

ggsave(
  filename = "gráficas/barras_apiladas_laboral_perfiles.png", 
  plot = p_apiladas_lab, 
  width = 12, 
  height = 7, 
  dpi = 300
)
})

# ==== SECTOR LABORAL X PERFIL VOL ====
# 1. Preparación de Datos: Composición por Sector Laboral (Ordenada por % Global)
datos_apilados_sector <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado",
    !is.na(sector_lab)
  ) |> 
  
  mutate(
    # Convertimos a texto explícito por seguridad
    sector_lab = as.character(sector_lab), 
    
    # Formateo del perfil
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntariado Actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntariado Pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c(
      "Cibervoluntarios",
      "Voluntariado Actual",
      "Voluntariado Pasado",
      "Nunca ha sido voluntario"
    ))
  ) |> 
  
  # --- TRUCO PARA ORDENAR: Calculamos el peso global de cada sector laboral ---
  group_by(sector_lab) |> 
  mutate(frecuencia_global = sum(weight, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(
    # fct_reorder ordena el sector laboral de mayor a menor frecuencia global
    sector_lab = fct_reorder(sector_lab, frecuencia_global, .desc = FALSE)
  ) |> 
  
  # Calculamos la suma por perfil y sector para el gráfico
  group_by(perfil_label, sector_lab) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) |> 
  
  # Calculamos el %
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()


# 2. Visualización: Barras Apiladas 100% (Sector Laboral)

# Primero, contamos cuántos sectores únicos hay en los datos (11)
num_colores_necesarios <- length(unique(datos_apilados_sector$sector_lab))

p_apiladas_sector <- datos_apilados_sector |> 
  ggplot(aes(x = porcentaje, y = perfil_label, fill = sector_lab)) +
  
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(label = if_else(porcentaje > 3, paste0(round(porcentaje, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  ) +
  
  # ⬇️ EL TRUCO: Expandimos tu paleta original a 11 colores dinámicamente
  scale_fill_manual(
    values = colorRampPalette(paleta_bienestar)(num_colores_necesarios),
    name = "Sector de Actividad"
  ) +
  
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Sector de Actividad según el Perfil de Voluntariado",
    subtitle = "Composición de cada perfil ordenada por frecuencias globales",
    x = "Porcentaje dentro del perfil de voluntariado",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background  = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor   = element_blank(),
    
    plot.title    = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y   = element_text(face = "bold", size = 10, color = "#34495e"),
    axis.text.x   = element_text(size = 9, color = "#7f8c8d"),
    
    # Leyenda
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9)
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 4, byrow = TRUE))

# 3. Guardado
ggsave(
  filename = "gráficas/barras_apiladas_sector_perfiles.png", 
  plot = p_apiladas_sector, 
  width = 11, 
  height = 7, 
  dpi = 300
)

# 3. Guardado (nomenclatura adaptada)
ggsave(
  filename = "gráficas/barras_apiladas_sector_perfiles.png", 
  plot = p_apiladas_sector, 
  width = 11, 
  height = 7, 
  dpi = 300
)
# DOUNT CHART OTRA FORMA ====

# 1. Diccionario limpio para las formas de colaborar
diccionario_colab <- c(
  "colab_donaciones" = "Donaciones (económicas / especie)",
  "colab_informal"   = "Apoyo informal (vecinos, familia)",
  "colab_trabajo"    = "A través del trabajo / empresa",
  "colab_activismo"  = "Activismo / Movimientos sociales",
  "colab_no"         = "No colaboro de ninguna manera"
)

# Extraemos el N unweighted (o sum(weight) si prefieres la estimación poblacional) para el centro
# Aquí uso el conteo de filas reales para la muestra, pero puedes cambiarlo por sum(df_no_voluntarios$weight)
n_personas_centro <- nrow(df_no_voluntarios) 

# 3. Data Wrangling de múltiples respuestas
datos_donut <- df_no_voluntarios |> 
  select(weight, starts_with("colab_")) |> 
  # Rellenamos NAs con 0 por ser dummy
  mutate(across(everything(), ~replace_na(.x, 0))) |> 
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("colab_"), 
    names_to = "tipo_colab", 
    values_to = "marcado"
  ) |> 
  # Nos quedamos solo con las opciones que sí han marcado
  filter(marcado == 1) |> 
  mutate(
    tipo_colab_clean = coalesce(diccionario_colab[tipo_colab], tipo_colab)
  ) |> 
  # Agrupamos y calculamos N y porcentajes
  group_by(tipo_colab_clean) |> 
  summarise(n_respuestas = sum(weight, na.rm = TRUE), .groups = "drop") |> 
  mutate(
    # Porcentaje sobre el total de RESPUESTAS
    porcentaje = (n_respuestas / sum(n_respuestas)) * 100,
    etiqueta = paste0(round(porcentaje, 1), "%")
  ) |> 
  # Ordenamos de mayor a menor para que los trozos del donut queden en orden de tamaño
  arrange(desc(porcentaje)) |> 
  mutate(tipo_colab_clean = factor(tipo_colab_clean, levels = tipo_colab_clean))


# 4. Visualización - Donut Chart
ggplot(datos_donut, aes(x = 2, y = n_respuestas, fill = tipo_colab_clean)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5), # 0.5 centra el texto en cada sector
    color = "#ffffff",
    size = 4.5,
    fontface = "bold"
  ) +
  # El xlim(0.5, 2.5) es lo que convierte el "quesito" (pie) en un donut (hueco en el medio)
  xlim(0.5, 2.5) +
  # Inyectamos dinámicamente el número de personas en el centro
  annotate(
    "text", 
    x = 0.5, 
    y = 0, 
    label = paste0(n_personas_centro, "\nPersonas"), 
    size = 6, 
    fontface = "bold", 
    color = "#333333"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 10))
  ) +
  labs(
    title = "Formas de colaboración alternativas",
    subtitle = "Porcentaje de menciones sobre el total de respuestas dadas por los 'No Voluntarios'",
    fill = "Forma de colaborar:"
  ) +
  # Usamos la paleta mako directamente acotada, que es más seguro que extraer valores a mano
  scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.2, end = 0.9)

# Guardamos el resultado
ggsave(
  filename = "gráficas/donut_colaboraciones.png",
  width = 10, 
  height = 6, 
  dpi = 300,
  bg = "white" # Para asegurarnos de que el fondo transparente no moleste si lo pegas en un PPT
)
# ====
# ==== LINEAS AFI POR EDAD ====
# 1. Preparación de Datos
datos_afi_alt <- df |> 
  select(perfil_voluntariado, edad_num, weight, starts_with("afi_")) |> 
  # Filtramos NAs, no clasificados y excluimos el tramo de 18 a 24 años (valor 21)
  filter(
    !is.na(edad_num), 
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado",
    edad_num != 21 
  ) |> 
  
  pivot_longer(
    cols = starts_with("afi_"),
    names_to = "afirmacion",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  mutate(
    afirmacion = case_when(
      afirmacion == "afi_quedarse" ~ "Ha venido para quedarse",
      afirmacion == "afi_gente_atras" ~ "Está dejando gente atrás",
      afirmacion == "afi_juventud_adiccion" ~ "La juventud tiene adicción",
      afirmacion == "afi_oportunidades" ~ "Crea nuevas oportunidades",
      afirmacion == "afi_demasiado_pantallas" ~ "Demasiado tiempo en pantallas",
      afirmacion == "afi_reducir_brechas" ~ "Ayuda a reducir brechas",
      TRUE ~ afirmacion
    ),
    
    # Forzamos el orden exacto de las facetas (Lectura en Z)
    afirmacion = factor(afirmacion, levels = c(
      "Ha venido para quedarse",       "Demasiado tiempo en pantallas",
      "La juventud tiene adicción",    "Crea nuevas oportunidades",
      "Está dejando gente atrás",      "Ayuda a reducir brechas"
    )),
    
    # Etiquetas y colores del perfil
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
  ) |> 
  
  # Calculamos la media ponderada
  group_by(afirmacion, perfil_label, edad_num) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Visualización: Gráfico Alternativo
p_alt_afi <- datos_afi_alt |> 
  ggplot(aes(x = edad_num, y = media_ponderada, color = perfil_label)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # Al usar ncol = 2, ggplot rellenará la cuadrícula siguiendo el orden exacto del factor
  facet_wrap(~ afirmacion, ncol = 2) +
  
  scale_color_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
  
  # Eje X reconfigurado: Omitimos el punto de ruptura 21 y la etiqueta "18-24"
  scale_x_continuous(
    breaks = c(29.5, 39.5, 49.5, 59.5, 70),
    labels = c("25-34", "35-44", "45-54", "55-64", "65+")
  ) +
  
  labs(
    title = "Evolución de las actitudes frente a la tecnología según la edad",
    subtitle = "Comparativa de los distintos perfiles ante cada afirmación (Grupo 18-24 excluido por baja N)",
    x = "Tramo de Edad",
    y = "Puntuación Media",
    color = "Perfil de Voluntariado" 
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e", face = "bold"),
    axis.text.y = element_text(size = 9, color = "#34495e", face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 9),
    
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
    strip.background = element_rect(fill = "gray90", color = NA),           
    panel.spacing = unit(1.5, "lines") 
  ) +
  guides(color = guide_legend(nrow = 1))

# Mostramos por consola
print(p_alt_afi)

# Exportación del output
ggsave(
  filename = "gráficas/lineas_tendencia_afi_edad_por_perfiles.png", 
  plot = p_alt_afi, 
  width = 11, 
  height = 9, 
  dpi = 300
)

# ==== LINEAS AFI POR TAMAÑO POB ====
# 1. Preparación de Datos: Afirmaciones cruzadas por Perfil y Tamaño de Población
datos_afi_pob_alt <- df |> 
  select(perfil_voluntariado, tamaño_pob, weight, starts_with("afi_")) |> 
 filter(!is.na(tamaño_pob), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado",
         !tamaño_pob %in% c("Menos de 2.000 personas", "De 2.000 a 5.000 personas", "De 5.000 a 10.000 personas")) |> 
  
  mutate(
    # Forzamos el orden INVERTIDO de los factores de población (de mayor a menor)
    tamaño_pob = factor(tamaño_pob, levels = c(
      "Más de 500.000 personas",
      "De 200.000 a 500.000 personas", 
      "De 50.000 a 200.000 personas",
      "De 10.000 a 50.000 personas"
    )),
    
    # Preparamos las etiquetas del perfil (que ahora serán las líneas)
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
  ) |> 
  
  pivot_longer(
    cols = starts_with("afi_"),
    names_to = "afirmacion",
    values_to = "puntuacion"
  ) |> 
  
  filter(!is.na(puntuacion)) |> 
  
  mutate(
    # Nombres limpios
    afirmacion = case_when(
      afirmacion == "afi_quedarse" ~ "Ha venido para quedarse",
      afirmacion == "afi_gente_atras" ~ "Está dejando gente atrás",
      afirmacion == "afi_juventud_adiccion" ~ "La juventud tiene adicción",
      afirmacion == "afi_oportunidades" ~ "Crea nuevas oportunidades",
      afirmacion == "afi_demasiado_pantallas" ~ "Demasiado tiempo en pantallas",
      afirmacion == "afi_reducir_brechas" ~ "Ayuda a reducir brechas",
      TRUE ~ afirmacion 
    ),
    
    # ⬇️ Forzamos el orden exacto de las facetas (Lectura en Z) ⬇️
    afirmacion = factor(afirmacion, levels = c(
      "Ha venido para quedarse",       "Demasiado tiempo en pantallas",
      "La juventud tiene adicción",    "Está dejando gente atrás",
      "Crea nuevas oportunidades",     "Ayuda a reducir brechas"
    ))
  ) |> 
  
  # Calculamos la media (esta vez por afirmacion, perfil y tamaño de población)
  group_by(afirmacion, perfil_label, tamaño_pob) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Visualización: Gráfico Alternativo (Población)
p_alt_afi_pob <- datos_afi_pob_alt |> 
  # ⬇️ AHORA EL COLOR Y EL GRUPO SON EL PERFIL ⬇️
  ggplot(aes(x = tamaño_pob, y = media_ponderada, color = perfil_label, group = perfil_label)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # ⬇️ FACETAS = AFIRMACIONES en cuadrícula 3x2 ⬇️
  facet_wrap(~ afirmacion, ncol = 2) +
  
  # Usamos escala Mako ajustada
  scale_color_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
  
  # Etiquetas resumidas para el eje X
  scale_x_discrete(
    labels = c(
      "Menos de 2.000 personas"       = "< 2.000",
      "De 2.000 a 5.000 personas"     = "2.000 - 5.000",
      "De 5.000 a 10.000 personas"    = "5.000 - 10.000",
      "De 10.000 a 50.000 personas"   = "10.000 - 50.000",
      "De 50.000 a 200.000 personas"  = "50.000 - 200.000",
      "De 200.000 a 500.000 personas" = "200.000 - 500.000",
      "Más de 500.000 personas"       = "+500.000"
    )
  ) +
  
  labs(
    title = "Actitudes frente a la tecnología según el tamaño del municipio",
    subtitle = "Comparativa de los distintos perfiles de voluntariado ante cada afirmación (Puntuación 0-5)",
    x = "Tamaño de Población",
    y = "Puntuación Media",
    color = "Perfil de Voluntariado"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    # Inclinamos textos del eje X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e", face = "bold"),
    axis.text.y = element_text(size = 9, color = "#34495e", face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 9),
    
    # Estilos para los encabezados de las afirmaciones (facets)
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
    strip.background = element_rect(fill = "gray90", color = NA),           
    panel.spacing = unit(1.5, "lines") 
  ) +
  
  # Forzamos la leyenda de los 4 perfiles en una sola línea para no comer espacio vertical
  guides(color = guide_legend(nrow = 1))

# Mostramos y exportamos
print(p_alt_afi_pob)

ggsave(
  filename = "gráficas/lineas_tendencia_afi_poblacion_por_perfiles.png", 
  plot = p_alt_afi_pob, 
  width = 11, 
  height = 10, # Un poco más alto para compensar las etiquetas del eje X a 45 grados
  dpi = 300
)
# ==== LINEAS BIENESTAR POR EDAD ====
# 1. Preparación de Datos
datos_bienestar_alt <- df |> 
  select(perfil_voluntariado, edad_num, weight, starts_with("bienestar_")) |> 
  # ⬇️ Filtramos NAs, no clasificados y excluimos el tramo de 18 a 24 años (valor 21)
  filter(
    !is.na(edad_num), 
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado",
    edad_num != 21 
  ) |> 
  
  pivot_longer(
    cols = starts_with("bienestar_"),
    names_to = "dimension",
    values_to = "puntuacion"
  ) |> 
  
  filter(!is.na(puntuacion)) |> 
  
  mutate(
    # Limpiamos los nombres de las dimensiones
    dimension = case_when(
      dimension == "bienestar_satisfaccion" ~ "Satisfacción general",
      dimension == "bienestar_integridad"   ~ "Integridad",
      dimension == "bienestar_desarrollo"   ~ "Desarrollo personal",
      dimension == "bienestar_libertad"     ~ "Libertad / Autonomía",
      dimension == "bienestar_necesidades"  ~ "Necesidades cubiertas",
      dimension == "bienestar_pertenencia"  ~ "Sentido de pertenencia",
      dimension == "bienestar_agencia"      ~ "Agencia / Control",
      TRUE ~ dimension
    ),
    
    # ⬇️ FORZAMOS EL ORDEN EN Z QUE HAS PEDIDO ⬇️
    dimension = factor(dimension, levels = c(
      "Satisfacción general",   "Integridad",              # Fila 1
      "Desarrollo personal",    "Necesidades cubiertas",   # Fila 2
      "Libertad / Autonomía",   "Sentido de pertenencia",  # Fila 3
      "Agencia / Control"                                  # Fila 4 (quedará sola a la izquierda)
    )),
    
    # Preparamos las etiquetas del perfil (líneas)
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    
    # Orden de los perfiles para la escala Mako
    perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
  ) |> 
  
  # Calculamos la media ponderada por dimensión, perfil y edad
  group_by(dimension, perfil_label, edad_num) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Visualización: Gráfico Alternativo (Facetas = Dimensiones, Líneas = Perfiles)
p_alt_bienestar <- datos_bienestar_alt |> 
  ggplot(aes(x = edad_num, y = media_ponderada, color = perfil_label)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # ⬇️ Facetado en 2 columnas siguiendo nuestro factor ⬇️
  facet_wrap(~ dimension, ncol = 2) +
  
  # Aplicamos la escala Mako ajustada para visibilidad
  scale_color_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
  
  # ⬇️ Eje X reconfigurado: Omitimos el punto de ruptura 21 y la etiqueta "18-24" ⬇️
  scale_x_continuous(
    breaks = c(29.5, 39.5, 49.5, 59.5, 70),
    labels = c("25-34", "35-44", "45-54", "55-64", "65+")
  ) +
  # Eje Y en formato porcentaje
  scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
  
  labs(
    title = "Evolución de las dimensiones de bienestar según la edad",
    subtitle = "Comparativa de perfiles ante cada dimensión (Grupo 18-24 excluido por baja N)", # ⬅️ Subtítulo actualizado
    x = "Tramo de Edad",
    y = "% de acuerdo",
    color = "Perfil de Voluntariado" 
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e", face = "bold"),
    axis.text.y = element_text(size = 9, color = "#34495e", face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 9),
    
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
    strip.background = element_rect(fill = "gray90", color = NA),           
    panel.spacing = unit(1.5, "lines") 
  ) +
  
  guides(color = guide_legend(nrow = 1))

# Mostramos y exportamos
print(p_alt_bienestar)

ggsave(
  filename = "gráficas/lineas_tendencia_bienestar_edad_por_perfiles.png", 
  plot = p_alt_bienestar, 
  width = 11, 
  height = 10.5, # Un pelín más alto que el de 6 facetas porque este tiene 4 filas (por la 7ª dimensión suelta)
  dpi = 300
)
# ==== LINEAS BIENESTAR POR TAMAÑO POB ====
# 1. Preparación de Datos: Dimensiones de bienestar cruzadas por Perfil y Población
datos_bienestar_pob_alt <- df |> 
  select(perfil_voluntariado, tamaño_pob, weight, starts_with("bienestar_")) |> 
  filter(!is.na(tamaño_pob), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado",
         !tamaño_pob %in% c("Menos de 2.000 personas", "De 2.000 a 5.000 personas", "De 5.000 a 10.000 personas")) |> 
  
  mutate(
    # Forzamos el orden INVERTIDO de los factores de población (de mayor a menor)
    tamaño_pob = factor(tamaño_pob, levels = c(
      "Más de 500.000 personas",
      "De 200.000 a 500.000 personas", 
      "De 50.000 a 200.000 personas",
      "De 10.000 a 50.000 personas"
    )),
    
    # Preparamos las etiquetas del perfil (líneas) y su orden para la paleta mako
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
  ) |> 
  
  pivot_longer(
    cols = starts_with("bienestar_"),
    names_to = "dimension",
    values_to = "puntuacion"
  ) |> 
  
  filter(!is.na(puntuacion)) |> 
  
  mutate(
    # Limpiamos los nombres de las dimensiones
    dimension = case_when(
      dimension == "bienestar_satisfaccion" ~ "Satisfacción general",
      dimension == "bienestar_integridad"   ~ "Integridad",
      dimension == "bienestar_desarrollo"   ~ "Desarrollo personal",
      dimension == "bienestar_libertad"     ~ "Libertad / Autonomía",
      dimension == "bienestar_necesidades"  ~ "Necesidades cubiertas",
      dimension == "bienestar_pertenencia"  ~ "Sentido de pertenencia",
      dimension == "bienestar_agencia"      ~ "Agencia / Control",
      TRUE ~ dimension
    ),
    
    # ⬇️ FORZAMOS EL ORDEN EN Z QUE HAS PEDIDO ⬇️
    dimension = factor(dimension, levels = c(
      "Satisfacción general",   "Integridad",              # Fila 1
      "Desarrollo personal",    "Necesidades cubiertas",   # Fila 2
      "Libertad / Autonomía",   "Sentido de pertenencia",  # Fila 3
      "Agencia / Control"                                  # Fila 4
    ))
  ) |> 
  
  # Calculamos la media ponderada por dimensión, perfil y tamaño poblacional
  group_by(dimension, perfil_label, tamaño_pob) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Visualización: Gráfico Alternativo (Población)
p_alt_bienestar_pob <- datos_bienestar_pob_alt |> 
  # AHORA EL COLOR Y EL GRUPO SON EL PERFIL
  ggplot(aes(x = tamaño_pob, y = media_ponderada, color = perfil_label, group = perfil_label)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # ⬇️ Facetado en 2 columnas siguiendo nuestro factor ⬇️
  facet_wrap(~ dimension, ncol = 2) +
  
  # Aplicamos la escala Mako ajustada para visibilidad
  scale_color_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
  
  scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
  
  # Etiquetas resumidas para el eje X
  scale_x_discrete(
    labels = c(
      "Menos de 2.000 personas"       = "< 2.000",
      "De 2.000 a 5.000 personas"     = "2.000 - 5.000",
      "De 5.000 a 10.000 personas"    = "5.000 - 10.000",
      "De 10.000 a 50.000 personas"   = "10.000 - 50.000",
      "De 50.000 a 200.000 personas"  = "50.000 - 200.000",
      "De 200.000 a 500.000 personas" = "200.000 - 500.000",
      "Más de 500.000 personas"       = "+500.000"
    )
  ) +
  
  labs(
    title = "Dimensiones de bienestar según el tamaño del municipio",
    subtitle = "Comparativa de los distintos perfiles de voluntariado ante cada dimensión",
    x = "Tamaño de Población",
    y = "% de acuerdo",
    color = "Perfil de Voluntariado"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    # Inclinamos los textos del eje X a 45 grados
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e", face = "bold"),
    axis.text.y = element_text(size = 9, color = "#34495e", face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 9),
    
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
    strip.background = element_rect(fill = "gray90", color = NA),           
    panel.spacing = unit(1.5, "lines") 
  ) +
  
  # Forzamos la leyenda a 1 fila para no ocupar demasiado alto
  guides(color = guide_legend(nrow = 1))

# Mostramos y exportamos
print(p_alt_bienestar_pob)

ggsave(
  filename = "gráficas/lineas_tendencia_bienestar_poblacion_por_perfiles.png", 
  plot = p_alt_bienestar_pob, 
  width = 11, 
  height = 11.5, # Ajustado para acomodar las 4 filas y las etiquetas en ángulo
  dpi = 300
)
# ==== LINEAS BIENESTAR TEC POR EDAD ====
# 1. Preparación de Datos: Dimensiones de bienestar tecnológico cruzadas por Perfil
datos_bienestartec_alt <- df |> 
  select(perfil_voluntariado, edad_num, weight, starts_with("bienestartec_")) |> 
  # Filtramos NAs, no clasificados y excluimos el tramo de 18 a 24 años (valor 21)
  filter(
    !is.na(edad_num), 
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado",
    edad_num != 21
  ) |> 
  
  pivot_longer(
    cols = starts_with("bienestartec_"),
    names_to = "dimension_tec",
    values_to = "puntuacion"
  ) |> 
  filter(!is.na(puntuacion)) |> 
  
  mutate(
    # Limpiamos los nombres de las dimensiones
    dimension_tec = case_when(
      dimension_tec == "bienestartec_competencias" ~ "Autosuficiencia y competencias",
      dimension_tec == "bienestartec_al_dia"       ~ "Acceso y actualización tecnológica",
      dimension_tec == "bienestartec_no_adicto"    ~ "Uso saludable (sin adicción)",
      dimension_tec == "bienestartec_conectado"    ~ "Conectado a otros y al mundo digital",
      dimension_tec == "bienestartec_critico"      ~ "Pensamiento crítico digital",
      TRUE ~ dimension_tec
    ),
    
    # Forzamos el orden en Z
    dimension_tec = factor(dimension_tec, levels = c(
      "Autosuficiencia y competencias",     "Uso saludable (sin adicción)",       # Fila 1
      "Pensamiento crítico digital",        "Acceso y actualización tecnológica", # Fila 2
      "Conectado a otros y al mundo digital"                                      # Fila 3
    )),
    
    # Preparamos las etiquetas del perfil (líneas) y su orden para la paleta mako
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
  ) |> 
  
  # Calculamos la media ponderada por dimensión, perfil y edad
  group_by(dimension_tec, perfil_label, edad_num) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Visualización: Gráfico Alternativo (Facetas = Dimensiones, Líneas = Perfiles)
p_alt_bienestartec <- datos_bienestartec_alt |> 
  # AHORA EL COLOR ES EL PERFIL
  ggplot(aes(x = edad_num, y = media_ponderada, color = perfil_label)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # Facetado en 2 columnas siguiendo nuestro factor
  facet_wrap(~ dimension_tec, ncol = 2) +
  
  # Aplicamos la escala Mako ajustada para visibilidad
  scale_color_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
  
  # Configuramos el eje X: Omitimos el punto de ruptura 21 y la etiqueta "18-24"
  scale_x_continuous(
    breaks = c(29.5, 39.5, 49.5, 59.5, 70),
    labels = c("25-34", "35-44", "45-54", "55-64", "65+")
  ) +
  # Eje Y en formato porcentaje
  scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
  
  labs(
    title = "Evolución del bienestar tecnológico según la edad",
    subtitle = "Comparativa de perfiles ante cada dimensión (Grupo 18-24 excluido por baja N)",
    x = "Tramo de Edad",
    y = "% de acuerdo",
    color = "Perfil de Voluntariado" 
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    # Textos en el eje X ligeramente girados para que encajen perfectos
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e", face = "bold"),
    axis.text.y = element_text(size = 9, color = "#34495e", face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 9),
    
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
    strip.background = element_rect(fill = "gray90", color = NA),           
    panel.spacing = unit(1.5, "lines") 
  ) +
  
  # Leyenda en 1 sola fila
  guides(color = guide_legend(nrow = 1))

# Mostramos y exportamos
print(p_alt_bienestartec)

ggsave(
  filename = "gráficas/lineas_tendencia_bienestartec_edad_por_perfiles.png", 
  plot = p_alt_bienestartec, 
  width = 11, 
  height = 9, # Altura de 9 es ideal para una cuadrícula de 3 filas
  dpi = 300
)
# ==== LINEAS BIENESTAR TEC POR TAMAÑO POB ====
# 1. Preparación de Datos: Dimensiones de bienestar tecnológico cruzadas por Perfil y Población
datos_bienestartec_pob_alt <- df |> 
  select(perfil_voluntariado, tamaño_pob, weight, starts_with("bienestartec_")) |> 
  filter(!is.na(tamaño_pob), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado",
         !tamaño_pob %in% c("Menos de 2.000 personas", "De 2.000 a 5.000 personas", "De 5.000 a 10.000 personas")) |> 
  
  mutate(
    # Forzamos el orden INVERTIDO de los factores de población (de mayor a menor)
    tamaño_pob = factor(tamaño_pob, levels = c(
      "Más de 500.000 personas",
      "De 200.000 a 500.000 personas", 
      "De 50.000 a 200.000 personas",
      "De 10.000 a 50.000 personas"
    )),
    
    # Preparamos las etiquetas del perfil y su orden (para paleta Mako)
    perfil_label = case_when(
      perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
      perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
      perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
      perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
    ),
    perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
  ) |> 
  
  pivot_longer(
    cols = starts_with("bienestartec_"),
    names_to = "dimension_tec",
    values_to = "puntuacion"
  ) |> 
  
  filter(!is.na(puntuacion)) |> 
  
  mutate(
    # Aplicamos las etiquetas precisas
    dimension_tec = case_when(
      dimension_tec == "bienestartec_competencias" ~ "Autosuficiencia y competencias",
      dimension_tec == "bienestartec_al_dia"       ~ "Acceso y actualización tecnológica",
      dimension_tec == "bienestartec_no_adicto"    ~ "Uso saludable (sin adicción)",
      dimension_tec == "bienestartec_conectado"    ~ "Integración y conexión digital",
      dimension_tec == "bienestartec_critico"      ~ "Pensamiento crítico digital",
      TRUE ~ dimension_tec
    ),
    
    # ⬇️ FORZAMOS EL ORDEN EN Z SOLICITADO ⬇️
    dimension_tec = factor(dimension_tec, levels = c(
      "Autosuficiencia y competencias",     "Uso saludable (sin adicción)",       # Fila 1
      "Pensamiento crítico digital",        "Acceso y actualización tecnológica", # Fila 2
      "Integración y conexión digital"                                            # Fila 3
    ))
  ) |> 
  
  # Calculamos la media ponderada
  group_by(dimension_tec, perfil_label, tamaño_pob) |> 
  summarise(
    media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Visualización: Gráfico Alternativo (Población)
p_alt_bienestartec_pob <- datos_bienestartec_pob_alt |> 
  # EL COLOR Y EL GRUPO SON AHORA EL PERFIL
  ggplot(aes(x = tamaño_pob, y = media_ponderada, color = perfil_label, group = perfil_label)) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # ⬇️ Facetado en 2 columnas ⬇️
  facet_wrap(~ dimension_tec, ncol = 2) +
  
  # Paleta Mako ajustada
  scale_color_viridis_d(option = "mako", begin = 0.15, end = 0.85) +
  scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
  
  # Etiquetas resumidas para el eje X
  scale_x_discrete(
    labels = c(
      "Menos de 2.000 personas"       = "< 2.000",
      "De 2.000 a 5.000 personas"     = "2.000 - 5.000",
      "De 5.000 a 10.000 personas"    = "5.000 - 10.000",
      "De 10.000 a 50.000 personas"   = "10.000 - 50.000",
      "De 50.000 a 200.000 personas"  = "50.000 - 200.000",
      "De 200.000 a 500.000 personas" = "200.000 - 500.000",
      "Más de 500.000 personas"       = "+500.000"
    )
  ) +
  
  labs(
    title = "Bienestar tecnológico según el tamaño del municipio",
    subtitle = "Comparativa de los distintos perfiles de voluntariado ante cada dimensión",
    x = "Tamaño de Población",
    y = "% de acuerdo",
    color = "Perfil de Voluntariado" 
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
    
    # Inclinamos los textos del eje X
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "#34495e", face = "bold"),
    axis.text.y = element_text(size = 9, color = "#34495e", face = "bold"),
    
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 9),
    
    strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
    strip.background = element_rect(fill = "gray90", color = NA),           
    panel.spacing = unit(1.5, "lines") 
  ) +
  
  # Forzamos la leyenda en una sola línea
  guides(color = guide_legend(nrow = 1))

# Mostramos y exportamos
print(p_alt_bienestartec_pob)

ggsave(
  filename = "gráficas/lineas_tendencia_bienestartec_poblacion_por_perfiles.png", 
  plot = p_alt_bienestartec_pob, 
  width = 11, 
  height = 10, # Ideal para las 3 filas y la inclinación a 45º
  dpi = 300
)
# ====

########### CORRPLOTS PERFIL VOL ########### ====
# ==== CORRPLOT DEF VOL X PERFIL VOL ====
# 1. Filtramos y preparamos los datos
df_cor_perfil <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado" 
  ) |> 
  select(weight, perfil_voluntariado, starts_with("def_vol_")) |> 
  drop_na() |> 
  mutate(
    perfil_no_voluntario = ifelse(perfil_voluntariado == "no_voluntario", 1, 0),
    perfil_vol_pasado    = ifelse(perfil_voluntariado == "voluntario_pasado", 1, 0),
    perfil_vol_actual    = ifelse(perfil_voluntariado == "voluntario_actual", 1, 0),
    perfil_vol_tec       = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)
  ) |> 
  # ⬇️ LA CORRECCIÓN: Borramos la variable de texto original para que no contamine la matriz
  select(-perfil_voluntariado)

# 2. Separamos las matrices numéricas
matriz_def_vol <- df_cor_perfil |> select(starts_with("def_vol_")) |> as.matrix()
matriz_perfil  <- df_cor_perfil |> select(starts_with("perfil_")) |> as.matrix()
pesos_perfil   <- df_cor_perfil$weight
# 3. Calculamos la correlación ponderada cruzada
resultados_cor <- weights::wtd.cor(x = matriz_def_vol, y = matriz_perfil, weight = pesos_perfil)

# Extraemos la matriz de coeficientes (r) para el color
mat_correlacion_perfil <- resultados_cor$correlation

# Extraemos la matriz de p-valores (p) para la significatividad
mat_pvalues_perfil <- resultados_cor$p.value

# Filtro DINÁMICO: si el p-valor es mayor a 0.05 (no significativo), ponemos 1 (bloqueado/gris)
mat_filtro_perfil <- ifelse(mat_pvalues_perfil > 0.05, 1, 0)

# 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES
diccionario_nombres_perfil <- c(
  # Definición de Voluntariado
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias",
  
  # Perfiles (Las dummies que creamos en el paso 1)
  "perfil_no_voluntario" = "Nunca voluntario",
  "perfil_vol_pasado"    = "Voluntario Pasado",
  "perfil_vol_actual"    = "Voluntario Actual",
  "perfil_vol_tec"       = "Cibervoluntarios"
)

traducir_seguro_perfil <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres_perfil[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    # Quitamos prefijos por si se nos cuela alguna no registrada
    str_remove_all(nombres_originales, "def_vol_|perfil_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora a filas y columnas
rownames(mat_correlacion_perfil) <- traducir_seguro_perfil(rownames(mat_correlacion_perfil))
colnames(mat_correlacion_perfil) <- traducir_seguro_perfil(colnames(mat_correlacion_perfil))

rownames(mat_filtro_perfil) <- rownames(mat_correlacion_perfil)
colnames(mat_filtro_perfil) <- colnames(mat_correlacion_perfil)

# 5. Visualización con Corrplot
png("gráficas/corrplot_perfil_vs_def_vol.png", width = 10, height = 7, units = "in", res = 300)

corrplot::corrplot(
  mat_correlacion_perfil, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro_perfil,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Perfil de Voluntariado y Definiciones de Voluntariado\n(Casillas grises indican relaciones débiles <= 0.07)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT BIENESTAR X PERFIL VOL ====
# 1. Filtramos y preparamos los datos
df_cor_bienestar <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado" 
  ) |> 
  select(weight, perfil_voluntariado, starts_with("bienestar_")) |> 
  drop_na() |> 
  # Transformamos la variable categórica en Dummies
  mutate(
    perfil_no_voluntario = ifelse(perfil_voluntariado == "no_voluntario", 1, 0),
    perfil_vol_pasado    = ifelse(perfil_voluntariado == "voluntario_pasado", 1, 0),
    perfil_vol_actual    = ifelse(perfil_voluntariado == "voluntario_actual", 1, 0),
    perfil_vol_tec       = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)
  ) |> 
  # ¡LA CORRECCIÓN! Borramos la variable de texto original
  select(-perfil_voluntariado)

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor_bienestar |> select(starts_with("bienestar_")) |> as.matrix()
matriz_perfil    <- df_cor_bienestar |> select(starts_with("perfil_")) |> as.matrix()
pesos_bienestar  <- df_cor_bienestar$weight

# 3. Calculamos la correlación ponderada (El "Método Pro")
resultados_cor_bienestar <- weights::wtd.cor(x = matriz_bienestar, y = matriz_perfil, weight = pesos_bienestar)

# Extraemos correlación y p-valores
mat_correlacion_bienestar <- resultados_cor_bienestar$correlation
mat_pvalues_bienestar     <- resultados_cor_bienestar$p.value

# Filtro DINÁMICO: si el p-valor es > 0.05, no es significativo (1 -> gris), sino 0 (color)
mat_filtro_bienestar <- ifelse(mat_pvalues_bienestar > 0.05, 1, 0)

# 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES
diccionario_nombres_bienestar <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Perfiles
  "perfil_no_voluntario" = "Nunca voluntario",
  "perfil_vol_pasado"    = "Voluntario Pasado",
  "perfil_vol_actual"    = "Voluntario Actual",
  "perfil_vol_tec"       = "Cibervoluntarios"
)

traducir_seguro_bienestar <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres_bienestar[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|perfil_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora a filas y columnas
rownames(mat_correlacion_bienestar) <- traducir_seguro_bienestar(rownames(mat_correlacion_bienestar))
colnames(mat_correlacion_bienestar) <- traducir_seguro_bienestar(colnames(mat_correlacion_bienestar))

rownames(mat_filtro_bienestar) <- rownames(mat_correlacion_bienestar)
colnames(mat_filtro_bienestar) <- colnames(mat_correlacion_bienestar)

# 5. Visualización con Corrplot
png("gráficas/corrplot_perfil_vs_bienestar.png", width = 10, height = 7, units = "in", res = 300)

corrplot::corrplot(
  mat_correlacion_bienestar, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad dinámico
  p.mat = mat_filtro_bienestar,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Perfil de Voluntariado y Dimensiones del Bienestar\n(Casillas grises indican relaciones no significativas, p > 0.05)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT BIENESTARTEC X PERFIL VOL ====
# 1. Filtramos y preparamos los datos
df_cor_bte <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado" 
  ) |> 
  # ¡Ojo aquí! Usamos el prefijo correcto del codebook: bienestartec_
  select(weight, perfil_voluntariado, starts_with("bienestartec_")) |> 
  drop_na() |> 
  # Transformamos la variable categórica en Dummies
  mutate(
    perfil_no_voluntario = ifelse(perfil_voluntariado == "no_voluntario", 1, 0),
    perfil_vol_pasado    = ifelse(perfil_voluntariado == "voluntario_pasado", 1, 0),
    perfil_vol_actual    = ifelse(perfil_voluntariado == "voluntario_actual", 1, 0),
    perfil_vol_tec       = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)
  ) |> 
  # Borramos la variable de texto original para que la matriz no falle
  select(-perfil_voluntariado)

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor_bte |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_perfil       <- df_cor_bte |> select(starts_with("perfil_")) |> as.matrix()
pesos_bienestartec  <- df_cor_bte$weight

# 3. Calculamos la correlación ponderada (El "Método Pro")
resultados_cor_bte <- weights::wtd.cor(x = matriz_bienestartec, y = matriz_perfil, weight = pesos_bienestartec)

# Extraemos correlación y p-valores
mat_correlacion_bte <- resultados_cor_bte$correlation
mat_pvalues_bte     <- resultados_cor_bte$p.value

# Filtro DINÁMICO: si el p-valor es > 0.05, no es significativo (1 -> gris), sino 0 (color)
mat_filtro_bte <- ifelse(mat_pvalues_bte > 0.05, 1, 0)

# 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES (Extraída de tu codebook)
diccionario_nombres_bte <- c(
  # Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Perfiles
  "perfil_no_voluntario" = "Nunca voluntario",
  "perfil_vol_pasado"    = "Voluntario Pasado",
  "perfil_vol_actual"    = "Voluntario Actual",
  "perfil_vol_tec"       = "Cibervoluntarios"
)

traducir_seguro_bte <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres_bte[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|perfil_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora a filas y columnas
rownames(mat_correlacion_bte) <- traducir_seguro_bte(rownames(mat_correlacion_bte))
colnames(mat_correlacion_bte) <- traducir_seguro_bte(colnames(mat_correlacion_bte))

rownames(mat_filtro_bte) <- rownames(mat_correlacion_bte)
colnames(mat_filtro_bte) <- colnames(mat_correlacion_bte)

# 5. Visualización con Corrplot
png("gráficas/corrplot_perfil_vs_bienestartec.png", width = 10, height = 7, units = "in", res = 300)

corrplot::corrplot(
  mat_correlacion_bte, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad dinámico
  p.mat = mat_filtro_bte,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Perfil de Voluntariado y Bienestar Tecnológico\n(Casillas grises indican relaciones no significativas, p > 0.05)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT RESP X PERFIL VOL ====
# 1. Filtramos y preparamos los datos
df_cor_resp <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado" 
  ) |> 
  # ⬇️ EL TRUCO: Seleccionamos resp_ pero EXCLUIMOS resp_tec_ para que no se mezclen
  select(weight, perfil_voluntariado, starts_with("resp_") & !starts_with("resp_tec_")) |> 
  drop_na() |> 
  # Transformamos la variable categórica en Dummies
  mutate(
    perfil_no_voluntario = ifelse(perfil_voluntariado == "no_voluntario", 1, 0),
    perfil_vol_pasado    = ifelse(perfil_voluntariado == "voluntario_pasado", 1, 0),
    perfil_vol_actual    = ifelse(perfil_voluntariado == "voluntario_actual", 1, 0),
    perfil_vol_tec       = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)
  ) |> 
  # Borramos la variable de texto original para que la matriz no falle
  select(-perfil_voluntariado)

# 2. Separamos las matrices numéricas
# Aplicamos la misma lógica de exclusión aquí
matriz_resp   <- df_cor_resp |> select(starts_with("resp_") & !starts_with("resp_tec_")) |> as.matrix()
matriz_perfil <- df_cor_resp |> select(starts_with("perfil_")) |> as.matrix()
pesos_resp    <- df_cor_resp$weight

# 3. Calculamos la correlación ponderada (El "Método Pro")
resultados_cor_resp <- weights::wtd.cor(x = matriz_resp, y = matriz_perfil, weight = pesos_resp)

# Extraemos correlación y p-valores
mat_correlacion_resp <- resultados_cor_resp$correlation
mat_pvalues_resp     <- resultados_cor_resp$p.value

# Filtro DINÁMICO: si el p-valor es > 0.05, no es significativo (1 -> gris), sino 0 (color)
mat_filtro_resp <- ifelse(mat_pvalues_resp > 0.05, 1, 0)

# 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES (Extraída del Codebook)
diccionario_nombres_resp <- c(
  # Actores responsables del bienestar SOCIAL (no tecnológico)
  "resp_estado"     = "Estado / Adm. Pública",
  "resp_empresa"    = "Empresas",
  "resp_tercer"     = "Tercer Sector / ONG",
  "resp_ciudadania" = "Ciudadanía",
  "resp_educativas" = "Inst. Educativas",
  
  # Perfiles
  "perfil_no_voluntario" = "Nunca voluntario",
  "perfil_vol_pasado"    = "Voluntario Pasado",
  "perfil_vol_actual"    = "Voluntario Actual",
  "perfil_vol_tec"       = "Cibervoluntarios"
)

traducir_seguro_resp <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres_resp[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "resp_|perfil_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora a filas y columnas
rownames(mat_correlacion_resp) <- traducir_seguro_resp(rownames(mat_correlacion_resp))
colnames(mat_correlacion_resp) <- traducir_seguro_resp(colnames(mat_correlacion_resp))

rownames(mat_filtro_resp) <- rownames(mat_correlacion_resp)
colnames(mat_filtro_resp) <- colnames(mat_correlacion_resp)

# 5. Visualización con Corrplot
# Nombre de archivo siguiendo la nueva directriz: perfil_vs_resp
png("gráficas/corrplot_perfil_vs_resp.png", width = 10, height = 7, units = "in", res = 300)

corrplot::corrplot(
  mat_correlacion_resp, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad dinámico
  p.mat = mat_filtro_resp,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Perfil de Voluntariado y Responsables del Bienestar Social\n(Casillas grises indican relaciones no significativas, p > 0.05)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT RESPTEC X PERFIL VOL ====
# 1. Filtramos y preparamos los datos
df_cor_resptec <- df |> 
  filter(
    !is.na(perfil_voluntariado), 
    perfil_voluntariado != "no_clasificado" 
  ) |> 
  # Usamos el prefijo correcto del codebook: resptec_
  select(weight, perfil_voluntariado, starts_with("resptec_")) |> 
  drop_na() |> 
  # Transformamos la variable categórica en Dummies
  mutate(
    perfil_no_voluntario = ifelse(perfil_voluntariado == "no_voluntario", 1, 0),
    perfil_vol_pasado    = ifelse(perfil_voluntariado == "voluntario_pasado", 1, 0),
    perfil_vol_actual    = ifelse(perfil_voluntariado == "voluntario_actual", 1, 0),
    perfil_vol_tec       = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)
  ) |> 
  # Borramos la variable de texto original para que la matriz no falle
  select(-perfil_voluntariado)

# 2. Separamos las matrices numéricas
matriz_resptec <- df_cor_resptec |> select(starts_with("resptec_")) |> as.matrix()
matriz_perfil  <- df_cor_resptec |> select(starts_with("perfil_")) |> as.matrix()
pesos_resptec  <- df_cor_resptec$weight

# 3. Calculamos la correlación ponderada (El "Método Pro")
resultados_cor_resptec <- weights::wtd.cor(x = matriz_resptec, y = matriz_perfil, weight = pesos_resptec)

# Extraemos correlación y p-valores
mat_correlacion_resptec <- resultados_cor_resptec$correlation
mat_pvalues_resptec     <- resultados_cor_resptec$p.value

# Filtro DINÁMICO: si el p-valor es > 0.05, no es significativo (1 -> gris), sino 0 (color)
mat_filtro_resptec <- ifelse(mat_pvalues_resptec > 0.05, 1, 0)

# 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES (Extraída de tu codebook)
diccionario_nombres_resptec <- c(
  # Actores responsables del bienestar tecnológico
  "resptec_estado"     = "Estado / Adm. Pública",
  "resptec_empresa"    = "Empresas",
  "resptec_tercer"     = "Tercer Sector / ONG",
  "resptec_ciudadania" = "Ciudadanía",
  "resptec_educativas" = "Inst. Educativas",
  
  # Perfiles
  "perfil_no_voluntario" = "Nunca voluntario",
  "perfil_vol_pasado"    = "Voluntario Pasado",
  "perfil_vol_actual"    = "Voluntario Actual",
  "perfil_vol_tec"       = "Cibervoluntarios"
)

traducir_seguro_resptec <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres_resptec[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "resptec_|perfil_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora a filas y columnas
rownames(mat_correlacion_resptec) <- traducir_seguro_resptec(rownames(mat_correlacion_resptec))
colnames(mat_correlacion_resptec) <- traducir_seguro_resptec(colnames(mat_correlacion_resptec))

rownames(mat_filtro_resptec) <- rownames(mat_correlacion_resptec)
colnames(mat_filtro_resptec) <- colnames(mat_correlacion_resptec)

# 5. Visualización con Corrplot
# ⬇️ NUEVA NOMENCLATURA: perfil_vs_resptec
png("gráficas/corrplot_perfil_vs_resptec.png", width = 10, height = 7, units = "in", res = 300)

corrplot::corrplot(
  mat_correlacion_resptec, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad dinámico
  p.mat = mat_filtro_resptec,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Perfil de Voluntariado y Responsables del Bienestar Tech\n(Casillas grises indican relaciones no significativas, p > 0.05)",
  mar = c(0,0,4,0)           
)

dev.off()

########### BIENESTAR NO VOLUNTARIOS ########### ====
# ==== CORRPLOT RESP X BIENESTAR ====

# 1. Filtramos y preparamos los datos
df_cor2 <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  select(weight, starts_with("bienestar_"), starts_with("resp_")) |> 
  # CRUCIAL: Usamos drop_na() en lugar de rellenar con 0 para respetar la escala Likert (1-5)
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor2 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_resp      <- df_cor2 |> select(starts_with("resp_")) |> as.matrix()
pesos2           <- df_cor2$weight

# 3. Calculamos la correlación ponderada
mat_correlacion2 <- wtd.cor(x = matriz_bienestar, y = matriz_resp, weight = pesos2)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro2 <- ifelse(abs(mat_correlacion2) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres2 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Atribución de Responsabilidad
  "resp_estado"      = "Estado / Gobiernos",
  "resp_empresa"     = "Empresas",
  "resp_tercer"      = "Tercer Sector / ONG",
  "resp_ciudadania"  = "Ciudadanía",
  "resp_educativas"  = "Inst. Educativas"
)

traducir_seguro2 <- function(nombres_originales) {
  # Buscamos cada nombre en el diccionario
  nombres_traducidos <- diccionario_nombres2[nombres_originales]
  
  # Si alguna variable no estaba en el diccionario (da NA), simplemente le borramos el prefijo
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|resp_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales)) # Devolvemos el vector limpio
}

# Aplicamos la función (esto garantiza que las dimensiones cuadren perfectamente)
rownames(mat_correlacion2) <- traducir_seguro2(rownames(mat_correlacion2))
colnames(mat_correlacion2) <- traducir_seguro2(colnames(mat_correlacion2))

rownames(mat_filtro2) <- rownames(mat_correlacion2)
colnames(mat_filtro2) <- colnames(mat_correlacion2)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestar_vs_resp_NO_VOL.png", width = 10, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion2, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro2,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  # Círculo sólido que cubre el cuadro
  pch.col = "gray80",        # En color gris tenue
  pch.cex = 4,               # Lo hacemos un pelín más grande para que tape bien
  
  title = "Correlación entre Dimensiones de Bienestar y Atribución de Responsabilidad\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT RESP TEC X BIENESTAR TEC ====

# 1. Filtramos y preparamos los datos
df_cor3 <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  # Seleccionamos las nuevas familias de variables
  select(weight, starts_with("bienestartec_"), starts_with("resptec_")) |> 
  # Respetamos el Likert eliminando NAs en lugar de poner 0
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor3 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_resptec      <- df_cor3 |> select(starts_with("resptec_")) |> as.matrix()
pesos3              <- df_cor3$weight

# 3. Calculamos la correlación ponderada
mat_correlacion3 <- wtd.cor(x = matriz_bienestartec, y = matriz_resptec, weight = pesos3)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro3 <- ifelse(abs(mat_correlacion3) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres3 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Atribución de Responsabilidad Tecnológica (revisa si coinciden con tu codebook)
  "resptec_estado"      = "Estado / Gobiernos",
  "resptec_empresa"     = "Empresas",
  "resptec_tercer"      = "Tercer Sector / ONG",
  "resptec_ciudadania"  = "Ciudadanía",
  "resptec_educativas"  = "Inst. Educativas"
)

traducir_seguro3 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres3[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|resptec_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion3) <- traducir_seguro3(rownames(mat_correlacion3))
colnames(mat_correlacion3) <- traducir_seguro3(colnames(mat_correlacion3))

rownames(mat_filtro3) <- rownames(mat_correlacion3)
colnames(mat_filtro3) <- colnames(mat_correlacion3)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestartec_vs_resptec_NO_VOL.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion3, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro3,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones de Bienestar Tecnológico y Atribución de Responsabilidad\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()



# ==== CORRPLOT DEF VOL X BIENESTAR ====

# 1. Filtramos y preparamos los datos
df_cor4 <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  select(weight, starts_with("bienestar_"), starts_with("def_vol_")) |> 
  # CRUCIAL: Mantenemos drop_na() porque def_vol_ es Likert
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor4 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_def_vol   <- df_cor4 |> select(starts_with("def_vol_")) |> as.matrix()
pesos4           <- df_cor4$weight

# 3. Calculamos la correlación ponderada
mat_correlacion4 <- wtd.cor(x = matriz_bienestar, y = matriz_def_vol, weight = pesos4)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro4 <- ifelse(abs(mat_correlacion4) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres4 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Definición de Voluntariado (Resumidas del Codebook)
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias"
)

traducir_seguro4 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres4[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|def_vol_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion4) <- traducir_seguro4(rownames(mat_correlacion4))
colnames(mat_correlacion4) <- traducir_seguro4(colnames(mat_correlacion4))

rownames(mat_filtro4) <- rownames(mat_correlacion4)
colnames(mat_filtro4) <- colnames(mat_correlacion4)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestar_vs_def_vol_NO_VOL.png", width = 10, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion4, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro4,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones del Bienestar y Def. de Voluntariado\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT DEF VOL X BIENESTAR TEC ====

# 1. Filtramos y preparamos los datos
df_cor5 <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  select(weight, starts_with("bienestartec_"), starts_with("def_vol_")) |> 
  # Mantenemos drop_na() por la escala Likert de def_vol_
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor5 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_def_vol      <- df_cor5 |> select(starts_with("def_vol_")) |> as.matrix()
pesos5              <- df_cor5$weight

# 3. Calculamos la correlación ponderada
mat_correlacion5 <- wtd.cor(x = matriz_bienestartec, y = matriz_def_vol, weight = pesos5)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro5 <- ifelse(abs(mat_correlacion5) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres5 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Definición de Voluntariado (Resumidas del Codebook)
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias"
)

traducir_seguro5 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres5[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|def_vol_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion5) <- traducir_seguro5(rownames(mat_correlacion5))
colnames(mat_correlacion5) <- traducir_seguro5(colnames(mat_correlacion5))

rownames(mat_filtro5) <- rownames(mat_correlacion5)
colnames(mat_filtro5) <- colnames(mat_correlacion5)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestartec_vs_def_vol_NO_VOL.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion5, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro5,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones del Bienestar Tecnológico y Def. de Voluntariado\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ====
########### BIENESTAR VOLUNTARIOS ########### ====
# ==== CORRPLOT RESP X BIENESTAR ====

# 1. Filtramos y preparamos los datos
df_cor2 <- df |> 
  filter(perfil_voluntariado == "voluntario_actual") |> 
  select(weight, starts_with("bienestar_"), starts_with("resp_")) |> 
  # CRUCIAL: Usamos drop_na() en lugar de rellenar con 0 para respetar la escala Likert (1-5)
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor2 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_resp      <- df_cor2 |> select(starts_with("resp_")) |> as.matrix()
pesos2           <- df_cor2$weight

# 3. Calculamos la correlación ponderada
mat_correlacion2 <- wtd.cor(x = matriz_bienestar, y = matriz_resp, weight = pesos2)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro2 <- ifelse(abs(mat_correlacion2) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres2 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Atribución de Responsabilidad
  "resp_estado"      = "Estado / Gobiernos",
  "resp_empresa"     = "Empresas",
  "resp_tercer"      = "Tercer Sector / ONG",
  "resp_ciudadania"  = "Ciudadanía",
  "resp_educativas"  = "Inst. Educativas"
)

traducir_seguro2 <- function(nombres_originales) {
  # Buscamos cada nombre en el diccionario
  nombres_traducidos <- diccionario_nombres2[nombres_originales]
  
  # Si alguna variable no estaba en el diccionario (da NA), simplemente le borramos el prefijo
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|resp_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales)) # Devolvemos el vector limpio
}

# Aplicamos la función (esto garantiza que las dimensiones cuadren perfectamente)
rownames(mat_correlacion2) <- traducir_seguro2(rownames(mat_correlacion2))
colnames(mat_correlacion2) <- traducir_seguro2(colnames(mat_correlacion2))

rownames(mat_filtro2) <- rownames(mat_correlacion2)
colnames(mat_filtro2) <- colnames(mat_correlacion2)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestar_vs_resp_VOL.png", width = 10, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion2, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro2,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  # Círculo sólido que cubre el cuadro
  pch.col = "gray80",        # En color gris tenue
  pch.cex = 4,               # Lo hacemos un pelín más grande para que tape bien
  
  title = "Correlación entre Dimensiones de Bienestar y Atribución de Responsabilidad\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT RESP TEC X BIENESTAR TEC ====

# 1. Filtramos y preparamos los datos
df_cor3 <- df |> 
  filter(perfil_voluntariado == "voluntario_actual") |> 
  # Seleccionamos las nuevas familias de variables
  select(weight, starts_with("bienestartec_"), starts_with("resptec_")) |> 
  # Respetamos el Likert eliminando NAs en lugar de poner 0
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor3 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_resptec      <- df_cor3 |> select(starts_with("resptec_")) |> as.matrix()
pesos3              <- df_cor3$weight

# 3. Calculamos la correlación ponderada
mat_correlacion3 <- wtd.cor(x = matriz_bienestartec, y = matriz_resptec, weight = pesos3)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro3 <- ifelse(abs(mat_correlacion3) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres3 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Atribución de Responsabilidad Tecnológica (revisa si coinciden con tu codebook)
  "resptec_estado"      = "Estado / Gobiernos",
  "resptec_empresa"     = "Empresas",
  "resptec_tercer"      = "Tercer Sector / ONG",
  "resptec_ciudadania"  = "Ciudadanía",
  "resptec_educativas"  = "Inst. Educativas"
)

traducir_seguro3 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres3[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|resptec_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion3) <- traducir_seguro3(rownames(mat_correlacion3))
colnames(mat_correlacion3) <- traducir_seguro3(colnames(mat_correlacion3))

rownames(mat_filtro3) <- rownames(mat_correlacion3)
colnames(mat_filtro3) <- colnames(mat_correlacion3)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestartec_vs_resptec_VOL.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion3, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro3,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones de Bienestar Tecnológico y Atribución de Responsabilidad\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()



# ==== CORRPLOT DEF VOL X BIENESTAR ====

# 1. Filtramos y preparamos los datos
df_cor4 <- df |> 
  filter(perfil_voluntariado == "voluntario_actual") |> 
  select(weight, starts_with("bienestar_"), starts_with("def_vol_")) |> 
  # CRUCIAL: Mantenemos drop_na() porque def_vol_ es Likert
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor4 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_def_vol   <- df_cor4 |> select(starts_with("def_vol_")) |> as.matrix()
pesos4           <- df_cor4$weight

# 3. Calculamos la correlación ponderada
mat_correlacion4 <- wtd.cor(x = matriz_bienestar, y = matriz_def_vol, weight = pesos4)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro4 <- ifelse(abs(mat_correlacion4) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres4 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Definición de Voluntariado (Resumidas del Codebook)
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias"
)

traducir_seguro4 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres4[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|def_vol_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion4) <- traducir_seguro4(rownames(mat_correlacion4))
colnames(mat_correlacion4) <- traducir_seguro4(colnames(mat_correlacion4))

rownames(mat_filtro4) <- rownames(mat_correlacion4)
colnames(mat_filtro4) <- colnames(mat_correlacion4)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestar_vs_def_vol_VOL.png", width = 10, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion4, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro4,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones del Bienestar y Def. de Voluntariado\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT DEF VOL X BIENESTAR TEC ====

# 1. Filtramos y preparamos los datos
df_cor5 <- df |> 
  filter(perfil_voluntariado == "voluntario_actual") |> 
  select(weight, starts_with("bienestartec_"), starts_with("def_vol_")) |> 
  # Mantenemos drop_na() por la escala Likert de def_vol_
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor5 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_def_vol      <- df_cor5 |> select(starts_with("def_vol_")) |> as.matrix()
pesos5              <- df_cor5$weight

# 3. Calculamos la correlación ponderada
mat_correlacion5 <- wtd.cor(x = matriz_bienestartec, y = matriz_def_vol, weight = pesos5)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro5 <- ifelse(abs(mat_correlacion5) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres5 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Definición de Voluntariado (Resumidas del Codebook)
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias"
)

traducir_seguro5 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres5[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|def_vol_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion5) <- traducir_seguro5(rownames(mat_correlacion5))
colnames(mat_correlacion5) <- traducir_seguro5(colnames(mat_correlacion5))

rownames(mat_filtro5) <- rownames(mat_correlacion5)
colnames(mat_filtro5) <- colnames(mat_correlacion5)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestartec_vs_def_vol_VOL.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion5, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro5,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones del Bienestar Tecnológico y Def. de Voluntariado\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ====
########### BIENESTAR VOLUNTARIOS TECNOLÓGICOS ########### ====
# ==== CORRPLOT RESP X BIENESTAR ====

# 1. Filtramos y preparamos los datos
df_cor2 <- df |> 
  filter(perfil_voluntariado == "cibervoluntarios") |> 
  select(weight, starts_with("bienestar_"), starts_with("resp_")) |> 
  # CRUCIAL: Usamos drop_na() en lugar de rellenar con 0 para respetar la escala Likert (1-5)
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor2 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_resp      <- df_cor2 |> select(starts_with("resp_")) |> as.matrix()
pesos2           <- df_cor2$weight

# 3. Calculamos la correlación ponderada
mat_correlacion2 <- wtd.cor(x = matriz_bienestar, y = matriz_resp, weight = pesos2)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro2 <- ifelse(abs(mat_correlacion2) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres2 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Atribución de Responsabilidad
  "resp_estado"      = "Estado / Gobiernos",
  "resp_empresa"     = "Empresas",
  "resp_tercer"      = "Tercer Sector / ONG",
  "resp_ciudadania"  = "Ciudadanía",
  "resp_educativas"  = "Inst. Educativas"
)

traducir_seguro2 <- function(nombres_originales) {
  # Buscamos cada nombre en el diccionario
  nombres_traducidos <- diccionario_nombres2[nombres_originales]
  
  # Si alguna variable no estaba en el diccionario (da NA), simplemente le borramos el prefijo
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|resp_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales)) # Devolvemos el vector limpio
}

# Aplicamos la función (esto garantiza que las dimensiones cuadren perfectamente)
rownames(mat_correlacion2) <- traducir_seguro2(rownames(mat_correlacion2))
colnames(mat_correlacion2) <- traducir_seguro2(colnames(mat_correlacion2))

rownames(mat_filtro2) <- rownames(mat_correlacion2)
colnames(mat_filtro2) <- colnames(mat_correlacion2)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestar_vs_resp_VOLTEC.png", width = 10, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion2, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro2,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  # Círculo sólido que cubre el cuadro
  pch.col = "gray80",        # En color gris tenue
  pch.cex = 4,               # Lo hacemos un pelín más grande para que tape bien
  
  title = "Correlación entre Dimensiones de Bienestar y Atribución de Responsabilidad\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT RESP TEC X BIENESTAR TEC ====

# 1. Filtramos y preparamos los datos
df_cor3 <- df |> 
  filter(perfil_voluntariado == "cibervoluntarios") |> 
  # Seleccionamos las nuevas familias de variables
  select(weight, starts_with("bienestartec_"), starts_with("resptec_")) |> 
  # Respetamos el Likert eliminando NAs en lugar de poner 0
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor3 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_resptec      <- df_cor3 |> select(starts_with("resptec_")) |> as.matrix()
pesos3              <- df_cor3$weight

# 3. Calculamos la correlación ponderada
mat_correlacion3 <- wtd.cor(x = matriz_bienestartec, y = matriz_resptec, weight = pesos3)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro3 <- ifelse(abs(mat_correlacion3) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres3 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Atribución de Responsabilidad Tecnológica (revisa si coinciden con tu codebook)
  "resptec_estado"      = "Estado / Gobiernos",
  "resptec_empresa"     = "Empresas",
  "resptec_tercer"      = "Tercer Sector / ONG",
  "resptec_ciudadania"  = "Ciudadanía",
  "resptec_educativas"  = "Inst. Educativas"
)

traducir_seguro3 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres3[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|resptec_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion3) <- traducir_seguro3(rownames(mat_correlacion3))
colnames(mat_correlacion3) <- traducir_seguro3(colnames(mat_correlacion3))

rownames(mat_filtro3) <- rownames(mat_correlacion3)
colnames(mat_filtro3) <- colnames(mat_correlacion3)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestartec_vs_resptec_VOLTEC.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion3, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro3,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones de Bienestar Tecnológico y Atribución de Responsabilidad\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()



# ==== CORRPLOT DEF VOL X BIENESTAR ====

# 1. Filtramos y preparamos los datos
df_cor4 <- df |> 
  filter(perfil_voluntariado == "cibervoluntarios") |>  
  select(weight, starts_with("bienestar_"), starts_with("def_vol_")) |> 
  # CRUCIAL: Mantenemos drop_na() porque def_vol_ es Likert
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor4 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_def_vol   <- df_cor4 |> select(starts_with("def_vol_")) |> as.matrix()
pesos4           <- df_cor4$weight

# 3. Calculamos la correlación ponderada
mat_correlacion4 <- wtd.cor(x = matriz_bienestar, y = matriz_def_vol, weight = pesos4)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro4 <- ifelse(abs(mat_correlacion4) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres4 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Definición de Voluntariado (Resumidas del Codebook)
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias"
)

traducir_seguro4 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres4[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|def_vol_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion4) <- traducir_seguro4(rownames(mat_correlacion4))
colnames(mat_correlacion4) <- traducir_seguro4(colnames(mat_correlacion4))

rownames(mat_filtro4) <- rownames(mat_correlacion4)
colnames(mat_filtro4) <- colnames(mat_correlacion4)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestar_vs_def_vol_VOLTEC.png", width = 10, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion4, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro4,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones del Bienestar y Def. de Voluntariado\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ==== CORRPLOT DEF VOL X BIENESTAR TEC ====

# 1. Filtramos y preparamos los datos
df_cor5 <- df |> 
  filter(perfil_voluntariado == "cibervoluntarios") |> 
  select(weight, starts_with("bienestartec_"), starts_with("def_vol_")) |> 
  # Mantenemos drop_na() por la escala Likert de def_vol_
  drop_na()

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor5 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_def_vol      <- df_cor5 |> select(starts_with("def_vol_")) |> as.matrix()
pesos5              <- df_cor5$weight

# 3. Calculamos la correlación ponderada
mat_correlacion5 <- wtd.cor(x = matriz_bienestartec, y = matriz_def_vol, weight = pesos5)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro5 <- ifelse(abs(mat_correlacion5) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres5 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Definición de Voluntariado (Resumidas del Codebook)
  "def_vol_apoyo_admin"     = "Apoyar a la Adm. Pública",
  "def_vol_transfor_social" = "Transformación social",
  "def_vol_organizacion"    = "Reducir desigualdades",
  "def_vol_ocio"            = "Ocio y tiempo libre",
  "def_vol_competencias"    = "Adquirir competencias"
)

traducir_seguro5 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres5[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|def_vol_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion5) <- traducir_seguro5(rownames(mat_correlacion5))
colnames(mat_correlacion5) <- traducir_seguro5(colnames(mat_correlacion5))

rownames(mat_filtro5) <- rownames(mat_correlacion5)
colnames(mat_filtro5) <- colnames(mat_correlacion5)

# 5. Visualización con Corrplot
png("gráficas/old/corrplot_bienestartec_vs_def_vol_VOLTEC.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion5, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro5,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  title = "Correlación entre Dimensiones del Bienestar Tecnológico y Def. de Voluntariado\n(Casillas grises indican relaciones no significativas)",
  mar = c(0,0,4,0)           
)

dev.off()
# ====
########### VOL TIPO ########### ====
# ==== CORRPLOT BIENESTAR X VOL TIPO ====

# 1. Filtramos y preparamos los datos
df_cor6 <- df |> 
  # Excluimos explícitamente a los no voluntarios
  filter(perfil_voluntariado != "no_voluntario") |> 
  select(weight, starts_with("bienestar_"), starts_with("volu_tipo_")) |> 
  # Como ambas familias son Dummy (0 y 1), rellenamos los NAs con 0
  mutate(across(everything(), ~replace_na(.x, 0)))

# 2. Separamos las matrices numéricas
matriz_bienestar <- df_cor6 |> select(starts_with("bienestar_")) |> as.matrix()
matriz_tipos     <- df_cor6 |> select(starts_with("volu_tipo_")) |> as.matrix()
pesos6           <- df_cor6$weight

# 3. Calculamos la correlación ponderada (Phi para variables dummy)
mat_correlacion6 <- wtd.cor(x = matriz_bienestar, y = matriz_tipos, weight = pesos6)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro6 <- ifelse(abs(mat_correlacion6) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres6 <- c(
  # Dimensiones de Bienestar
  "bienestar_satisfaccion" = "Satisfacción general",
  "bienestar_integridad"   = "Integridad",
  "bienestar_desarrollo"   = "Desarrollo personal",
  "bienestar_libertad"     = "Libertad / Autonomía",
  "bienestar_necesidades"  = "Necesidades cubiertas",
  "bienestar_pertenencia"  = "Sentido de pertenencia",
  "bienestar_agencia"      = "Agencia / Control",
  
  # Tipos de Voluntariado
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

traducir_seguro6 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres6[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestar_|volu_tipo_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion6) <- traducir_seguro6(rownames(mat_correlacion6))
colnames(mat_correlacion6) <- traducir_seguro6(colnames(mat_correlacion6))

rownames(mat_filtro6) <- rownames(mat_correlacion6)
colnames(mat_filtro6) <- colnames(mat_correlacion6)

# 5. Visualización con Corrplot
png("gráficas/corrplot_tipos_vs_bienestar.png", width = 11, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion6, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.8,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro6,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 3.5,             
  
  # Solo ponemos el TÍTULO PRINCIPAL aquí
  title = "Correlación entre Tipos de Voluntariado y Dimensiones de Bienestar",
  mar = c(0,0,4,0)           
)

# ⬇️ AÑADIMOS EL SUBTÍTULO FORMATEADO ⬇️
# side = 3 (arriba), line = 0.5 (separación), cex = tamaño, col = color
mtext("(Casillas grises indican relaciones no significativas)", side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()
# ==== CORRPLOT BIENESTAR TEC X VOL TIPO ====

# 1. Filtramos y preparamos los datos
df_cor7 <- df |> 
  # Excluimos explícitamente a los no voluntarios
  filter(perfil_voluntariado != "no_voluntario") |> 
  select(weight, starts_with("bienestartec_"), starts_with("volu_tipo_")) |> 
  # Como ambas familias son Dummy (0 y 1), rellenamos los NAs con 0
  mutate(across(everything(), ~replace_na(.x, 0)))

# 2. Separamos las matrices numéricas
matriz_bienestartec <- df_cor7 |> select(starts_with("bienestartec_")) |> as.matrix()
matriz_tipos        <- df_cor7 |> select(starts_with("volu_tipo_")) |> as.matrix()
pesos7              <- df_cor7$weight

# 3. Calculamos la correlación ponderada (Phi para variables dummy)
mat_correlacion7 <- wtd.cor(x = matriz_bienestartec, y = matriz_tipos, weight = pesos7)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro7 <- ifelse(abs(mat_correlacion7) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres7 <- c(
  # Dimensiones de Bienestar Tecnológico
  "bienestartec_competencias" = "Autosuficiencia y competencias",
  "bienestartec_al_dia"       = "Acceso y actualización",
  "bienestartec_no_adicto"    = "Uso saludable (sin adicción)",
  "bienestartec_conectado"    = "Integración y conexión",
  "bienestartec_critico"      = "Pensamiento crítico digital",
  
  # Tipos de Voluntariado
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

traducir_seguro7 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres7[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "bienestartec_|volu_tipo_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion7) <- traducir_seguro7(rownames(mat_correlacion7))
colnames(mat_correlacion7) <- traducir_seguro7(colnames(mat_correlacion7))

rownames(mat_filtro7) <- rownames(mat_correlacion7)
colnames(mat_filtro7) <- colnames(mat_correlacion7)

# 5. Visualización con Corrplot
png("gráficas/corrplot_tipos_vs_bienestartec.png", width = 11, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion7, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.8,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro7,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 3.5,             
  
  # Solo ponemos el TÍTULO PRINCIPAL aquí
  title = "Correlación entre Tipos de Voluntariado y Bienestar Tecnológico",
  mar = c(0,0,4,0)           
)

# ⬇️ AÑADIMOS EL SUBTÍTULO FORMATEADO ⬇️
mtext("(Casillas grises indican relaciones no significativas)", side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()
# ==== CORRPLOT RAZONES X VOL TIPO ====

# 1. Filtramos y preparamos los datos
df_cor <- df |> 
  filter(perfil_voluntariado %in% c("voluntario_actual", "voluntario_pasado", "cibervoluntarios")) |> 
  select(weight, starts_with("volu_razones_"), starts_with("volu_tipo_")) |> 
  mutate(across(everything(), ~replace_na(.x, 0)))

# 2. Separamos las matrices numéricas
matriz_razones <- df_cor |> select(starts_with("volu_razones_")) |> as.matrix()
matriz_tipos   <- df_cor |> select(starts_with("volu_tipo_")) |> as.matrix()
pesos          <- df_cor$weight

# 3. Calculamos la correlación ponderada
mat_correlacion <- wtd.cor(x = matriz_razones, y = matriz_tipos, weight = pesos)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro <- ifelse(abs(mat_correlacion) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres <- c(
  "volu_razones_tiempo"     = "Tener tiempo libre",
  "volu_razones_devolver"   = "Devolver a la sociedad",
  "volu_razones_brechas"    = "Reducir brechas/desigualdad",
  "volu_razones_convencido" = "Alguien me convenció",
  "volu_razones_util"       = "Sentirse útil",
  "volu_razones_conectar"   = "Conectar con otros",
  "volu_razones_ayudar"     = "Ayudar a los demás",
  
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

traducir_seguro <- function(nombres_originales) {
  # Buscamos cada nombre en el diccionario
  nombres_traducidos <- diccionario_nombres[nombres_originales]
  
  # Si alguna variable no estaba en el diccionario (da NA), simplemente le borramos el prefijo
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "volu_razones_|volu_tipo_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales)) # Devolvemos el vector limpio
}

# Aplicamos la función (esto garantiza que las dimensiones cuadren perfectamente)
rownames(mat_correlacion) <- traducir_seguro(rownames(mat_correlacion))
colnames(mat_correlacion) <- traducir_seguro(colnames(mat_correlacion))

rownames(mat_filtro) <- rownames(mat_correlacion)
colnames(mat_filtro) <- colnames(mat_correlacion)

# 5. Visualización con Corrplot

# --- TRUCO INFALIBLE PARA CORRPLOT ---
# 1. Localizamos la columna y la vaciamos para que corrplot no la pinte en negro
idx_ciber <- which(colnames(mat_correlacion) == "Cibervoluntarios")
colnames(mat_correlacion)[idx_ciber] <- ""

png("gráficas/corrplot_tipos_vs_razones.png", width = 11, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.8,       
  tl.col = "black",      # Volvemos a dejar el negro por defecto
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  p.mat = mat_filtro,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 3.5,             
  
  title = "Correlación entre Tipos de Voluntariado y Motivos",
  mar = c(0,0,4,0)           
)

# 2. Añadimos la etiqueta manualmente en las coordenadas matemáticas de corrplot
text(
  x = idx_ciber, 
  y = nrow(mat_correlacion) + 0.9, # Posición matemática exacta del eje superior
  labels = "Cibervoluntarios",
  col = "#B2182B", 
  font = 2,      # 2 = Negrita
  srt = 45,      # Misma rotación que tl.srt
  adj = c(0, 0), # Misma alineación que corrplot
  xpd = TRUE     # Permite escribir fuera de los márgenes
)

mtext("(Casillas grises indican relaciones no significativas)", side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()


# ==== CORRPLOT BENEFICIO PERCIBIDO X VOL TIPO ====

# 1. Filtramos y preparamos los datos
df_cor8 <- df |> 
  # Excluimos a los no voluntarios
  filter(perfil_voluntariado != "no_voluntario") |> 
  select(weight, starts_with("volu_beneficio_"), starts_with("volu_tipo_")) |> 
  # Al ser opciones de respuesta múltiple (0/1), rellenamos los NAs con 0
  mutate(across(everything(), ~replace_na(.x, 0)))

# 2. Separamos las matrices numéricas
matriz_beneficio <- df_cor8 |> select(starts_with("volu_beneficio_")) |> as.matrix()
matriz_tipos     <- df_cor8 |> select(starts_with("volu_tipo_")) |> as.matrix()
pesos8           <- df_cor8$weight

# 3. Calculamos la correlación ponderada (Phi para variables dummy)
mat_correlacion8 <- wtd.cor(x = matriz_beneficio, y = matriz_tipos, weight = pesos8)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro8 <- ifelse(abs(mat_correlacion8) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres8 <- c(
  # Beneficios del Voluntariado (Resumidos del Codebook)
  "volu_beneficio_desarrollo"     = "Desarrollo personal",
  "volu_beneficio_satisfaccion"   = "Satisfacción de ayudar",
  "volu_beneficio_economico"      = "Beneficios económicos",
  "volu_beneficio_util"           = "Tiempo libre útil",
  "volu_beneficio_reconocimiento" = "Reconocimiento social",
  "volu_beneficio_parte"          = "Sentirme parte de algo",
  "volu_beneficio_aprendizaje"    = "Aprendizaje personal",
  
  # Tipos de Voluntariado
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

traducir_seguro8 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres8[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "volu_beneficio_|volu_tipo_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion8) <- traducir_seguro8(rownames(mat_correlacion8))
colnames(mat_correlacion8) <- traducir_seguro8(colnames(mat_correlacion8))

rownames(mat_filtro8) <- rownames(mat_correlacion8)
colnames(mat_filtro8) <- colnames(mat_correlacion8)

# 5. Visualización con Corrplot
png("gráficas/corrplot_tipos_vs_beneficios.png", width = 11, height = 7, units = "in", res = 300)

corrplot(
  mat_correlacion8, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.8,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro8,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 3.5,             
  
  # TÍTULO PRINCIPAL
  title = "Correlación entre Tipos de Voluntariado y Beneficios Percibidos",
  mar = c(0,0,4,0)           
)

# SUBTÍTULO
mtext("(Casillas grises indican relaciones no significativas)", side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()

# ====
########### VOL NUNCA ###########
# ==== CORRPLOT RESP X VOL NUNCA ====
# 1. Filtramos y preparamos los datos
df_cor9 <- df |> 
  # Esta vez nos quedamos EXCLUSIVAMENTE con quienes nunca han sido voluntarios
  filter(perfil_voluntariado == "no_voluntario") |> 
  select(weight, starts_with("vol_nunca_"), starts_with("resp_")) |> 
  # Rellenamos NAs con 0 SOLO para las variables dummy de motivos
  mutate(across(starts_with("vol_nunca_"), ~replace_na(.x, 0))) |> 
  # Borramos NAs para no romper la escala Likert de responsabilidad
  drop_na()

# 2. Separamos las matrices numéricas
matriz_nunca <- df_cor9 |> select(starts_with("vol_nunca_")) |> as.matrix()
matriz_resp  <- df_cor9 |> select(starts_with("resp_")) |> as.matrix()
pesos9       <- df_cor9$weight

# 3. Calculamos la correlación ponderada (Punto-Biserial)
mat_correlacion9 <- wtd.cor(x = matriz_nunca, y = matriz_resp, weight = pesos9)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro9 <- ifelse(abs(mat_correlacion9) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres9 <- c(
  # Motivos por los que NUNCA ha hecho voluntariado (Resumidos)
  "vol_nunca_tiempo"        = "Falta de tiempo",
  "vol_nunca_otra_manera"   = "Colaboro de otra forma",
  "vol_nunca_no_necesidad"  = "No lo veo necesario",
  "vol_nunca_no_encontrado" = "No he encontrado dónde",
  "vol_nunca_no_planteado"  = "No me lo he planteado",
  
  # Atribución de Responsabilidad
  "resp_estado"      = "Estado / Gobiernos",
  "resp_empresa"     = "Empresas Tecnológicas",
  "resp_tercer"      = "Tercer Sector / ONG",
  "resp_ciudadania"  = "Ciudadanía",
  "resp_educativas"  = "Inst. Educativas"
)

traducir_seguro9 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres9[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "vol_nunca_|resp_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion9) <- traducir_seguro9(rownames(mat_correlacion9))
colnames(mat_correlacion9) <- traducir_seguro9(colnames(mat_correlacion9))

rownames(mat_filtro9) <- rownames(mat_correlacion9)
colnames(mat_filtro9) <- colnames(mat_correlacion9)

# 5. Visualización con Corrplot
png("gráficas/corrplot_nunca_vs_resp.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion9, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro9,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  # TÍTULO PRINCIPAL
  title = "Correlación entre Motivos de No Participación y Atribución de Responsabilidad",
  mar = c(0,0,4,0)           
)

# SUBTÍTULO
mtext("(Casillas grises indican relaciones no significativas)", side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()
# ==== CORRPLOT RESP TEC X VOL NUNCA ====

# 1. Filtramos y preparamos los datos
df_cor10 <- df |> 
  # Nos quedamos EXCLUSIVAMENTE con quienes nunca han sido voluntarios
  filter(perfil_voluntariado == "no_voluntario") |> 
  select(weight, starts_with("vol_nunca_"), starts_with("resptec_")) |> 
  # Rellenamos NAs con 0 SOLO para las variables dummy de motivos
  mutate(across(starts_with("vol_nunca_"), ~replace_na(.x, 0))) |> 
  # Borramos NAs para no romper la escala Likert de responsabilidad tecnológica
  drop_na()

# 2. Separamos las matrices numéricas
matriz_nunca   <- df_cor10 |> select(starts_with("vol_nunca_")) |> as.matrix()
matriz_resptec <- df_cor10 |> select(starts_with("resptec_")) |> as.matrix()
pesos10        <- df_cor10$weight

# 3. Calculamos la correlación ponderada (Punto-Biserial)
mat_correlacion10 <- wtd.cor(x = matriz_nunca, y = matriz_resptec, weight = pesos10)$correlation

# Filtro manual: si es <= 0.07, ponemos 1 (bloqueado), si no 0
mat_filtro10 <- ifelse(abs(mat_correlacion10) <= 0.07, 1, 0)

# ⬇️ 4. TRADUCCIÓN DINÁMICA A PRUEBA DE ERRORES ⬇️
diccionario_nombres10 <- c(
  # Motivos por los que NUNCA ha hecho voluntariado (Resumidos)
  "vol_nunca_tiempo"        = "Falta de tiempo",
  "vol_nunca_otra_manera"   = "Colaboro de otra forma",
  "vol_nunca_no_necesidad"  = "No lo veo necesario",
  "vol_nunca_no_encontrado" = "No he encontrado dónde",
  "vol_nunca_no_planteado"  = "No me lo he planteado",
  
  # Atribución de Responsabilidad Tecnológica
  "resptec_estado"      = "Estado / Gobiernos",
  "resptec_empresa"     = "Empresas Tecnológicas",
  "resptec_tercer"      = "Tercer Sector / ONG",
  "resptec_ciudadania"  = "Ciudadanía",
  "resptec_educativas"  = "Inst. Educativas"
)

traducir_seguro10 <- function(nombres_originales) {
  nombres_traducidos <- diccionario_nombres10[nombres_originales]
  
  nombres_finales <- ifelse(
    is.na(nombres_traducidos), 
    str_remove_all(nombres_originales, "vol_nunca_|resptec_"), 
    nombres_traducidos
  )
  
  return(unname(nombres_finales))
}

# Aplicamos la función traductora
rownames(mat_correlacion10) <- traducir_seguro10(rownames(mat_correlacion10))
colnames(mat_correlacion10) <- traducir_seguro10(colnames(mat_correlacion10))

rownames(mat_filtro10) <- rownames(mat_correlacion10)
colnames(mat_filtro10) <- colnames(mat_correlacion10)

# 5. Visualización con Corrplot
png("gráficas/corrplot_nunca_vs_resptec.png", width = 10, height = 6.5, units = "in", res = 300)

corrplot(
  mat_correlacion10, 
  method = "color",       
  is.corr = TRUE,         
  addCoef.col = "black",  
  number.cex = 0.9,       
  tl.col = "black",       
  tl.srt = 45,            
  col = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200), 
  
  # Filtro de significatividad manual
  p.mat = mat_filtro10,        
  sig.level = 0.5,           
  insig = "pch",             
  pch = 19,                  
  pch.col = "gray80",        
  pch.cex = 4,               
  
  # TÍTULO PRINCIPAL
  title = "Correlación entre Motivos de No Participación y Resp. Tecnológica",
  mar = c(0,0,4,0)           
)

# SUBTÍTULO
mtext("(Casillas grises indican relaciones no significativas)", side = 3, line = 0.5, cex = 0.9, col = "gray40")

dev.off()



# ====
# ==== STACKED BARPLOT BENEFICIO MER X VOL TIPO ====

# 1. Diccionario de tipos de voluntariado
diccionario_tipos <- c(
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling
df_beneficio_tipo <- df |> 
  # Excluir no voluntarios y NAs en la pregunta objetivo
  filter(
    perfil_voluntariado != "no_voluntario",
    !is.na(vol_beneficio)
  ) |> 
  select(weight, vol_beneficio, starts_with("volu_tipo_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("volu_tipo_"),
    names_to = "tipo_voluntariado",
    values_to = "participa"
  ) |> 
  
  # Nos quedamos SOLO con los que participan en ese tipo
  filter(participa == 1) |> 
  
  # Limpiamos las etiquetas
  mutate(
    vol_beneficio_clean = case_when(
      str_detect(vol_beneficio, "incluso") ~ "Sí, incluso económicos",
      str_detect(vol_beneficio, "no deben ser económicos") ~ "Sí, pero NO económicos",
      str_detect(vol_beneficio, "altruista") ~ "No, totalmente altruista",
      TRUE ~ "Otro"
    ),
    tipo_voluntariado = coalesce(diccionario_tipos[tipo_voluntariado], tipo_voluntariado)
  ) |> 
  
  # Calculamos porcentajes
  group_by(tipo_voluntariado, vol_beneficio_clean) |> 
  summarise(n_ponderado = sum(weight, na.rm = TRUE), .groups = "drop_last") |> 
  mutate(porcentaje = n_ponderado / sum(n_ponderado)) |> 
  ungroup() |> 
  
  # Ordenamos las categorías del stack
  mutate(
    vol_beneficio_clean = fct_relevel(
      vol_beneficio_clean,
      "No, totalmente altruista", 
      "Sí, pero NO económicos", 
      "Sí, incluso económicos"
    )
  )

# ⬇️ NUEVA LÓGICA DE ORDENACIÓN ⬇️
# Buscamos quién tiene más % de "Sí, incluso económicos"
orden_tipos <- df_beneficio_tipo |> 
  filter(vol_beneficio_clean == "Sí, incluso económicos") |> 
  # Arrange ascendente para que el más alto se coloque arriba en el ggplot
  arrange(porcentaje) |> 
  pull(tipo_voluntariado)

# Aplicamos ese orden al factor
df_beneficio_tipo <- df_beneficio_tipo |> 
  mutate(tipo_voluntariado = factor(tipo_voluntariado, levels = orden_tipos))

# 3. Visualización con ggplot2
png("gráficas/barplot_beneficios_vs_tipos.png", width = 11, height = 7, units = "in", res = 300)

ggplot(df_beneficio_tipo, aes(x = porcentaje, y = tipo_voluntariado, fill = vol_beneficio_clean)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.5) + 
  
  # Paleta Mako acotada como pediste para evitar extremos
  scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.2, end = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  
  labs(
    title = "¿Deberían las personas voluntarias obtener algún beneficio?",
    subtitle = "Porcentaje de respuesta desglosado por el tipo de voluntariado en el que participan",
    x = "Porcentaje de voluntarios dentro de cada tipo",
    y = NULL,
    fill = "Respuesta:"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

dev.off()

# ==== STACKED BARPLOT DAR RECIBIR X VOL TIPO ====

# 1. Diccionario de tipos de voluntariado
diccionario_tipos <- c(
  "volu_tipo_social"      = "Asistencial / Social",
  "volu_tipo_coopera"     = "Cooperación internacional",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sanitario / Salud",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario / Vecinal",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling
df_dar_tipo <- df |> 
  # Excluir no voluntarios y NAs en la pregunta objetivo
  filter(
    perfil_voluntariado != "no_voluntario",
    !is.na(volu_dar_recibir)
  ) |> 
  select(weight, volu_dar_recibir, starts_with("volu_tipo_")) |> 
  
  # Pivotamos a formato largo
  pivot_longer(
    cols = starts_with("volu_tipo_"),
    names_to = "tipo_voluntariado",
    values_to = "participa"
  ) |> 
  
  # Nos quedamos SOLO con los que participan en ese tipo
  filter(participa == 1) |> 
  
  # Limpiamos las etiquetas con saltos de línea automáticos
  mutate(
    volu_dar_recibir_clean = str_wrap(volu_dar_recibir, width = 30),
    tipo_voluntariado = coalesce(diccionario_tipos[tipo_voluntariado], tipo_voluntariado)
  ) |> 
  
  # Calculamos porcentajes ponderados
  group_by(tipo_voluntariado, volu_dar_recibir_clean) |> 
  summarise(n_ponderado = sum(weight, na.rm = TRUE), .groups = "drop_last") |> 
  mutate(porcentaje = n_ponderado / sum(n_ponderado)) |> 
  ungroup() 

# ⬇️ ORDENACIÓN FIJADA POR "RECIBES MÁS" ⬇️
# Filtramos los que contienen la frase "recibes más"
orden_tipos <- df_dar_tipo |> 
  filter(str_detect(str_to_lower(volu_dar_recibir_clean), "recibes más")) |> 
  arrange(porcentaje) |> 
  pull(tipo_voluntariado)

# Por seguridad (por si algún tipo de voluntariado tuviera un 0% en esa respuesta y desapareciera de la lista)
orden_completo <- c(setdiff(unique(df_dar_tipo$tipo_voluntariado), orden_tipos), orden_tipos)

# Aplicamos el orden al eje Y y forzamos a que esa respuesta quede al final de la leyenda (para que la escalera se vea bien)
df_dar_tipo <- df_dar_tipo |> 
  mutate(
    tipo_voluntariado = factor(tipo_voluntariado, levels = orden_completo),
    volu_dar_recibir_clean = fct_relevel(volu_dar_recibir_clean, 
                                         function(x) {
                                           c(x[!str_detect(str_to_lower(x), "recibes más")], 
                                             x[str_detect(str_to_lower(x), "recibes más")])
                                         })
  )

# --- VECTOR CONDICIONAL PARA NEGRITA ---
estilos_y <- ifelse(levels(df_dar_tipo$tipo_voluntariado) == "Cibervoluntarios", "bold", "plain")

# 3. Visualización con ggplot2
# 3. Visualización con ggplot2
png("gráficas/barplot_dar_recibir_vs_tipos.png", width = 11, height = 7, units = "in", res = 300)

ggplot(df_dar_tipo, aes(x = porcentaje, y = tipo_voluntariado, fill = volu_dar_recibir_clean)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.5) + 
  
  # ⬇️ AÑADIMOS LOS PORCENTAJES DENTRO DE LAS BARRAS ⬇️
  geom_text(
    # Ocultamos los menores al 4% para que no se superponga el texto en tramos pequeños
    aes(label = if_else(porcentaje > 0.04, percent(porcentaje, accuracy = 1), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  
  # Paleta Mako acotada
  scale_fill_viridis_d(option = "mako", direction = -1, begin = 0.2, end = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  
  labs(
    title = "En tu experiencia como persona voluntaria...",
    subtitle = "Porcentaje de respuesta desglosado por el tipo de voluntariado en el que participan",
    x = "Porcentaje de voluntarios dentro de cada tipo",
    y = NULL,
    fill = "Respuesta:"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = estilos_y)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

dev.off()
# ==== STACKED BARPLOT VOLU FREQ X EDAD ====
# 1. Preparación de Datos: Frecuencia de voluntariado por Grupo de Edad

datos_apilados_freq_edad <- df |> 
  # Filtramos a quienes respondieron la frecuencia y tienen edad registrada
  filter(
    !is.na(volu_freq),
    !is.na(grupo_edad),
    origen == "panel"
  ) |> 
  
  mutate(
    # Convertimos la frecuencia a factor ordenado de MENOR a MAYOR
    volu_freq = factor(volu_freq, levels = c(
      "Con menos frecuencia",
      "Al menos una vez al trimestre",
      "Al menos una vez al mes",
      "Al menos una vez a la semana"
    )),
    
    # Invertimos el grupo de edad para que los más jóvenes queden arriba en el gráfico
    grupo_edad = fct_rev(as.factor(grupo_edad))
  ) |> 
  
  # Calculamos la suma ponderada por edad y frecuencia
  group_by(grupo_edad, volu_freq) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last" # Deja agrupado por grupo_edad para el siguiente mutate
  ) |> 
  
  # Calculamos el % de cada frecuencia DENTRO de cada grupo de edad
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()


# 2. Visualización: Barras Apiladas 100%

p_freq_edad <- datos_apilados_freq_edad |> 
  ggplot(aes(x = porcentaje, y = grupo_edad, fill = volu_freq)) +
  
  # Barras apiladas con borde blanco para separar
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  # Texto con el porcentaje (ocultamos los menores al 3% para que no se amontonen)
  geom_text(
    aes(label = if_else(porcentaje > 3, paste0(round(porcentaje, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4,
    fontface = "bold"
  ) +
  
  # Paleta Mako (empieza más oscuro para las frecuencias bajas y claro para las altas, o al revés)
  scale_fill_viridis_d(
    option = "mako", 
    begin = 0.2, 
    end = 0.8,
    name = "Frecuencia de voluntariado"
  ) +
  
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Frecuencia de voluntariado según el grupo de edad",
    subtitle = "Distribución interna del tiempo dedicado en cada franja de edad",
    x = "Porcentaje dentro del grupo de edad",
    y = "Grupo de Edad"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background  = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor   = element_blank(),
    
    plot.title    = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y   = element_text(face = "bold", size = 11, color = "#34495e"),
    axis.text.x   = element_text(size = 10, color = "#7f8c8d"),
    
    # Leyenda arriba para que quede limpia
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 10)
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

# Mostramos el gráfico
print(p_freq_edad)

# 3. Guardado
ggsave(
  filename = "gráficas/barras_apiladas_freq_edad.png", 
  plot = p_freq_edad, 
  width = 11, 
  height = 6.5, 
  dpi = 300
)
# ==== STACKED BARPLOT VOLU FREQ X VOL TIPO ====
# 1. Diccionario de los tipos de voluntariado
diccionario_tipo_vol <- c(
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Ambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Socio-sanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling: Cruzar Respuesta Múltiple con Frecuencia
datos_apilados_tipo_freq <- df |> 
  # Nos quedamos con quienes han respondido a la frecuencia
  filter(!is.na(volu_freq)) |> 
  select(weight, volu_freq, starts_with("volu_tipo_")) |> 
  
  # Pivotamos las respuestas múltiples
  pivot_longer(
    cols = starts_with("volu_tipo_"),
    names_to = "tipo_voluntariado",
    values_to = "marcado"
  ) |> 
  
  # Nos quedamos SOLO con las filas donde la persona marcó ese tipo (valor == 1)
  filter(!is.na(marcado), marcado == 1) |> 
  
  mutate(
    # Traducimos el nombre
    tipo_clean = coalesce(diccionario_tipo_vol[tipo_voluntariado], tipo_voluntariado),
    
    # Ordenamos la frecuencia de menor a mayor dedicación
    volu_freq = factor(volu_freq, levels = c(
      "Con menos frecuencia",
      "Al menos una vez al trimestre",
      "Al menos una vez al mes",
      "Al menos una vez a la semana"
    ))
  ) |> 
  
  # Agrupamos para calcular porcentajes de frecuencia DENTRO de cada tipo
  group_by(tipo_clean, volu_freq) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) |> 
  
  # Calculamos el %
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()

# --- NUEVO TRUCO DE ORDENACIÓN: Por el stack de mayor frecuencia ---
orden_intensidad <- datos_apilados_tipo_freq |> 
  select(tipo_clean, volu_freq, porcentaje) |> 
  # Pivotamos para tener las frecuencias como columnas rellenando con 0 si no hay nadie
  pivot_wider(names_from = volu_freq, values_from = porcentaje, values_fill = 0) |> 
  # Ordenamos por la columna de mayor frecuencia (ascendente para que en ggplot quede arriba)
  arrange(`Al menos una vez a la semana`) |> 
  pull(tipo_clean)

# Aplicamos el orden al dataset final
datos_apilados_tipo_freq <- datos_apilados_tipo_freq |> 
  mutate(tipo_clean = factor(tipo_clean, levels = orden_intensidad))

# Vector condicional para negrita
estilos_y_freq <- ifelse(levels(datos_apilados_tipo_freq$tipo_clean) == "Cibervoluntarios", "bold", "plain")

# 3. Visualización: Barras Apiladas
p_tipo_freq <- ggplot(datos_apilados_tipo_freq, aes(x = porcentaje, y = tipo_clean, fill = volu_freq)) +
  
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(label = if_else(porcentaje > 4, paste0(round(porcentaje, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.8,
    fontface = "bold"
  ) +
  
  # Paleta Mako para indicar intensidad temporal
  scale_fill_viridis_d(
    option = "mako", 
    begin = 0.2, 
    end = 0.8,
    name = "Frecuencia"
  ) +
  
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Frecuencia de dedicación según el Tipo de Voluntariado",
    subtitle = "Ordenado de mayor a menor porcentaje de dedicación semanal",
    x = "Porcentaje dentro del tipo de voluntariado",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background  = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor   = element_blank(),
    
    plot.title    = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y   = element_text(face = estilos_y_freq, size = 11, color = "#34495e"),
    axis.text.x   = element_text(size = 10, color = "#7f8c8d"),
    
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 10)
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

print(p_tipo_freq)

# Guardamos
ggsave(
  filename = "gráficas/barras_apiladas_tipo_vs_freq.png", 
  plot = p_tipo_freq, 
  width = 11, 
  height = 7, 
  dpi = 300
)
# ==== STACKED BARPLOT ANTIGUEDAD X EDAD ====
# 1. Preparación de Datos: Antigüedad por Grupo de Edad

datos_apilados_antiguedad_edad <- df |> 
  filter(
    !is.na(volu_antiguedad),
    !is.na(grupo_edad),
    origen == "panel"
  ) |> 
  
  mutate(
    # ⬇️ EL CAMBIO: Le damos la vuelta al factor con fct_rev() para que se apile de izquierda a derecha
    volu_antiguedad = fct_rev(factor(volu_antiguedad, levels = c(
      "Menos de 12 meses",
      "1-2 años",
      "Menos de 5 años",
      "6-9 años",
      "10 años o más"
    ))),
    
    # Invertimos el grupo de edad para que los más jóvenes queden arriba en el gráfico
    grupo_edad = fct_rev(as.factor(grupo_edad))
  ) |> 
  
  group_by(grupo_edad, volu_antiguedad) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) |> 
  
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()


# 2. Visualización: Barras Apiladas 100%

p_antiguedad_edad <- datos_apilados_antiguedad_edad |> 
  ggplot(aes(x = porcentaje, y = grupo_edad, fill = volu_antiguedad)) +
  
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = if_else(porcentaje > 3, paste0(round(porcentaje, 1), "%"), ""),
      color = volu_antiguedad == "10 años o más",
      group = volu_antiguedad # ⬅️ ESTO ES LO QUE ARREGLA EL DESCENTRAMIENTO
    ),
    position = position_stack(vjust = 0.5),
    size = 4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white")) +
  
  scale_fill_viridis_d(
    option = "viridis", 
    begin = 0.1, 
    end = 0.9,
    direction = -1,
    name = "Antigüedad en el voluntariado"
  ) +
  
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Antigüedad en el voluntariado según el grupo de edad",
    subtitle = "Distribución interna del tiempo de permanencia en cada franja de edad",
    x = "Porcentaje dentro del grupo de edad",
    y = "Grupo de Edad"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background  = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor   = element_blank(),
    
    plot.title    = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y   = element_text(face = "bold", size = 11, color = "#34495e"),
    axis.text.x   = element_text(size = 10, color = "#7f8c8d"),
    
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 10)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

print(p_antiguedad_edad)

ggsave(
  filename = "gráficas/barras_apiladas_antiguedad_edad.png", 
  plot = p_antiguedad_edad, 
  width = 11, 
  height = 6.5, 
  dpi = 300
)
# ==== STACKED BARPLOT ANTIGUEDAD X VOL TIPO ====
# 1. Diccionario de los tipos de voluntariado
diccionario_tipo_vol <- c(
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Ambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Socio-sanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling: Cruzar Respuesta Múltiple con Antigüedad
datos_apilados_tipo_antig <- df |> 
  filter(!is.na(volu_antiguedad)) |> 
  select(weight, volu_antiguedad, starts_with("volu_tipo_")) |> 
  
  pivot_longer(
    cols = starts_with("volu_tipo_"),
    names_to = "tipo_voluntariado",
    values_to = "marcado"
  ) |> 
  
  filter(!is.na(marcado), marcado == 1) |> 
  
  mutate(
    tipo_clean = coalesce(diccionario_tipo_vol[tipo_voluntariado], tipo_voluntariado),
    
    volu_antiguedad = fct_rev(factor(volu_antiguedad, levels = c(
      "Menos de 12 meses",
      "1-2 años",
      "Menos de 5 años",
      "6-9 años",
      "10 años o más"
    )))
  ) |> 
  
  group_by(tipo_clean, volu_antiguedad) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop_last"
  ) |> 
  
  mutate(
    porcentaje = (total_ponderado / sum(total_ponderado)) * 100
  ) |> 
  ungroup()

# --- NUEVO TRUCO DE ORDENACIÓN: Por el stack de nuevas incorporaciones ---
orden_nuevos <- datos_apilados_tipo_antig |> 
  select(tipo_clean, volu_antiguedad, porcentaje) |> 
  pivot_wider(names_from = volu_antiguedad, values_from = porcentaje, values_fill = 0) |> 
  # ⬇️ ORDENAMOS POR "Menos de 12 meses" (Ascendente para que el mayor quede arriba)
  arrange(`Menos de 12 meses`) |> 
  pull(tipo_clean)

datos_apilados_tipo_antig <- datos_apilados_tipo_antig |> 
  mutate(tipo_clean = factor(tipo_clean, levels = orden_nuevos))

# 1. Creamos un vector condicional basado en los niveles del factor
estilos_y <- ifelse(levels(datos_apilados_tipo_antig$tipo_clean) == "Cibervoluntarios", "bold", "plain")

# 3. Visualización: Barras Apiladas
p_tipo_antiguedad <- ggplot(datos_apilados_tipo_antig, aes(x = porcentaje, y = tipo_clean, fill = volu_antiguedad)) +
  
  geom_col(width = 0.7, color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = if_else(porcentaje > 4, paste0(round(porcentaje, 1), "%"), ""),
      color = volu_antiguedad == "10 años o más",
      group = volu_antiguedad 
    ),
    position = position_stack(vjust = 0.5),
    size = 3.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white")) +
  
  scale_fill_viridis_d(
    option = "viridis", 
    begin = 0.1, 
    end = 0.9,
    direction = -1,
    name = "Antigüedad en el voluntariado"
  ) +
  
  scale_x_continuous(
    labels = \(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Antigüedad en el voluntariado según su tipología",
    subtitle = "Tipos de voluntariado ordenados por la mayor proporción de nuevas incorporaciones (< 12 meses)",
    x = "Porcentaje dentro del tipo de voluntariado",
    y = NULL
  ) +
  
  theme_minimal(base_family = "sans", base_size = 14) +
  theme(
    panel.background = element_rect(fill = "#fdfdfd", color = NA),
    plot.background  = element_rect(fill = "#fdfdfd", color = NA),
    
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor   = element_blank(),
    
    plot.title    = element_text(face = "bold", size = 15, color = "#2c3e50"),
    plot.subtitle = element_text(size = 11, color = "#7f8c8d", margin = margin(b = 15)),
    axis.text.y = element_text(face = estilos_y, size = 11, color = "#34495e"),
    axis.text.x = element_text(size = 10, color = "#7f8c8d"),
    
    legend.position = "top",
    legend.title    = element_text(face = "bold", size = 10)
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2, byrow = TRUE))

print(p_tipo_antiguedad)

# Guardamos
ggsave(
  filename = "gráficas/barras_apiladas_tipo_vs_antiguedad.png", 
  plot = p_tipo_antiguedad, 
  width = 11, 
  height = 7, 
  dpi = 300
)
# ====
# ==== HEATMAP VOLU FREQ X VOL TIPO ====
# 1. Diccionario de tipos de voluntariado (Ajusta los nombres a tu codebook)
diccionario_tipo <- c(
  "volu_tipo_social"      = "Social",
  "volu_tipo_desarrollo"  = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Ambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Socio-sanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Tecnológico"
)

# 2. Data Wrangling
df_heatmap_freq <- df |> 
  filter(!is.na(volu_freq)) |> 
  select(weight, volu_freq, starts_with("volu_tipo_"))

# A) Calculamos el TOTAL DE PERSONAS en cada frecuencia
base_freq <- df_heatmap_freq |> 
  group_by(volu_freq) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada tipo y sacamos el porcentaje real
df_heatmap_plot_freq <- df_heatmap_freq |> 
  pivot_longer(
    cols = starts_with("volu_tipo_"),
    names_to = "tipo",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(volu_freq, tipo) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE),
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # Calculamos la N real
    .groups = "drop"
  ) |> 
  
  left_join(base_freq, by = "volu_freq") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    tipo_clean = coalesce(diccionario_tipo[tipo], tipo),
    
    # FILTRO DINÁMICO: Ocultamos casillas con N < 15
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje)
  )

# ORDENACIÓN AUTOMÁTICA DEL EJE X (de más a menos popular)
orden_tipos <- df_heatmap_plot_freq |> 
  group_by(tipo_clean) |> 
  summarise(media_porcentaje = mean(porcentaje, na.rm = TRUE)) |> 
  arrange(desc(media_porcentaje)) |> 
  pull(tipo_clean)

df_heatmap_plot_freq <- df_heatmap_plot_freq |> 
  mutate(tipo_clean = factor(tipo_clean, levels = orden_tipos))

# 3. Visualización: Heatmap
png("gráficas/heatmap_tipo_vs_freq.png", width = 12, height = 6.5, units = "in", res = 300)

ggplot(df_heatmap_plot_freq, aes(x = tipo_clean, y = volu_freq, fill = porcentaje_plot)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = label_plot,
      color = replace_na(porcentaje_plot < 0.4, TRUE) 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" # Color gris para los NA
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Tipos de voluntariado según la frecuencia de dedicación",
    subtitle = "Porcentaje dentro de cada frecuencia (casillas grises excluidas por tener N < 15)",
    x = NULL,
    y = "Frecuencia",
    fill = "% de personas:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y = element_text(margin = margin(r = 15))
  )

dev.off()
# ====
# ==== HEATMAP EDAD X VOL NUNCA ====

# 1. Diccionario de motivos por los que nunca ha sido voluntario
diccionario_nunca <- c(
  "vol_nunca_tiempo"        = "Falta de tiempo",
  "vol_nunca_otra_manera"   = "Colaboro de otra forma",
  "vol_nunca_no_necesidad"  = "No lo veo necesario",
  "vol_nunca_no_encontrado" = "No he encontrado dónde",
  "vol_nunca_no_planteado"  = "No me lo he planteado"
)

# 2. Data Wrangling
df_heatmap_edad <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  filter(!is.na(grupo_edad)) |> 
  select(weight, grupo_edad, starts_with("vol_nunca_"))

# A) Calculamos el TOTAL DE PERSONAS en cada grupo de edad
base_edad <- df_heatmap_edad |> 
  group_by(grupo_edad) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada motivo y sacamos el porcentaje real
df_heatmap_plot <- df_heatmap_edad |> 
  pivot_longer(
    cols = starts_with("vol_nunca_"),
    names_to = "motivo",
    values_to = "marcado"
  ) |> 
  
  group_by(grupo_edad, motivo) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE), 
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # ⬅️ Calculamos la N real
    .groups = "drop"
  ) |> 
  
  left_join(base_edad, by = "grupo_edad") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    motivo_clean = coalesce(diccionario_nunca[motivo], motivo),
    
    # Invertimos para que los más jóvenes queden arriba en el gráfico
    grupo_edad = fct_rev(as.factor(grupo_edad)),
    
    # ORDENAMOS EL EJE X MANUALMENTE
    motivo_clean = factor(motivo_clean, levels = c(
      "Falta de tiempo",
      "No me lo he planteado",
      "No he encontrado dónde",
      "Colaboro de otra forma",
      "No lo veo necesario"
    )),
    
    # ⬇️ FILTRO DINÁMICO: Ocultamos casillas con N < 15 ⬇️
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje)
  )

# 3. Visualización: Heatmap
png("gráficas/heatmap_nunca_vs_edad.png", width = 10, height = 6.5, units = "in", res = 300)

ggplot(df_heatmap_plot, aes(x = motivo_clean, y = grupo_edad, fill = porcentaje_plot)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Añadimos el texto del porcentaje adaptado al filtro
  geom_text(
    aes(
      label = label_plot,
      color = replace_na(porcentaje_plot < 0.4, TRUE) 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" # ⬅️ Color gris para los NA
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Motivos para NO hacer voluntariado según el grupo de edad",
    subtitle = "Porcentaje dentro de cada estrato (casillas grises excluidas por tener N < 15)",
    x = NULL,
    y = "Grupo de Edad",
    fill = "% de personas:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y = element_text(margin = margin(r = 15))
  )

dev.off()
# ==== HEATMAP TAMAÑO POB X VOL NUNCA ====

# 1. Diccionario de motivos por los que nunca ha sido voluntario
diccionario_nunca <- c(
  "vol_nunca_tiempo"        = "Falta de tiempo",
  "vol_nunca_otra_manera"   = "Colaboro de otra forma",
  "vol_nunca_no_necesidad"  = "No lo veo necesario",
  "vol_nunca_no_encontrado" = "No he encontrado dónde",
  "vol_nunca_no_planteado"  = "No me lo he planteado"
)

# 2. Data Wrangling (Usando 'tamaño_pob' y 'vol_nunca_')
df_heatmap_pob_nunca <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  filter(!is.na(tamaño_pob)) |> 
  mutate(
    # 1️⃣ AGRUPAMOS LAS TRES PRIMERAS CATEGORÍAS
    tamaño_pob = case_when(
      tamaño_pob %in% c("Menos de 2.000 personas", "De 2.000 a 5.000 personas", "De 5.000 a 10.000 personas") ~ "Menos de 10.000 personas",
      TRUE ~ as.character(tamaño_pob)
    )
  ) |> 
  select(weight, tamaño_pob, starts_with("vol_nunca_"))

# A) Calculamos el TOTAL DE PERSONAS en cada tamaño de población (ya agrupado)
base_pob_nunca <- df_heatmap_pob_nunca |> 
  group_by(tamaño_pob) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos personas ponderadas, N real (sin pesos) y porcentajes
df_heatmap_plot_pob_nunca <- df_heatmap_pob_nunca |> 
  pivot_longer(
    cols = starts_with("vol_nunca_"),
    names_to = "motivo",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(tamaño_pob, motivo) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE), 
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # 2️⃣ CALCULAMOS LA N REAL DE LA CASILLA
    .groups = "drop"
  ) |> 
  
  left_join(base_pob_nunca, by = "tamaño_pob") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    motivo_clean = coalesce(diccionario_nunca[motivo], motivo),
    
    # 3️⃣ APLICAMOS EL FILTRO DE N < 15 PARA OCULTAR CASILLAS
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje),
    
    # ORDENAMOS EL EJE Y (TAMAÑO POBLACIONAL) CON LOS NIVELES NUEVOS
    tamaño_pob = factor(tamaño_pob, levels = rev(c(
      "Más de 500.000 personas",
      "De 200.000 a 500.000 personas", 
      "De 50.000 a 200.000 personas",
      "De 10.000 a 50.000 personas", 
      "Menos de 10.000 personas"
    )))
  )

# ORDENACIÓN AUTOMÁTICA DEL EJE X (de más a menos popular)
orden_motivos_pob <- df_heatmap_plot_pob_nunca |> 
  group_by(motivo_clean) |> 
  summarise(media_porcentaje = mean(porcentaje, na.rm = TRUE)) |> 
  arrange(desc(media_porcentaje)) |> 
  pull(motivo_clean)

df_heatmap_plot_pob_nunca <- df_heatmap_plot_pob_nunca |> 
  mutate(motivo_clean = factor(motivo_clean, levels = orden_motivos_pob))

# 3. Visualización: Heatmap
png("gráficas/heatmap_nunca_vs_poblacion.png", width = 11, height = 7, units = "in", res = 300)

ggplot(df_heatmap_plot_pob_nunca, aes(x = motivo_clean, y = tamaño_pob, fill = porcentaje_plot)) + # Usamos porcentaje_plot
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Añadimos el texto del porcentaje adaptado
  geom_text(
    aes(
      label = label_plot,
      color = replace_na(porcentaje_plot < 0.4, TRUE) # replace_na salva el error con NAs
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" # 4️⃣ COLOR GRIS PARA LAS CASILLAS CON NA (N < 15)
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Motivos para NO hacer voluntariado según tamaño del municipio",
    subtitle = "Porcentaje dentro de cada estrato (casillas grises excluidas por tener N < 15)",
    x = NULL,
    y = "Tamaño de Población",
    fill = "% de personas:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)), 
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ==== HEATMAP SECTOR LAB X VOL NUNCA ====

# 1. Diccionario de motivos por los que nunca ha sido voluntario
diccionario_nunca <- c(
  "vol_nunca_tiempo"        = "Falta de tiempo",
  "vol_nunca_otra_manera"   = "Colaboro de otra forma",
  "vol_nunca_no_necesidad"  = "No lo veo necesario",
  "vol_nunca_no_encontrado" = "No he encontrado dónde",
  "vol_nunca_no_planteado"  = "No me lo he planteado"
)

# 2. Data Wrangling (Usando 'sector_lab')
df_heatmap_sector <- df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  filter(!is.na(sector_lab)) |> 
  select(weight, sector_lab, starts_with("vol_nunca_"))

# A) Calculamos el TOTAL DE PERSONAS en cada sector laboral
base_sector <- df_heatmap_sector |> 
  group_by(sector_lab) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada motivo y sacamos el porcentaje real
df_heatmap_plot <- df_heatmap_sector |> 
  pivot_longer(
    cols = starts_with("vol_nunca_"),
    names_to = "motivo",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(sector_lab, motivo) |> 
  summarise(n_marcan = sum(weight[marcado == 1], na.rm = TRUE), .groups = "drop") |> 
  
  left_join(base_sector, by = "sector_lab") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    motivo_clean = coalesce(diccionario_nunca[motivo], motivo),
    
    # Invertimos el factor para que se ordene alfabéticamente de arriba a abajo
    sector_lab = fct_rev(as.factor(sector_lab)),
    
    # ⬇️ ORDENAMOS EL EJE X MANUALMENTE ⬇️
    motivo_clean = factor(motivo_clean, levels = c(
      "Falta de tiempo",
      "No me lo he planteado",
      "No he encontrado dónde",
      "Colaboro de otra forma",
      "No lo veo necesario"
    ))
  )

# 3. Visualización: Heatmap
png("gráficas/heatmap_nunca_vs_sector.png", width = 11, height = 7, units = "in", res = 300)

ggplot(df_heatmap_plot, aes(x = motivo_clean, y = sector_lab, fill = porcentaje)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Añadimos el texto del porcentaje. Texto blanco si es BAJO (< 0.4)
  geom_text(
    aes(
      label = percent(porcentaje, accuracy = 1),
      color = porcentaje < 0.4 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = percent_format()
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Motivos para NO hacer voluntariado según el Sector Laboral",
    subtitle = "Porcentaje real de personas dentro de cada sector que marcó cada motivo",
    x = NULL,
    y = "Sector Laboral",
    fill = "% de personas:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)), # ⬅️ Aquí está el aire para la etiqueta
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ====
# ==== HEATMAP EDAD X VOL POSI ====

# 1. Diccionario de ámbitos de voluntariado deseados
diccionario_posi <- c(
  "posi_social"      = "Social",
  "posi_desarrollo"  = "Cooperación al desarrollo",
  "posi_ambiental"   = "Ambiental",
  "posi_cultural"    = "Cultural",
  "posi_deportivo"   = "Deportivo",
  "posi_educativo"   = "Educativo",
  "posi_sanitario"   = "Socio-sanitario",
  "posi_ocio"        = "Ocio y tiempo libre",
  "posi_comunitario" = "Comunitario",
  "posi_proteccion"  = "Protección civil",
  "posi_tecnologico" = "Tecnológico"
)

# 2. Data Wrangling (Usando 'grupo_edad')
df_heatmap_posi <- df |> 
  # Nos quedamos con los que responden a "Si pudieras hacer voluntariado..."
  filter(perfil_voluntariado %in% c("no_voluntario", "voluntario_pasado")) |> 
  filter(!is.na(grupo_edad)) |> 
  select(weight, grupo_edad, starts_with("posi_"))

# A) Calculamos el TOTAL DE PERSONAS en cada grupo de edad
base_edad_posi <- df_heatmap_posi |> 
  group_by(grupo_edad) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada ámbito y sacamos el porcentaje real
df_heatmap_plot_posi <- df_heatmap_posi |> 
  pivot_longer(
    cols = starts_with("posi_"),
    names_to = "ambito",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(grupo_edad, ambito) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE),
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # ⬅️ Calculamos la N real
    .groups = "drop"
  ) |> 
  
  left_join(base_edad_posi, by = "grupo_edad") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    ambito_clean = coalesce(diccionario_posi[ambito], ambito),
    
    # Invertimos para que los más jóvenes queden arriba en el gráfico
    grupo_edad = fct_rev(as.factor(grupo_edad)),
    
    # ⬇️ FILTRO DINÁMICO: Ocultamos casillas con N < 15 ⬇️
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje)
  )

# ⬇️ ORDENACIÓN AUTOMÁTICA DEL EJE X (de más a menos popular) ⬇️
orden_ambitos <- df_heatmap_plot_posi |> 
  group_by(ambito_clean) |> 
  summarise(media_porcentaje = mean(porcentaje, na.rm = TRUE)) |> 
  arrange(desc(media_porcentaje)) |> 
  pull(ambito_clean)

df_heatmap_plot_posi <- df_heatmap_plot_posi |> 
  mutate(ambito_clean = factor(ambito_clean, levels = orden_ambitos))


# 3. Visualización: Heatmap
# Ampliamos a width = 12 para acomodar bien las 11 variables
png("gráficas/heatmap_posibles_vs_edad.png", width = 12, height = 6.5, units = "in", res = 300)

ggplot(df_heatmap_plot_posi, aes(x = ambito_clean, y = grupo_edad, fill = porcentaje_plot)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Añadimos el texto del porcentaje adaptado al truco
  geom_text(
    aes(
      label = label_plot, 
      # replace_na evita errores lógicos al evaluar los NAs generados por el truco
      color = replace_na(porcentaje_plot < 0.4, TRUE) 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" # ⬅️ Aquí definimos el color gris para los NA creados
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Ámbitos de interés para hacer voluntariado según el grupo de edad",
    subtitle = "Porcentaje dentro de cada estrato (casillas grises excluidas por tener N < 15)", # ⬅️ Subtítulo actualizado
    x = NULL,
    y = "Grupo de Edad",
    fill = "% de interés:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)), 
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()

# ==== HEATMAP TAMAÑO POB X VOL POSI ====

# 1. Diccionario de ámbitos de voluntariado deseados
diccionario_posi <- c(
  "posi_social"      = "Social",
  "posi_desarrollo"  = "Cooperación al desarrollo",
  "posi_ambiental"   = "Ambiental",
  "posi_cultural"    = "Cultural",
  "posi_deportivo"   = "Deportivo",
  "posi_educativo"   = "Educativo",
  "posi_sanitario"   = "Socio-sanitario",
  "posi_ocio"        = "Ocio y tiempo libre",
  "posi_comunitario" = "Comunitario",
  "posi_proteccion"  = "Protección civil",
  "posi_tecnologico" = "Tecnológico"
)

# 2. Data Wrangling (Usando 'tamaño_pob')
df_heatmap_pob <- df |> 
  filter(perfil_voluntariado %in% c("no_voluntario", "voluntario_pasado")) |> 
  filter(!is.na(tamaño_pob)) |> 
  mutate(
    # AGRUPAMOS LAS TRES PRIMERAS CATEGORÍAS
    tamaño_pob = case_when(
      tamaño_pob %in% c("Menos de 2.000 personas", "De 2.000 a 5.000 personas", "De 5.000 a 10.000 personas") ~ "Menos de 10.000 personas",
      TRUE ~ as.character(tamaño_pob)
    )
  ) |> 
  select(weight, tamaño_pob, starts_with("posi_"))

# A) Calculamos el TOTAL DE PERSONAS en cada tamaño de población
base_pob <- df_heatmap_pob |> 
  group_by(tamaño_pob) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada ámbito y sacamos el porcentaje real
df_heatmap_plot_pob <- df_heatmap_pob |> 
  pivot_longer(
    cols = starts_with("posi_"),
    names_to = "ambito",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(tamaño_pob, ambito) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE),
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # Calculamos la N real
    .groups = "drop"
  ) |> 
  
  left_join(base_pob, by = "tamaño_pob") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    ambito_clean = coalesce(diccionario_posi[ambito], ambito),
    
    # ORDENAMOS EL EJE Y (TAMAÑO POBLACIONAL) CON LOS NIVELES NUEVOS
    tamaño_pob = factor(tamaño_pob, levels = rev(c(
      "Más de 500.000 personas",
      "De 200.000 a 500.000 personas", 
      "De 50.000 a 200.000 personas",
      "De 10.000 a 50.000 personas", 
      "Menos de 10.000 personas"
    ))),
    
    # FILTRO DINÁMICO: Ocultamos casillas con N < 15
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje)
  )

# ORDENACIÓN AUTOMÁTICA DEL EJE X (de más a menos popular)
orden_ambitos <- df_heatmap_plot_pob |> 
  group_by(ambito_clean) |> 
  summarise(media_porcentaje = mean(porcentaje, na.rm = TRUE)) |> 
  arrange(desc(media_porcentaje)) |> 
  pull(ambito_clean)

df_heatmap_plot_pob <- df_heatmap_plot_pob |> 
  mutate(ambito_clean = factor(ambito_clean, levels = orden_ambitos))


# 3. Visualización: Heatmap
png("gráficas/heatmap_posibles_vs_poblacion.png", width = 12, height = 6.5, units = "in", res = 300)

ggplot(df_heatmap_plot_pob, aes(x = ambito_clean, y = tamaño_pob, fill = porcentaje_plot)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Añadimos el texto del porcentaje adaptado al truco
  geom_text(
    aes(
      label = label_plot, 
      color = replace_na(porcentaje_plot < 0.4, TRUE) # Evita fallos con NAs
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" # Define el color para las variables excluidas (NA)
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Ámbitos de interés para hacer voluntariado según tamaño del municipio",
    subtitle = "Porcentaje dentro de cada estrato (casillas grises excluidas por tener N < 15)", # Subtítulo actualizado
    x = NULL,
    y = "Tamaño de Población",
    fill = "% de interés:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ==== HEATMAP SECTOR LAB X VOL POSI ====

# 1. Diccionario de ámbitos de voluntariado deseados
diccionario_posi <- c(
  "posi_social"      = "Social",
  "posi_desarrollo"  = "Cooperación al desarrollo",
  "posi_ambiental"   = "Ambiental",
  "posi_cultural"    = "Cultural",
  "posi_deportivo"   = "Deportivo",
  "posi_educativo"   = "Educativo",
  "posi_sanitario"   = "Socio-sanitario",
  "posi_ocio"        = "Ocio y tiempo libre",
  "posi_comunitario" = "Comunitario",
  "posi_proteccion"  = "Protección civil",
  "posi_tecnologico" = "Tecnológico"
)

# 2. Data Wrangling (Usando 'sector_lab')
df_heatmap_sector_posi <- df |> 
  filter(perfil_voluntariado %in% c("no_voluntario", "voluntario_pasado")) |> 
  filter(!is.na(sector_lab)) |> 
  select(weight, sector_lab, starts_with("posi_"))

# A) Calculamos el TOTAL DE PERSONAS en cada sector laboral
base_sector_posi <- df_heatmap_sector_posi |> 
  group_by(sector_lab) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada ámbito y sacamos el porcentaje real
df_heatmap_plot_sector_posi <- df_heatmap_sector_posi |> 
  pivot_longer(
    cols = starts_with("posi_"),
    names_to = "ambito",
    values_to = "marcado"
  ) |>
  
  group_by(sector_lab, ambito) |> 
  summarise(n_marcan = sum(weight[marcado == 1], na.rm = TRUE), .groups = "drop") |> 
  
  left_join(base_sector_posi, by = "sector_lab") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    ambito_clean = coalesce(diccionario_posi[ambito], ambito),
    
    # Invertimos el factor para que se ordene alfabéticamente de arriba a abajo
    sector_lab = fct_rev(as.factor(sector_lab))
  )

# ⬇️ ORDENACIÓN AUTOMÁTICA DEL EJE X (de más a menos popular) ⬇️
orden_ambitos_sector <- df_heatmap_plot_sector_posi |> 
  group_by(ambito_clean) |> 
  summarise(media_porcentaje = mean(porcentaje, na.rm = TRUE)) |> 
  arrange(desc(media_porcentaje)) |> 
  pull(ambito_clean)

df_heatmap_plot_sector_posi <- df_heatmap_plot_sector_posi |> 
  mutate(ambito_clean = factor(ambito_clean, levels = orden_ambitos_sector))


# 3. Visualización: Heatmap
# Ancho = 12 para dar aire a los 11 ámbitos, Alto = 7 para dar aire a los sectores
png("gráficas/heatmap_posibles_vs_sector.png", width = 12, height = 7, units = "in", res = 300)

ggplot(df_heatmap_plot_sector_posi, aes(x = ambito_clean, y = sector_lab, fill = porcentaje)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Añadimos el texto del porcentaje (contraste inteligente)
  geom_text(
    aes(
      label = percent(porcentaje, accuracy = 1),
      color = porcentaje < 0.4 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = percent_format()
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Ámbitos de interés para hacer voluntariado según el Sector Laboral",
    subtitle = "Porcentaje de personas no voluntarias interesadas en cada ámbito, desglosado por sector",
    x = NULL,
    y = "Sector Laboral",
    fill = "% de interés:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)), # Aire para el título del eje Y
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ====
# ==== HEATMAP EDAD X COLAB ====
# 1. Diccionario de formas alternativas de colaboración (colab_)
diccionario_colab <- c(
  "colab_donaciones" = "Donaciones al Tercer Sector",
  "colab_informal"   = "Ayuda informal por mi cuenta",
  "colab_trabajo"    = "Siento que ayudo con mi trabajo",
  "colab_activismo"  = "Activismo en redes/digital",
  "colab_no"         = "No colaboro de otra manera"
)

# 2. Data Wrangling
df_heatmap_colab <- df |> 
  # ⬇️ NUEVO FILTRO: Incluimos a no voluntarios y voluntarios pasados
  filter(perfil_voluntariado %in% c("no_voluntario", "voluntario_pasado")) |> 
  filter(!is.na(grupo_edad)) |> 
  select(weight, grupo_edad, starts_with("colab_"))

# A) Calculamos el TOTAL DE PERSONAS en cada grupo de edad
base_edad_colab <- df_heatmap_colab |> 
  group_by(grupo_edad) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada forma de colaboración y sacamos % real
df_heatmap_plot_colab <- df_heatmap_colab |> 
  pivot_longer(
    cols = starts_with("colab_"),
    names_to = "colaboracion",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(grupo_edad, colaboracion) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE), 
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # ⬅️ Calculamos la N real
    .groups = "drop"
  ) |> 
  
  left_join(base_edad_colab, by = "grupo_edad") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    colaboracion_clean = coalesce(diccionario_colab[colaboracion], colaboracion),
    grupo_edad = fct_rev(as.factor(grupo_edad))
  )

# ⬇️ EL TRUCO DEL GRIS (DINÁMICO) Y ORDEN MANUAL DEL EJE X ⬇️
df_heatmap_plot_colab <- df_heatmap_plot_colab |> 
  mutate(
    # Aplicamos el orden estricto que has pedido
    colaboracion_clean = factor(colaboracion_clean, levels = c(
      "No colaboro de otra manera", 
      "Ayuda informal por mi cuenta", 
      "Donaciones al Tercer Sector", 
      "Siento que ayudo con mi trabajo", 
      "Activismo en redes/digital"
    )),
    
    # Ocultamos casillas con N < 15
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje)
  )

# 3. Visualización: Heatmap
png("gráficas/heatmap_colab_vs_edad.png", width = 12, height = 6.5, units = "in", res = 300)

ggplot(df_heatmap_plot_colab, aes(x = colaboracion_clean, y = grupo_edad, fill = porcentaje_plot)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = label_plot,
      # replace_na evita errores lógicos al evaluar los NAs que acabamos de crear
      color = replace_na(porcentaje_plot < 0.4, TRUE)
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" # ⬅️ Aquí definimos el color exacto para los datos anulados (NA)
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Formas de colaboración alternativas según el grupo de edad",
    subtitle = "Porcentaje dentro de cada estrato (casillas grises excluidas por tener N < 15)", # ⬅️ Subtítulo actualizado
    x = NULL,
    y = "Grupo de Edad",
    fill = "% de participación:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 11, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)), 
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ==== HEATMAP TAMAÑO POB X COLAB ====
# 1. Diccionario de formas alternativas de colaboración (colab_)
diccionario_colab <- c(
  "colab_donaciones" = "Donaciones al Tercer Sector",
  "colab_informal"   = "Ayuda informal por mi cuenta",
  "colab_trabajo"    = "Siento que ayudo con mi trabajo",
  "colab_activismo"  = "Activismo en redes/digital",
  "colab_no"         = "No colaboro de otra manera"
)

# 2. Data Wrangling (Usando 'tamaño_pob')
df_heatmap_colab_pob <- df |> 
  # Mantenemos a no voluntarios y voluntarios pasados
  filter(perfil_voluntariado %in% c("no_voluntario", "voluntario_pasado")) |> 
  filter(!is.na(tamaño_pob)) |> 
  select(weight, tamaño_pob, starts_with("colab_")) |> 
  
  # ⬇️ APLICAMOS TU ORDEN: Las poblaciones mayores quedarán arriba en el gráfico
  mutate(
    tamaño_pob = factor(tamaño_pob, levels = rev(c(
      "Más de 500.000 personas",
      "De 200.000 a 500.000 personas", 
      "De 50.000 a 200.000 personas",
      "De 10.000 a 50.000 personas", 
      "De 5.000 a 10.000 personas", 
      "De 2.000 a 5.000 personas", 
      "Menos de 2.000 personas"
    )))
  )

# A) Calculamos el TOTAL DE PERSONAS en cada grupo de tamaño de población
base_pob_colab <- df_heatmap_colab_pob |> 
  group_by(tamaño_pob) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada forma de colaboración y sacamos % real
df_heatmap_plot_pob <- df_heatmap_colab_pob |> 
  pivot_longer(
    cols = starts_with("colab_"),
    names_to = "colaboracion",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(tamaño_pob, colaboracion) |> 
  summarise(n_marcan = sum(weight[marcado == 1], na.rm = TRUE), .groups = "drop") |> 
  
  left_join(base_pob_colab, by = "tamaño_pob") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    colaboracion_clean = coalesce(diccionario_colab[colaboracion], colaboracion)
  )

# ⬇️ ORDENACIÓN AUTOMÁTICA DEL EJE X (de más a menos popular) ⬇️
orden_colab_pob <- df_heatmap_plot_pob |> 
  group_by(colaboracion_clean) |> 
  summarise(media_porcentaje = mean(porcentaje, na.rm = TRUE)) |> 
  arrange(desc(media_porcentaje)) |> 
  pull(colaboracion_clean)

df_heatmap_plot_pob <- df_heatmap_plot_pob |> 
  mutate(colaboracion_clean = factor(colaboracion_clean, levels = orden_colab_pob))


# 3. Visualización: Heatmap
png("gráficas/heatmap_colab_vs_poblacion.png", width = 12, height = 7, units = "in", res = 300)

ggplot(df_heatmap_plot_pob, aes(x = colaboracion_clean, y = tamaño_pob, fill = porcentaje)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  # Texto con porcentaje normal (sin celdas grises)
  geom_text(
    aes(
      label = scales::percent(porcentaje, accuracy = 1),
      color = porcentaje < 0.4 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format()
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Formas de colaboración alternativas según el tamaño de población",
    subtitle = "Porcentaje de personas sin voluntariado activo que colaboran de otra manera",
    x = NULL,
    y = "Tamaño de la población",
    fill = "% de participación:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 11, color = "black", face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15)), 
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ====
# ==== HEATMAP RAZONES X VOL TIPO ====
# 1. Diccionario (Mantenemos el anterior)
diccionario_nombres <- c(
  "volu_razones_tiempo"     = "Tener tiempo libre",
  "volu_razones_devolver"   = "Devolver a la sociedad",
  "volu_razones_brechas"    = "Reducir brechas/desigualdad",
  "volu_razones_convencido" = "Alguien me convenció",
  "volu_razones_util"       = "Sentirse útil",
  "volu_razones_conectar"   = "Conectar con otros",
  "volu_razones_ayudar"     = "Ayudar a los demás",
  
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling con fila de TOTAL
df_base <- df |> 
  filter(perfil_voluntariado %in% c("voluntario_actual", "voluntario_pasado", "cibervoluntarios"))

# A) Calculamos las medias por grupo
resumen_grupos <- df_base |> 
  select(weight, starts_with("volu_tipo_"), starts_with("volu_razones_")) |> 
  pivot_longer(cols = starts_with("volu_tipo_"), names_to = "tipo", values_to = "marcado_tipo") |> 
  filter(!is.na(marcado_tipo), marcado_tipo == 1) |> 
  group_by(tipo) |> 
  summarise(across(starts_with("volu_razones_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE)), .groups = "drop") |> 
  mutate(tipo_clean = coalesce(diccionario_nombres[tipo], tipo)) |> 
  select(-tipo)

# B) Calculamos la media TOTAL (población completa)
resumen_total <- df_base |> 
  filter(perfil_voluntariado %in% c("voluntario_actual", "cibervoluntarios")) |> 
  summarise(across(starts_with("volu_razones_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE))) |> 
  mutate(tipo_clean = "TOTAL")

# C) Unimos y preparamos para el plot
df_heatmap_plot <- bind_rows(resumen_grupos, resumen_total) |> 
  pivot_longer(cols = starts_with("volu_razones_"), names_to = "razon", values_to = "porcentaje") |> 
  mutate(razon_clean = coalesce(diccionario_nombres[razon], razon))

# D) Ordenación de ejes
orden_razones <- df_heatmap_plot |> 
  filter(tipo_clean == "TOTAL") |> 
  arrange(desc(porcentaje)) |> 
  pull(razon_clean)

# Forzamos que TOTAL aparezca arriba del todo
df_heatmap_plot <- df_heatmap_plot |> 
  mutate(
    razon_clean = factor(razon_clean, levels = orden_razones),
    tipo_clean = factor(tipo_clean),
    tipo_clean = fct_relevel(tipo_clean, "TOTAL", after = Inf) # TOTAL quedará abajo si usas fct_rev luego, ajustamos:
  )

# 3. Estilos condicionales (TOTAL y Cibervoluntarios en negrita)
# Ordenamos los niveles alfabéticamente pero dejando TOTAL al principio o final
niveles_y <- sort(unique(df_heatmap_plot$tipo_clean))
niveles_y <- c("TOTAL", rev(sort(setdiff(unique(df_heatmap_plot$tipo_clean), "TOTAL")))) # TOTAL al final del vector (abajo en el plot)

df_heatmap_plot$tipo_clean <- factor(df_heatmap_plot$tipo_clean, levels = niveles_y)

estilos_y <- ifelse(
  levels(df_heatmap_plot$tipo_clean) %in% c("TOTAL", "Cibervoluntarios"), 
  "bold", 
  "plain"
)

# 4. Visualización
png("gráficas/heatmap_tipos_vs_razones.png", width = 11, height = 7.5, units = "in", res = 300)

ggplot(df_heatmap_plot, aes(x = razon_clean, y = tipo_clean, fill = porcentaje)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = percent(porcentaje, accuracy = 1),
      color = porcentaje < 0.4 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", direction = 1, begin = 0.1, end = 0.9, labels = percent_format()
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  labs(
    title = "Motivos para hacer voluntariado: Grupos vs. Total Población",
    subtitle = "La fila TOTAL representa la media de todos los voluntarios de la muestra",
    x = NULL, y = NULL, fill = "% de personas:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = estilos_y),
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ==== HEATMAP RAZONES X VOL TIPO DIF ====

# 1. Diccionario
diccionario_nombres <- c(
  "volu_razones_tiempo"     = "Tener tiempo libre",
  "volu_razones_devolver"   = "Devolver a la sociedad",
  "volu_razones_brechas"    = "Reducir brechas/desigualdad",
  "volu_razones_convencido" = "Alguien me convenció",
  "volu_razones_util"       = "Sentirse útil",
  "volu_razones_conectar"   = "Conectar con otros",
  "volu_razones_ayudar"     = "Ayudar a los demás",
  
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling
df_base <- df |> 
  filter(perfil_voluntariado %in% c("voluntario_actual", "cibervoluntarios"))

# EJECUTAMOS LOS TESTS
resultados_test <- test_doble_multi_chisq(df_base, "volu_tipo_", "volu_razones_")

# A) Medias por grupo
resumen_grupos <- df_base |> 
  select(weight, starts_with("volu_tipo_"), starts_with("volu_razones_")) |> 
  pivot_longer(cols = starts_with("volu_tipo_"), names_to = "tipo", values_to = "marcado_tipo") |> 
  filter(!is.na(marcado_tipo), marcado_tipo == 1) |> 
  group_by(tipo) |> 
  summarise(across(starts_with("volu_razones_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE)), .groups = "drop")

# B) Media de personas (Media General ponderada del total de la muestra)
resumen_media_tipos <- df_base |> 
  filter(perfil_voluntariado == "voluntario_actual") |> 
  summarise(across(starts_with("volu_razones_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE))) |> 
  pivot_longer(everything(), names_to = "razon", values_to = "porcentaje_base")

# C) Cruzamos datos y TESTS
df_diferencias <- resumen_grupos |> 
  pivot_longer(cols = starts_with("volu_razones_"), names_to = "razon", values_to = "porcentaje_grupo") |> 
  left_join(resumen_media_tipos, by = "razon") |> 
  left_join(resultados_test, by = c("tipo" = "variable_1", "razon" = "variable_2")) |> 
  mutate(
    diferencia = porcentaje_grupo - porcentaje_base,
    
    # Casillas no significativas en NA para pintar gris
    diferencia_plot = if_else(significativo == "No", NA_real_, diferencia),
    
    tipo_clean = coalesce(diccionario_nombres[tipo], tipo),
    razon_clean = coalesce(diccionario_nombres[razon], razon),
    
    texto_base = sprintf("%+d%%", round(diferencia * 100)),
    
    # Borramos texto si no es significativo
    label_texto = if_else(significativo == "No", "", texto_base)
  )

# D) Creamos la fila MEDIA GENERAL artificial
df_fila_total <- resumen_media_tipos |> 
  mutate(
    tipo_clean = "MEDIA GENERAL",
    razon_clean = coalesce(diccionario_nombres[razon], razon),
    diferencia_plot = NA_real_, 
    label_texto = percent(porcentaje_base, accuracy = 1)
  )

# E) Unimos todo
df_heatmap_diff <- bind_rows(df_diferencias, df_fila_total)

# F) Ordenación
orden_razones <- resumen_media_tipos |> 
  mutate(razon_clean = coalesce(diccionario_nombres[razon], razon)) |> 
  arrange(desc(porcentaje_base)) |> 
  pull(razon_clean)

niveles_y <- c("MEDIA GENERAL", rev(sort(unique(df_diferencias$tipo_clean))))

df_heatmap_diff <- df_heatmap_diff |> 
  mutate(
    razon_clean = factor(razon_clean, levels = orden_razones),
    tipo_clean = factor(tipo_clean, levels = niveles_y)
  )

# 3. Estilos condicionales
estilos_y <- ifelse(levels(df_heatmap_diff$tipo_clean) %in% c("Cibervoluntarios", "MEDIA GENERAL"), "bold", "plain")

# 4. Visualización
png("gráficas/heatmap_tipos_vs_razones_DIF.png", width = 11, height = 7.5, units = "in", res = 300)

ggplot(df_heatmap_diff, aes(x = razon_clean, y = tipo_clean, fill = diferencia_plot)) +
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = label_texto,
      color = replace_na(abs(diferencia_plot) > 0.08, FALSE) 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
    labels = percent_format(),
    na.value = "gray90" 
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  
  labs(
    title = "Motivos según el Tipo de Voluntariado",
    subtitle = "Diferencia en porcentaje respecto a la media general de personas (p > 0.05 en gris)",
    x = NULL, y = NULL, fill = "Diferencia vs Media:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = estilos_y),
    axis.text.x = element_text(size = 11, color = "black", face = "bold", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ==== HEATMAP BENEFICIOS X VOL TIPO DIF ====

# 1. Diccionario
diccionario_nombres <- c(
  "volu_beneficio_desarrollo"     = "Desarrollo personal",
  "volu_beneficio_satisfaccion"   = "Satisfacción de ayudar",
  "volu_beneficio_economico"      = "Beneficios económicos",
  "volu_beneficio_util"           = "Tiempo libre útil",
  "volu_beneficio_reconocimiento" = "Reconocimiento social",
  "volu_beneficio_parte"          = "Sentirme parte de algo",
  "volu_beneficio_aprendizaje"    = "Aprendizaje personal",
  
  "volu_tipo_social"      = "Social",
  "volu_tipo_coopera"     = "Cooperación al desarrollo",
  "volu_tipo_ambiental"   = "Medioambiental",
  "volu_tipo_cultural"    = "Cultural",
  "volu_tipo_deportivo"   = "Deportivo",
  "volu_tipo_educativo"   = "Educativo",
  "volu_tipo_sanitario"   = "Sociosanitario",
  "volu_tipo_ocio"        = "Ocio y tiempo libre",
  "volu_tipo_comunitario" = "Comunitario",
  "volu_tipo_proteccion"  = "Protección civil",
  "volu_tipo_tecnologico" = "Cibervoluntarios"
)

# 2. Data Wrangling
df_base <- df |> 
  filter(perfil_voluntariado %in% c("voluntario_actual", "cibervoluntarios"))

# ⬇️ EJECUTAMOS LOS TESTS
resultados_test <- test_doble_multi_chisq(df_base, "volu_tipo_", "volu_beneficio_")

# A) Medias por grupo
resumen_grupos <- df_base |> 
  select(weight, starts_with("volu_tipo_"), starts_with("volu_beneficio_")) |> 
  pivot_longer(cols = starts_with("volu_tipo_"), names_to = "tipo", values_to = "marcado_tipo") |> 
  filter(!is.na(marcado_tipo), marcado_tipo == 1) |> 
  group_by(tipo) |> 
  summarise(across(starts_with("volu_beneficio_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE)), .groups = "drop")

# ⬇️ B) Media de personas (Media General ponderada del total de la muestra)
resumen_media_tipos <- df_base |> 
  summarise(across(starts_with("volu_beneficio_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE))) |> 
  pivot_longer(everything(), names_to = "beneficio", values_to = "porcentaje_base")

# C) Cruzamos datos y TESTS
df_diferencias <- resumen_grupos |> 
  pivot_longer(cols = starts_with("volu_beneficio_"), names_to = "beneficio", values_to = "porcentaje_grupo") |> 
  left_join(resumen_media_tipos, by = "beneficio") |> 
  left_join(resultados_test, by = c("tipo" = "variable_1", "beneficio" = "variable_2")) |> 
  mutate(
    diferencia = porcentaje_grupo - porcentaje_base,
    
    # El color gris se activa convirtiendo el valor en NA
    diferencia_plot = if_else(significativo == "No", NA_real_, diferencia),
    
    tipo_clean = coalesce(diccionario_nombres[tipo], tipo),
    beneficio_clean = coalesce(diccionario_nombres[beneficio], beneficio),
    
    texto_base = sprintf("%+d%%", round(diferencia * 100)),
    
    # Si no es significativo, borramos el texto (queda la casilla gris vacía)
    label_texto = if_else(significativo == "No", "", texto_base)
  )

# D) Creamos la fila MEDIA GENERAL artificial
df_fila_total <- resumen_media_tipos |> 
  mutate(
    tipo_clean = "MEDIA GENERAL",
    beneficio_clean = coalesce(diccionario_nombres[beneficio], beneficio),
    diferencia_plot = NA_real_, 
    label_texto = percent(porcentaje_base, accuracy = 1)
  )

# E) Unimos todo
df_heatmap_diff <- bind_rows(df_diferencias, df_fila_total)

# F) Ordenación 
orden_beneficios <- resumen_media_tipos |> 
  mutate(beneficio_clean = coalesce(diccionario_nombres[beneficio], beneficio)) |> 
  arrange(desc(porcentaje_base)) |> 
  pull(beneficio_clean)

niveles_y <- c("MEDIA GENERAL", rev(sort(unique(df_diferencias$tipo_clean))))

df_heatmap_diff <- df_heatmap_diff |> 
  mutate(
    beneficio_clean = factor(beneficio_clean, levels = orden_beneficios),
    tipo_clean = factor(tipo_clean, levels = niveles_y)
  )

# 3. Estilos condicionales
estilos_y <- ifelse(levels(df_heatmap_diff$tipo_clean) %in% c("Cibervoluntarios", "MEDIA GENERAL"), "bold", "plain")

# 4. Visualización
png("gráficas/heatmap_tipos_vs_beneficios_DIF.png", width = 11, height = 7.5, units = "in", res = 300)

ggplot(df_heatmap_diff, aes(x = beneficio_clean, y = tipo_clean, fill = diferencia_plot)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = label_texto,
      color = replace_na(abs(diferencia_plot) > 0.08, FALSE) 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
    labels = percent_format(),
    na.value = "gray90" 
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  
  labs(
    title = "Beneficios percibidos según el Tipo de Voluntariado",
    subtitle = "Diferencia en pp respecto a la media general de personas (p > 0.05 en gris)",
    x = NULL, y = NULL, fill = "Diferencia vs Media:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    
    axis.text.y = element_text(size = 12, color = "black", face = estilos_y),
    axis.text.x = element_text(size = 11, color = "black", face = "bold", angle = 45, hjust = 1),
    legend.position = "right"
  )

dev.off()
# ==== TABLA VOLTEC PAST TIPO ====
# 1. Preparamos los datos
tabla_voltec <- df |> 
  # Quitamos los NA
  filter(!is.na(voltec_past_tipo)) |> 
  
  # Contamos sumando los pesos
  count(voltec_past_tipo, wt = weight, name = "n") |> 
  arrange(desc(n)) |> 
  
  # Calculamos el porcentaje y ELIMINAMOS la columna 'n'
  mutate(porcentaje = n / sum(n)) |> 
  select(-n) # ⬅️ Aquí eliminamos la columna de Personas

# 2. Le damos formato profesional con gt
tabla_formateada <- tabla_voltec |> 
  gt() |> 
  
  # Añadimos título y subtítulo
  tab_header(
    title = md("**¿Qué tipo de voluntariado han hecho antes los cibervoluntarios?**"),
    subtitle = "Distribución de las personas con experiencia previa"
  ) |> 
  
  # Renombramos las columnas
  cols_label(
    voltec_past_tipo = "Tipo de voluntariado",
    porcentaje = "% del total"
  ) |> 
  
  # Formateamos solo el porcentaje
  fmt_percent(columns = porcentaje, decimals = 1) |> 
  
  # Diseño elegante
  opt_row_striping() |> 
  opt_table_lines("none") |> 
  tab_options(
    heading.align = "left",
    column_labels.font.weight = "bold",
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    table_body.hlines.color = "#f2f2f2"
  ) |> 
  
  # Destacamos la columna del tipo de voluntariado en negrita
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = voltec_past_tipo)
  )

# Mostramos la tabla
tabla_formateada

# Guardado
gtsave(tabla_formateada, "gráficas/tabla_voltec_pasado_porcentajes.png", vwidth = 800)
# ====
# ==== BARPLOT DEJARLO ====
# 1. Diccionario
diccionario_dejarlo <- c(
  "vol_dejarlo_tiempo"        = "Falta de tiempo",
  "vol_dejarlo_agotamiento"   = "Agotamiento, desmotivación",
  "vol_dejarlo_diferencias"   = "Diferencias con la organización",
  "vol_dejarlo_desinteres"    = "Desinterés",
  "vol_dejarlo_llenaba"       = "No me llenaba"
)

# 2. Data Wrangling
df_barras <- df |> 
  filter(perfil_voluntariado == "voluntario_pasado") |> 
  select(weight, starts_with("vol_dejarlo_"))

# Total de personas en este grupo
total_personas <- sum(df_barras$weight, na.rm = TRUE)

# Calcular porcentajes
df_barras_plot <- df_barras |> 
  pivot_longer(
    cols = starts_with("vol_dejarlo_"),
    names_to = "motivo",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(motivo) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    motivo_clean = coalesce(diccionario_dejarlo[motivo], motivo)
  ) |> 
  
  # Ordenamos de mayor a menor para el gráfico
  arrange(desc(porcentaje)) |> 
  mutate(motivo_clean = factor(motivo_clean, levels = motivo_clean))

# 3. Visualización
png("gráficas/barplot_dejarlo.png", width = 10, height = 6.5, units = "in", res = 300)

ggplot(df_barras_plot, aes(x = motivo_clean, y = porcentaje, fill = porcentaje)) +
  geom_col(width = 0.7, color = "white") +
  
  # Etiquetas de datos sobre las barras
  geom_text(
    aes(label = scales::percent(porcentaje, accuracy = 1)),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(option = "mako", direction = 1, begin = 0.2, end = 0.8) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  # str_wrap divide la etiqueta larga
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  # Damos margen superior al eje Y para que los textos no se corten
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, max(df_barras_plot$porcentaje) * 1.15)) +
  
  labs(
    title = "Motivos para dejar el voluntariado",
    subtitle = "Porcentaje de voluntarios pasados que marcaron cada motivo",
    x = NULL,
    y = "% de voluntarios pasados",
    fill = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.x = element_text(size = 11, color = "black", face = "bold"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none"
  )

dev.off()
# ==== HEATMAP EDAD X DEJARLO ====
# 1. Diccionario exacto extraído del codebook
diccionario_dejarlo <- c(
  "vol_dejarlo_tiempo"        = "Falta de tiempo",
  "vol_dejarlo_agotamiento"   = "Agotamiento, desmotivación",
  "vol_dejarlo_diferencias"   = "Diferencias con la organización",
  "vol_dejarlo_desinteres"    = "Desinterés",
  "vol_dejarlo_llenaba"       = "No me llenaba"
)

# 2. Data Wrangling
df_heatmap_edad <- df |> 
  filter(perfil_voluntariado == "voluntario_pasado") |> 
  filter(!is.na(grupo_edad)) |> 
  select(weight, grupo_edad, starts_with("vol_dejarlo_"))

# A) Calculamos el TOTAL DE PERSONAS en cada grupo de edad
base_edad <- df_heatmap_edad |> 
  group_by(grupo_edad) |> 
  summarise(total_personas = sum(weight, na.rm = TRUE), .groups = "drop")

# B) Calculamos cuántas personas marcan cada motivo y sacamos el porcentaje real
df_heatmap_plot <- df_heatmap_edad |> 
  pivot_longer(
    cols = starts_with("vol_dejarlo_"),
    names_to = "motivo",
    values_to = "marcado"
  ) |> 
  mutate(marcado = replace_na(marcado, 0)) |> 
  
  group_by(grupo_edad, motivo) |> 
  summarise(
    n_marcan = sum(weight[marcado == 1], na.rm = TRUE),
    n_real_sin_pesos = sum(marcado == 1, na.rm = TRUE), # Calculamos la N real
    .groups = "drop"
  ) |> 
  
  left_join(base_edad, by = "grupo_edad") |> 
  mutate(
    porcentaje = n_marcan / total_personas,
    motivo_clean = coalesce(diccionario_dejarlo[motivo], motivo),
    
    # Invertimos para que los más jóvenes queden arriba en el gráfico
    grupo_edad = fct_rev(as.factor(grupo_edad)),
    
    # ORDENAMOS EL EJE X MANUALMENTE
    motivo_clean = factor(motivo_clean, levels = unname(diccionario_dejarlo)),
    
    # FILTRO DINÁMICO: Ocultamos casillas con N < 15
    label_plot = if_else(n_real_sin_pesos < 15, "", scales::percent(porcentaje, accuracy = 1)),
    porcentaje_plot = if_else(n_real_sin_pesos < 15, NA_real_, porcentaje)
  )

# 3. Visualización: Heatmap
png("gráficas/heatmap_dejarlo_vs_edad.png", width = 11, height = 7, units = "in", res = 300)

ggplot(df_heatmap_plot, aes(x = motivo_clean, y = grupo_edad, fill = porcentaje_plot)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  
  geom_text(
    aes(
      label = label_plot,
      color = replace_na(porcentaje_plot < 0.4, TRUE) 
    ), 
    size = 4.5, 
    fontface = "bold"
  ) +
  
  scale_fill_viridis_c(
    option = "mako", 
    direction = 1, 
    begin = 0.1, 
    end = 0.9, 
    labels = scales::percent_format(),
    na.value = "grey85" 
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") + 
  
  # str_wrap divide automáticamente la etiqueta larga en varias líneas
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  
  labs(
    title = "Motivos para dejar el voluntariado según el grupo de edad",
    subtitle = "Porcentaje dentro de cada estrato (casillas grises excluidas por tener N < 15)", 
    x = NULL,
    y = "Grupo de Edad",
    fill = "% de personas:"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    axis.text.y = element_text(size = 12, color = "black", face = "bold"),
    axis.text.x = element_text(size = 11, color = "black", angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y = element_text(margin = margin(r = 15))
  )

dev.off()
# ==== HEATMAP TAMAÑO POB X DEJARLO ====
# ====
# ==== PREDICTORES CIBERVOLUNTARIOS ====
# TODAS VARIABLES
slopes_ciber_grafico <- resultados_slopes_ciber |> 
  filter(p.value < 0.05, 
         !term %in% c("sector_lab", "comunidad_autonoma")) |>
  mutate(
    term_mod = case_when(
      term == "def_vol_transfor_social" ~ "def_vol_transfor_social (likert)",
      term %in% c("resptec_educativas", "resp_tec_educativas") ~ "resptec_educativas (likert)",
      term == "resp_empresa" ~ "resp_empresa (likert)",
      TRUE ~ paste0(term, " (binaria)")
    )
  ) |>
  arrange(desc(estimate)) |> 
  mutate(
    term_num = paste0(row_number(), ". ", term_mod),
    term_num = reorder(term_num, estimate), 
    signo = ifelse(estimate > 0, "positivo", "negativo"),
    etiqueta_pct = paste0(round(estimate * 100, 1), "%")
  )

ggplot(slopes_ciber_grafico, aes(x = estimate, y = term_num, fill = signo)) +
  geom_col(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = etiqueta_pct, 
                hjust = ifelse(estimate > 0, -0.2, 1.2)), 
            size = 3.5) +
  scale_fill_manual(values = c("positivo" = "steelblue", "negativo" = "indianred")) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title = "Principales predictores de ser cibervoluntario - Variables de opinión",
    subtitle = "Cambio promedio en la probabilidad de ser cibervoluntario al incrementar un punto (likert)\no al seleccionar una respuesta (binaria).",
    x = "Efecto Marginal Promedio (AME)",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = "gráficas/cibervoluntarios_predictores.png",
  width = 10, 
  height = 6, 
  dpi = 300
)

# SOLO DEMOGRÁFICAS

# 1. Crear la tabla con la nueva métrica y etiquetas ajustadas
resultados_demo <- tidy(modelo_logistico_demo) |> 
  filter(p.value < 0.05, term != "(Intercept)") |> 
  arrange(desc(estimate)) |> 
  mutate(
    term_mod = case_when(
      str_starts(term, "genero") ~ str_replace(term, "genero(.*)", "genero = \\1 (binaria)"),
      str_starts(term, "sector_lab") ~ str_replace(term, "sector_lab(.*)", "sector_lab = \\1 (categorial)"),
      str_starts(term, "comunidad_autonoma") ~ str_replace(term, "comunidad_autonoma(.*)", "comunidad_autonoma = \\1 (categorial)"),
      str_starts(term, "edad") ~ str_replace(term, "edad(.*)", "edad = \\1 (categorial)"),
      str_starts(term, "situacion_lab") ~ str_replace(term, "situacion_lab(.*)", "situacion_lab = \\1 (categorial)"),
      str_starts(term, "tamano_poblacion") ~ str_replace(term, "tamano_poblacion(.*)", "tamano_poblacion = \\1 (categorial)"),
      str_starts(term, "grupo_edadMás de 65 años") ~ str_replace(term, "grupo_edad(.*)", "grupo_edad = \\1 (categorial)"),
      TRUE ~ paste0(term, " (categorial)")
    ),
    efecto_max_prob = estimate / 4,
    term_num = paste0(row_number(), ". ", term_mod),
    term_num = reorder(term_num, efecto_max_prob),
    signo = ifelse(efecto_max_prob > 0, "positivo", "negativo"),
    etiqueta_pct = paste0(round(efecto_max_prob * 100, 1), "%")
  )

# 2. Generar el gráfico
ggplot(resultados_demo, aes(x = efecto_max_prob, y = term_num, fill = signo)) +
  geom_col(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = etiqueta_pct, 
                hjust = ifelse(efecto_max_prob > 0, -0.2, 1.2)), 
            size = 3.5) +
  scale_fill_manual(values = c("positivo" = "steelblue", "negativo" = "indianred")) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title = "Principales predictores de ser cibervoluntario - Variables demográficas",
    subtitle = "Efecto marginal máximo estimado sobre la probabilidad de ser cibervoluntario.",
    x = "Efecto Máximo en Probabilidad",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = "gráficas/cibervoluntarios_predictores_demo.png",
  width = 10, 
  height = 6, 
  dpi = 300
)


# ==== EFECTOS CIBERVOLUNTARIOS PSM ====
resultados_grafico_psm <- resultados_psm_ciber |> 
  filter(p.value < 0.05) |>
  mutate(
    term_mod = case_when(
      variable_target == "def_vol_transfor_social" ~ "def_vol_transfor_social (likert)",
      variable_target %in% c("resp_tec_educativas", "resptec_educativas") ~ "resptec_educativas (likert)",
      variable_target == "resp_empresa" ~ "resp_empresa (likert)",
      TRUE ~ paste0(variable_target, " (binaria)")
    )
  ) |>
  arrange(desc(estimate)) |> 
  mutate(
    term_num = paste0(row_number(), ". ", term_mod),
    term_num = reorder(term_num, estimate), 
    signo = ifelse(estimate > 0, "positivo", "negativo"),
    etiqueta_dinamica = ifelse(grepl("\\(binaria\\)", term_mod),
                               paste0(round(estimate * 100, 1), "%"),
                               as.character(round(estimate, 3)))
  )

ggplot(resultados_grafico_psm, aes(x = estimate, y = term_num, fill = signo)) +
  geom_col(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = etiqueta_dinamica, 
                hjust = ifelse(estimate > 0, -0.2, 1.2)), 
            size = 3.5) +
  scale_fill_manual(values = c("positivo" = "steelblue", "negativo" = "indianred")) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title = "Efecto causal de ser cibervoluntario (PSM)",
    subtitle = "Cambio en cada variable atribuible al cibervoluntariado\n(porcentaje para binarias, puntos de escala para likert).",
    x = "Efecto Estimado",
    y = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = "gráficas/cibervoluntarios_efectos.png",
  width = 10, 
  height = 6, 
  dpi = 300
)

# ==== Z LINEAS AFI X EDAD ====
local({
  
  datos_afi_tendencias <- df |> 
    # 1. Seleccionamos y filtramos incluyendo el perfil
    select(perfil_voluntariado, edad_num, weight, starts_with("afi_")) |> 
    filter(!is.na(edad_num), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado") |> 
    
    # 2. Pasamos las 6 columnas a formato largo
    pivot_longer(
      cols = starts_with("afi_"),
      names_to = "afirmacion",
      values_to = "puntuacion"
    ) |> 
    
    # Quitamos los NAs de las puntuaciones para que no rompan las medias
    filter(!is.na(puntuacion)) |> 
    
    # 3. Limpiamos los nombres (Afirmaciones y Perfiles)
    mutate(
      afirmacion = case_when(
        afirmacion == "afi_quedarse" ~ "Ha venido para quedarse",
        afirmacion == "afi_gente_atras" ~ "Está dejando gente atrás",
        afirmacion == "afi_juventud_adiccion" ~ "La juventud tiene adicción",
        afirmacion == "afi_oportunidades" ~ "Crea nuevas oportunidades",
        afirmacion == "afi_demasiado_pantallas" ~ "Demasiado tiempo en pantallas",
        afirmacion == "afi_reducir_brechas" ~ "Ayuda a reducir brechas",
        TRUE ~ afirmacion 
      ),
      # Preparamos las etiquetas del facet
      perfil_label = case_when(
        perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
        perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
        perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
        perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
      ),
      # Lo pasamos a factor para forzar el orden de los paneles de arriba a abajo
      perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
    ) |> 
    
    # 4. Calculamos la media ponderada (Añadimos perfil_label a la agrupación)
    group_by(perfil_label, edad_num, afirmacion) |> 
    summarise(
      media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- VISUALIZACIÓN ---
  
  p_tendencias_afi <- datos_afi_tendencias |> 
    ggplot(aes(x = edad_num, y = media_ponderada, color = afirmacion)) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # ⬇️ EL TRUCO MAGICO: Cortamos en 4 filas compartiendo el eje X ⬇️
    facet_grid(perfil_label ~ .) +
    
    scale_color_manual(values = paleta_pastel_moderna) +
    
    scale_x_continuous(
      breaks = c(21, 29.5, 39.5, 49.5, 59.5, 70),
      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ) +
    
    labs(
      title = "Evolución de las actitudes frente a la tecnología según la edad",
      subtitle = "Puntuación media ponderada comparando perfiles de voluntariado",
      x = "Tramo de Edad",
      y = "Puntuación Media",
      color = "Afirmación"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      axis.text = element_text(size = 10, color = "#34495e", face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_blank(), 
      legend.text = element_text(size = 9),
      
      # ⬇️ ESTILOS PARA LOS FACETS (Paneles) ⬇️
      strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), # Texto de la cabecera
      strip.background = element_rect(fill = "gray95", color = NA),           # Cesta gris clarita
      panel.spacing = unit(1, "lines")                                        # Aire entre paneles
    ) +
    guides(color = guide_legend(ncol = 3)) # 3 columnas quedan perfectas para 6 variables
  
  # Mostramos y exportamos
  print(p_tendencias_afi)
  
  ggsave(
    filename = "gráficas/old/z_lineas_tendencia_afi_edad_facets.png", 
    plot = p_tendencias_afi, 
    width = 10, 
    height = 11, # Subimos un poco la altura total para que los 4 bloques no se aplasten
    dpi = 300
  )
  
})


# ==== Z LINEAS BIENESTAR POR EDAD ====
local({
  # 1. Preparación de Datos: Formato Largo y Medias Ponderadas
  
  datos_bienestar_tendencias <- df |> 
    # Seleccionamos perfil_voluntariado, edad_num, peso y las 7 variables de bienestar
    select(perfil_voluntariado, edad_num, weight, starts_with("bienestar_")) |> 
    filter(!is.na(edad_num), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado") |> 
    
    # Pasamos las 7 columnas a formato largo
    pivot_longer(
      cols = starts_with("bienestar_"),
      names_to = "dimension",
      values_to = "puntuacion"
    ) |> 
    
    filter(!is.na(puntuacion)) |> 
    
    # Limpiamos los nombres para la leyenda y creamos la etiqueta del perfil
    mutate(
      dimension = case_when(
        dimension == "bienestar_satisfaccion" ~ "Satisfacción general",
        dimension == "bienestar_integridad"   ~ "Integridad",
        dimension == "bienestar_desarrollo"   ~ "Desarrollo personal",
        dimension == "bienestar_libertad"     ~ "Libertad / Autonomía",
        dimension == "bienestar_necesidades"  ~ "Necesidades cubiertas",
        dimension == "bienestar_pertenencia"  ~ "Sentido de pertenencia",
        dimension == "bienestar_agencia"      ~ "Agencia / Control",
        TRUE ~ dimension
      ),
      # Preparamos las etiquetas del facet
      perfil_label = case_when(
        perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
        perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
        perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
        perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
      ),
      # Factor con el orden invertido para que "Nunca" quede arriba del todo
      perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
    ) |> 
    
    # Calculamos la media ponderada por perfil, edad y dimensión de bienestar
    group_by(perfil_label, edad_num, dimension) |> 
    summarise(
      media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 2. Visualización: Gráfico de Múltiples Líneas en Facetas
  
  p_tendencias_bienestar <- datos_bienestar_tendencias |> 
    ggplot(aes(x = edad_num, y = media_ponderada, color = dimension)) +
    
    # Líneas gruesas y puntos para marcar los tramos exactos
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # ⬇️ EL TRUCO MAGICO: Cortamos en 4 filas compartiendo el eje X ⬇️
    facet_grid(perfil_label ~ .) +
    
    # Aplicamos nuestra paleta manual pastel de 7 colores
    scale_color_manual(values = paleta_bienestar) +
    
    # Configuramos el eje X con las marcas de clase y sus etiquetas reales
    scale_x_continuous(
      breaks = c(21, 29.5, 39.5, 49.5, 59.5, 70),
      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ) +
    scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
    
    labs(
      title = "Evolución de las dimensiones de bienestar según la edad",
      subtitle = "Puntuación media ponderada comparando perfiles de voluntariado",
      x = "Tramo de Edad",
      y = "% de acuerdo",
      color = "Dimensión de Bienestar" # Título de la leyenda
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      # Cuadrícula para seguir las líneas horizontales
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      axis.text = element_text(size = 10, color = "#34495e", face = "bold"),
      
      # Colocamos la leyenda abajo en varias columnas
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10), 
      legend.text = element_text(size = 9),
      
      # ⬇️ ESTILOS PARA LOS FACETS (Paneles) ⬇️
      strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
      strip.background = element_rect(fill = "gray90", color = NA),           
      panel.spacing = unit(1, "lines") 
    ) +
    
    # Organizamos la leyenda en 3 columnas para que los 7 ítems quepan perfectamente
    guides(color = guide_legend(ncol = 3, byrow = TRUE))
  
  # Mostramos y exportamos
  print(p_tendencias_bienestar)
  
  ggsave(
    filename = "gráficas/old/z_lineas_tendencia_bienestar_edad_facets.png",
    plot = p_tendencias_bienestar, 
    width = 10, 
    height = 11, # Aumentamos la altura para los 4 paneles
    dpi = 300
  )
})

# ==== Z LINEAS BIENESTAR TEC POR EDAD ====
local({
  
  # 1. Preparación de Datos: Formato Largo y Medias Ponderadas
  datos_bienestartec_tend <- df |> 
    # Seleccionamos perfil, edad_num, peso y las 5 variables de bienestar tecnológico
    select(perfil_voluntariado, edad_num, weight, starts_with("bienestartec_")) |> 
    filter(!is.na(edad_num), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado") |> 
    
    # Pasamos las 5 columnas a formato largo
    pivot_longer(
      cols = starts_with("bienestartec_"),
      names_to = "dimension_tec",
      values_to = "puntuacion"
    ) |> 
    
    filter(!is.na(puntuacion)) |> 
    
    # Limpiamos los nombres de dimensión y creamos los de perfil
    mutate(
      dimension_tec = case_when(
        dimension_tec == "bienestartec_competencias" ~ "Autosuficiencia y competencias",
        dimension_tec == "bienestartec_al_dia"       ~ "Acceso y actualización tecnológica",
        dimension_tec == "bienestartec_no_adicto"    ~ "Uso saludable (sin adicción)",
        dimension_tec == "bienestartec_conectado"    ~ "Conectado a otros y al mundo digital",
        dimension_tec == "bienestartec_critico"      ~ "Pensamiento crítico digital",
        TRUE ~ dimension_tec
      ),
      # Preparamos las etiquetas del facet
      perfil_label = case_when(
        perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
        perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
        perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
        perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
      ),
      # Factor con el orden invertido para que "Nunca" quede arriba del todo
      perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
    ) |> 
    
    # Calculamos la media ponderada por perfil, edad y dimensión
    group_by(perfil_label, edad_num, dimension_tec) |> 
    summarise(
      media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # 2. Visualización: Gráfico de Múltiples Líneas en Facetas
  p_tendencias_bienestartec <- datos_bienestartec_tend |> 
    ggplot(aes(x = edad_num, y = media_ponderada, color = dimension_tec)) +
    
    # Líneas gruesas y puntos para marcar los tramos exactos
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # ⬇️ EL TRUCO MÁGICO: Cortamos en 4 filas compartiendo el eje X ⬇️
    facet_grid(perfil_label ~ .) +
    
    # Aplicamos nuestra paleta manual de 5 colores
    scale_color_manual(values = paleta_bienestartec) +
    
    # Configuramos el eje X con las marcas de clase y sus etiquetas reales
    scale_x_continuous(
      breaks = c(21, 29.5, 39.5, 49.5, 59.5, 70),
      labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ) +
    scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
    
    labs(
      title = "Evolución del bienestar tecnológico según la edad",
      subtitle = "Puntuación media ponderada comparando perfiles de voluntariado",
      x = "Tramo de Edad",
      y = "% de acuerdo",
      color = "Dimensión Tecnológica" # Título de la leyenda
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      axis.text = element_text(size = 10, color = "#34495e", face = "bold"),
      
      # Leyenda abajo, bien espaciada
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10), 
      legend.text = element_text(size = 9),
      
      # ⬇️ ESTILOS PARA LOS FACETS (Paneles) ⬇️
      strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
      strip.background = element_rect(fill = "gray95", color = NA),           
      panel.spacing = unit(1, "lines") 
    ) +
    
    # Organizamos la leyenda en 2 filas para que respire bien
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  # Mostramos y exportamos
  print(p_tendencias_bienestartec)
  
  ggsave(
    filename = "gráficas/old/z_lineas_tendencia_bienestartec_edad_facets.png", 
    plot = p_tendencias_bienestartec, 
    width = 10, 
    height = 11, # Subimos a 11 para que entren las 4 filas perfectamente
    dpi = 300
  )
})

# ==== Z LINEAS AFI POR TAMAÑO POB ====
local({
  # 1. Preparación de Datos: Formato Largo y Medias Ponderadas
  datos_afi_pob <- df |> 
    # Seleccionamos también perfil_voluntariado
    select(perfil_voluntariado, tamaño_pob, weight, starts_with("afi_")) |> 
    filter(!is.na(tamaño_pob), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado") |> 
    
    mutate(
      # ⬇️ CAMBIO: Forzamos el orden INVERTIDO de los factores de población (de mayor a menor)
      tamaño_pob = factor(tamaño_pob, levels = c(
        "Más de 500.000 personas",
        "De 200.000 a 500.000 personas", 
        "De 50.000 a 200.000 personas",
        "De 10.000 a 50.000 personas", 
        "De 5.000 a 10.000 personas", 
        "De 2.000 a 5.000 personas", 
        "Menos de 2.000 personas"
      )),
      
      # Preparamos las etiquetas del facet de perfil
      perfil_label = case_when(
        perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
        perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
        perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
        perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
      ),
      # Factor con el orden para que "Nunca" quede arriba del todo
      perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
    ) |> 
    
    pivot_longer(
      cols = starts_with("afi_"),
      names_to = "afirmacion",
      values_to = "puntuacion"
    ) |> 
    
    filter(!is.na(puntuacion)) |> 
    
    mutate(
      afirmacion = case_when(
        afirmacion == "afi_quedarse" ~ "Ha venido para quedarse",
        afirmacion == "afi_gente_atras" ~ "Está dejando gente atrás",
        afirmacion == "afi_juventud_adiccion" ~ "La juventud tiene adicción",
        afirmacion == "afi_oportunidades" ~ "Crea nuevas oportunidades",
        afirmacion == "afi_demasiado_pantallas" ~ "Demasiado tiempo en pantallas",
        afirmacion == "afi_reducir_brechas" ~ "Ayuda a reducir brechas",
        TRUE ~ afirmacion 
      )
    ) |> 
    
    # Calculamos la media incluyendo el perfil
    group_by(perfil_label, tamaño_pob, afirmacion) |> 
    summarise(
      media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 2. Visualización: Líneas de Tendencia (Tamaño Población) en Facetas
  p_tendencias_afi_pob <- datos_afi_pob |> 
    ggplot(aes(x = tamaño_pob, y = media_ponderada, color = afirmacion, group = afirmacion)) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # ⬇️ EL TRUCO MAGICO: Cortamos en 4 filas ⬇️
    facet_grid(perfil_label ~ .) +
    
    scale_color_manual(values = paleta_pastel_moderna) +
    
    # Aplicamos tus etiquetas resumidas para el eje X 
    # (Al ser un vector con nombres, ggplot sabe mapearlas independientemente del orden visual)
    scale_x_discrete(
      labels = c(
        "Menos de 2.000 personas"       = "< 2.000",
        "De 2.000 a 5.000 personas"     = "2.000 - 5.000",
        "De 5.000 a 10.000 personas"    = "5.000 - 10.000",
        "De 10.000 a 50.000 personas"   = "10.000 - 50.000",
        "De 50.000 a 200.000 personas"  = "50.000 - 200.000",
        "De 200.000 a 500.000 personas" = "200.000 - 500.000",
        "Más de 500.000 personas"       = "+500.000"
      )
    ) +
    
    labs(
      title = "Actitudes frente a la tecnología según el tamaño del municipio",
      subtitle = "Puntuación media ponderada comparando perfiles de voluntariado",
      x = "Tamaño de Población",
      y = "Puntuación Media",
      color = "Afirmación"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      
      # Inclinamos los textos del eje X
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#34495e", face = "bold"),
      axis.text.y = element_text(size = 10, color = "#34495e", face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_blank(), 
      legend.text = element_text(size = 9),
      
      # ⬇️ ESTILOS PARA LOS FACETS ⬇️
      strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
      strip.background = element_rect(fill = "gray95", color = NA),           
      panel.spacing = unit(1, "lines") 
    ) +
    guides(color = guide_legend(ncol = 3)) # Ponemos 3 columnas para que cuadren las 6 afirmaciones bien
  
  # Mostramos y exportamos
  print(p_tendencias_afi_pob)
  
  ggsave(
    filename = "gráficas/old/z_lineas_tendencia_afi_poblacion_facets_invertido.png", 
    plot = p_tendencias_afi_pob, 
    width = 10, 
    height = 11.5, 
    dpi = 300
  )
})

# ==== Z LINEAS BIENESTAR POR TAMAÑO POB ====
local({
  # 1. Preparación de Datos: Bienestar por Tamaño de Población
  datos_bienestar_pob <- df |> 
    # Seleccionamos perfil_voluntariado, tamaño_pob, peso y las variables de bienestar
    select(perfil_voluntariado, tamaño_pob, weight, starts_with("bienestar_")) |> 
    filter(!is.na(tamaño_pob), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado") |> 
    
    mutate(
      # Forzamos el orden de los municipios (INVERTIDO: de mayor a menor)
      tamaño_pob = factor(tamaño_pob, levels = c(
        "Más de 500.000 personas",
        "De 200.000 a 500.000 personas", 
        "De 50.000 a 200.000 personas",
        "De 10.000 a 50.000 personas", 
        "De 5.000 a 10.000 personas", 
        "De 2.000 a 5.000 personas", 
        "Menos de 2.000 personas"
      )),
      
      # Preparamos las etiquetas del facet de perfil
      perfil_label = case_when(
        perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
        perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
        perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
        perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
      ),
      # Factor con el orden para que "Nunca" quede arriba del todo
      perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
    ) |> 
    
    # Pasamos las 7 columnas a formato largo
    pivot_longer(
      cols = starts_with("bienestar_"),
      names_to = "dimension",
      values_to = "puntuacion"
    ) |> 
    
    filter(!is.na(puntuacion)) |> 
    
    # Limpiamos los nombres para la leyenda
    mutate(
      dimension = case_when(
        dimension == "bienestar_satisfaccion" ~ "Satisfacción general",
        dimension == "bienestar_integridad"   ~ "Integridad",
        dimension == "bienestar_desarrollo"   ~ "Desarrollo personal",
        dimension == "bienestar_libertad"     ~ "Libertad / Autonomía",
        dimension == "bienestar_necesidades"  ~ "Necesidades cubiertas",
        dimension == "bienestar_pertenencia"  ~ "Sentido de pertenencia",
        dimension == "bienestar_agencia"      ~ "Agencia / Control",
        TRUE ~ dimension
      )
    ) |> 
    
    # Calculamos la media ponderada por perfil, tamaño de población y por dimensión
    group_by(perfil_label, tamaño_pob, dimension) |> 
    summarise(
      media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 2. Visualización: Líneas de Tendencia (Tamaño Población) en Facetas
  p_tendencias_bienestar_pob <- datos_bienestar_pob |> 
    ggplot(aes(x = tamaño_pob, y = media_ponderada, color = dimension, group = dimension)) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # ⬇️ EL TRUCO MÁGICO: Cortamos en 4 filas ⬇️
    facet_grid(perfil_label ~ .) +
    
    # Aplicamos nuestra paleta manual
    scale_color_manual(values = paleta_bienestar) +
    scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
    scale_x_discrete(
      labels = c(
        "Menos de 2.000 personas"       = "< 2.000",
        "De 2.000 a 5.000 personas"     = "2.000 - 5.000",
        "De 5.000 a 10.000 personas"    = "5.000 - 10.000",
        "De 10.000 a 50.000 personas"   = "10.000 - 50.000",
        "De 50.000 a 200.000 personas"  = "50.000 - 200.000",
        "De 200.000 a 500.000 personas" = "200.000 - 500.000",
        "Más de 500.000 personas"       = "+500.000"
      )
    ) +
    
    labs(
      title = "Dimensiones de bienestar según el tamaño del municipio",
      subtitle = "Puntuación media ponderada comparando perfiles de voluntariado",
      x = "Tamaño de Población",
      y = "% de acuerdo",
      color = "Dimensión de Bienestar"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      
      # Inclinamos los textos del eje X a 45 grados
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#34495e", face = "bold"),
      axis.text.y = element_text(size = 10, color = "#34495e", face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10), 
      legend.text = element_text(size = 9),
      
      # ⬇️ ESTILOS PARA LOS FACETS ⬇️
      strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
      strip.background = element_rect(fill = "gray95", color = NA),           
      panel.spacing = unit(1, "lines") 
    ) +
    
    guides(color = guide_legend(ncol = 3, byrow = TRUE))
  
  # Mostramos y exportamos
  print(p_tendencias_bienestar_pob)
  
  ggsave(
    filename = "gráficas/old/z_lineas_tendencia_bienestar_poblacion_facets_invertido.png", 
    plot = p_tendencias_bienestar_pob, 
    width = 10, 
    height = 11.5, # Ajustamos altura para las etiquetas a 45º y las facetas
    dpi = 300
  )
})

# ==== Z LINEAS BIENESTAR TEC POR TAMAÑO POB  ====
local({
  # 1. Preparación de Datos: Bienestar Tecnológico por Tamaño de Población
  datos_bienestartec_pob <- df |> 
    # Seleccionamos perfil, tamaño_pob, peso y las 5 variables de bienestar tecnológico
    select(perfil_voluntariado, tamaño_pob, weight, starts_with("bienestartec_")) |> 
    filter(!is.na(tamaño_pob), !is.na(perfil_voluntariado), perfil_voluntariado != "no_clasificado") |> 
    
    mutate(
      # Forzamos el orden de los municipios (INVERTIDO: de mayor a menor)
      tamaño_pob = factor(tamaño_pob, levels = c(
        "Más de 500.000 personas",
        "De 200.000 a 500.000 personas", 
        "De 50.000 a 200.000 personas",
        "De 10.000 a 50.000 personas", 
        "De 5.000 a 10.000 personas", 
        "De 2.000 a 5.000 personas", 
        "Menos de 2.000 personas"
      )),
      
      # Preparamos las etiquetas del facet de perfil
      perfil_label = case_when(
        perfil_voluntariado == "cibervoluntarios" ~ "Cibervoluntarios",
        perfil_voluntariado == "voluntario_actual" ~ "Voluntario actual",
        perfil_voluntariado == "voluntario_pasado" ~ "Voluntario pasado",
        perfil_voluntariado == "no_voluntario" ~ "Nunca ha sido voluntario"
      ),
      # Factor con el orden para que "Nunca" quede arriba del todo
      perfil_label = factor(perfil_label, levels = c("Nunca ha sido voluntario", "Voluntario pasado", "Voluntario actual", "Cibervoluntarios"))
    ) |> 
    
    # Pasamos a formato largo
    pivot_longer(
      cols = starts_with("bienestartec_"),
      names_to = "dimension_tec",
      values_to = "puntuacion"
    ) |> 
    
    filter(!is.na(puntuacion)) |> 
    
    # Aplicamos las etiquetas precisas y resumidas
    mutate(
      dimension_tec = case_when(
        dimension_tec == "bienestartec_competencias" ~ "Autosuficiencia y competencias",
        dimension_tec == "bienestartec_al_dia"       ~ "Acceso y actualización tecnológica",
        dimension_tec == "bienestartec_no_adicto"    ~ "Uso saludable (sin adicción)",
        dimension_tec == "bienestartec_conectado"    ~ "Integración y conexión digital",
        dimension_tec == "bienestartec_critico"      ~ "Pensamiento crítico digital",
        TRUE ~ dimension_tec
      )
    ) |> 
    
    # Calculamos la media ponderada incluyendo el perfil
    group_by(perfil_label, tamaño_pob, dimension_tec) |> 
    summarise(
      media_ponderada = weighted.mean(puntuacion, w = weight, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 2. Visualización: Líneas de Tendencia Tecnológica (Tamaño Población) en Facetas
  
  p_tendencias_bienestartec_pob <- datos_bienestartec_pob |> 
    # Recuerda: group = dimension_tec es vital para conectar los puntos de texto
    ggplot(aes(x = tamaño_pob, y = media_ponderada, color = dimension_tec, group = dimension_tec)) +
    
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    
    # ⬇️ EL TRUCO MÁGICO: Cortamos en 4 filas ⬇️
    facet_grid(perfil_label ~ .) +
    
    # Aplicamos la paleta pastel moderna de 5 colores
    scale_color_manual(values = paleta_bienestartec) +
    scale_y_continuous(labels = \(x) paste0(x * 100, "%")) +
    scale_x_discrete(
      labels = c(
        "Menos de 2.000 personas"       = "< 2.000",
        "De 2.000 a 5.000 personas"     = "2.000 - 5.000",
        "De 5.000 a 10.000 personas"    = "5.000 - 10.000",
        "De 10.000 a 50.000 personas"   = "10.000 - 50.000",
        "De 50.000 a 200.000 personas"  = "50.000 - 200.000",
        "De 200.000 a 500.000 personas" = "200.000 - 500.000",
        "Más de 500.000 personas"       = "+500.000"
      )
    ) +
    
    labs(
      title = "Bienestar tecnológico según el tamaño del municipio",
      subtitle = "Puntuación media ponderada comparando perfiles de voluntariado",
      x = "Tamaño de Población",
      y = "% de acuerdo",
      color = "Dimensión Tecnológica" 
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "#fdfdfd", color = NA),
      plot.background = element_rect(fill = "#fdfdfd", color = NA),
      
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d", margin = margin(b = 15)),
      
      # Textos del eje X inclinados para máxima legibilidad
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "#34495e", face = "bold"),
      axis.text.y = element_text(size = 10, color = "#34495e", face = "bold"),
      
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 10), 
      legend.text = element_text(size = 9),
      
      # ⬇️ ESTILOS PARA LOS FACETS ⬇️
      strip.text = element_text(face = "bold", size = 11, color = "#2c3e50"), 
      strip.background = element_rect(fill = "gray95", color = NA),           
      panel.spacing = unit(1, "lines") 
    ) +
    
    # Forzamos 2 filas para la leyenda
    guides(color = guide_legend(nrow = 2, byrow = TRUE))
  
  # Mostramos y exportamos
  print(p_tendencias_bienestartec_pob)
  
  ggsave(
    filename = "gráficas/old/z_lineas_tendencia_bienestartec_poblacion_facets_invertido.png", 
    plot = p_tendencias_bienestartec_pob, 
    width = 10, 
    height = 11.5, # Ajuste de altura para los textos a 45º y las facetas
    dpi = 300
  )
})

# ==== Z DIVERGENTES LIKERT ====
local({
  plot_likert_divergente <- function(data, prefijo, titulo, weights_var = "weight") {
    
    # 1. Preparar datos y calcular medias para el ordenamiento
    # Usamos el peso para que el orden sea representativo de la población
    df_long <- data |>
      select(weight, starts_with(prefijo)) |>
      pivot_longer(
        cols = starts_with(prefijo), 
        names_to = "pregunta_id", 
        values_to = "respuesta"
      ) |>
      filter(!is.na(respuesta)) |>
      # Calculamos la media ponderada por pregunta antes de limpiar el nombre
      group_by(pregunta_id) |>
      mutate(
        media_ponderada = weighted.mean(respuesta, w = .data[[weights_var]], na.rm = TRUE)
      ) |>
      ungroup() |>
      mutate(
        # Limpieza de etiquetas (consistente con tu código original)
        pregunta = str_remove(pregunta_id, prefijo),
        pregunta = str_replace_all(pregunta, "_", " "),
        pregunta = str_to_sentence(pregunta),
        # Ordenamos el factor: la media más alta tendrá el índice más alto (arriba en el eje Y)
        pregunta = fct_reorder(pregunta, media_ponderada),
        respuesta = as.character(respuesta)
      )
    
    # 2. Cálculo de proporciones ponderadas para el gráfico
    df_plot <- df_long |>
      group_by(pregunta, respuesta) |>
      summarise(n_w = sum(.data[[weights_var]]), .groups = "drop_last") |>
      mutate(
        pct = n_w / sum(n_w),
        pct_plot = if_else(respuesta %in% c("0", "1", "2"), -pct, pct)
      ) |>
      ungroup()
    
    # 3. Segmentación para el apilamiento divergente
    df_neg <- df_plot |> 
      filter(respuesta %in% c("0", "1", "2")) |>
      mutate(respuesta = factor(respuesta, levels = c("0", "1", "2")))
    
    df_pos <- df_plot |> 
      filter(respuesta %in% c("3", "4", "5")) |>
      mutate(respuesta = factor(respuesta, levels = c("5", "4", "3")))
    
    # 4. Construcción del gráfico (mantiene tu estética original)
    ggplot() +
      geom_col(data = df_neg, aes(x = pct_plot, y = pregunta, fill = respuesta), width = 0.7) +
      geom_col(data = df_pos, aes(x = pct_plot, y = pregunta, fill = respuesta), width = 0.7) +
      geom_vline(xintercept = 0, color = "gray30", linewidth = 0.8) +
      
      geom_text(
        data = df_neg,
        aes(x = pct_plot, y = pregunta, group = respuesta, 
            label = if_else(pct > 0.04, scales::percent(pct, accuracy = 1), "")),
        position = position_stack(vjust = 0.5),
        size = 3, 
        color = if_else(df_neg$respuesta == "0", "white", "black")
      ) +
      
      geom_text(
        data = df_pos,
        aes(x = pct_plot, y = pregunta, group = respuesta, 
            label = if_else(pct > 0.04, scales::percent(pct, accuracy = 1), "")),
        position = position_stack(vjust = 0.5),
        size = 3, 
        color = if_else(df_pos$respuesta == "5", "white", "black")
      ) +
      
      scale_fill_viridis_d(
        option = "mako",
        begin = 0.1, 
        end = 0.9,
        name = "Valoración:",
        limits = c("0", "1", "2", "3", "4", "5"),
        guide = guide_legend(nrow = 1) 
      ) +
      
      scale_x_continuous(labels = \(x) scales::percent(abs(x)), limits = c(-1, 1)) +
      labs(title = titulo, x = "Porcentaje de respuestas (ponderado)", y = NULL) +
      theme_minimal() +
      theme(
        legend.position = "top", 
        panel.grid.major.y = element_blank()
      )
  }
  
  
  # Generar los cuatro bloques
  etiquetas_actores <- c(
    "Estado" = "Estado / Adm. Pública",
    "Empresa" = "Empresas",
    "Tercer" = "Tercer Sector",
    "Ciudadania" = "Ciudadanía",
    "Educativas" = "Inst. Educativas"
  )
  
  # Afirmaciones sobre tecnología
  p_afi <- plot_likert_divergente(df, "afi_", "Afirmaciones sobre tecnología") + 
    scale_y_discrete(labels = c(
      "Quedarse" = "Han venido para quedarse",
      "Gente atras" = "Dejan a gente atrás",
      "Juventud adiccion" = "Juventud adicta",
      "Oportunidades" = "Generan nuevas oportunidades",
      "Demasiado pantallas" = "Exceso de tiempo en pantallas",
      "Reducir brechas" = "Ayudan a reducir brechas"
    ))
  
  # Definiciones de Voluntariado
  p_def <- plot_likert_divergente(df, "def_vol_", "Definiciones de Voluntariado") + 
    scale_y_discrete(labels = c(
      "Apoyo admin" = "Apoyo a la Administración",
      "Transfor social" = "Transformación social",
      "Organizacion" = "Organización ciudadana",
      "Ocio" = "Ocio / Tiempo libre",
      "Competencias" = "Adquisición de competencias"
    ))
  
  # Responsabilidades
  p_resp <- plot_likert_divergente(df, "resp_", "Responsabilidad sobre Bienestar Social") +
    scale_y_discrete(labels = etiquetas_actores)
  
  p_resptec <- plot_likert_divergente(df, "resptec_", "Responsabilidad sobre Bienestar Tecnológico") +
    scale_y_discrete(labels = etiquetas_actores)
  
  ggsave("gráficas/old/z_likert_afi.png", p_afi, width = 11, height = 6, bg = "#fdfdfd")
  ggsave("gráficas/old/z_likert_def_vol.png", p_def, width = 11, height = 6, bg = "#fdfdfd")
  ggsave("gráficas/old/z_likert_resp.png", p_resp, width = 11, height = 6, bg = "#fdfdfd")
  ggsave("gráficas/old/z_likert_resptec.png", p_resptec, width = 11, height = 6, bg = "#fdfdfd")
})

# ==== Z LOLLIPOPS BIENESTAR ====
local({
  vars_dummies <- list(
    bienestar = "bienestar_",
    bienestartec = "bienestartec_"
  )
  
  plot_lollipop_dummy <- function(data, prefijo, titulo) {
    
    # Procesamos los datos
    df_plot <- data |>
      select(starts_with(prefijo), weight) |> 
      summarise(across(-weight, \(x) weighted.mean(x, w = weight, na.rm = TRUE))) |>
      pivot_longer(cols = everything(), names_to = "variable", values_to = "prop") |>
      mutate(
        # Limpieza dinámica del nombre de la variable
        label_clean = str_remove(variable, prefijo),
        label_clean = str_replace_all(label_clean, "_", " "),
        label_clean = str_to_sentence(label_clean))
    
    ggplot(df_plot, aes(x = prop, y = fct_reorder(label_clean, prop))) +
      geom_segment(aes(x = 0, xend = prop, yend = label_clean), color = "gray90", linewidth = 1.2) +
      geom_point(color = "#1a5276", size = 3.5) +
      # Etiquetas de porcentaje
      geom_text(aes(label = percent(prop, accuracy = 1)), 
                hjust = -0.4, size = 3.5, fontface = "bold", color = "#1a5276") +
      scale_x_continuous(labels = percent_format(), limits = c(0, 1.1), expand = c(0, 0)) +
      labs(
        title = titulo,
        subtitle = "Porcentaje de encuestados que seleccionaron esta opción",
        x = NULL, 
        y = NULL
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "#fdfdfd", color = NA),
        plot.background = element_rect(fill = "#fdfdfd", color = NA),
        plot.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
        axis.text.y = element_text(size = 10, color = "#34495e")
      )
  }
  
  local({
    p1 <- plot_lollipop_dummy(df, "bienestar_", "Dimensiones del Bienestar")
    p2 <- plot_lollipop_dummy(df, "bienestartec_", "Bienestar y Tecnología")
    
    ggsave("gráficas/old/z_lollipop_bienestar.png", p1, width = 9, height = 5, bg = "#fdfdfd")
    ggsave("gráficas/old/z_lollipop_bienestartec.png", p2, width = 9, height = 5, bg = "#fdfdfd")
  })
})

# ==== Z TESTEO PIRÁMIDES ====

df_piramide <- df_cibervol |> 
  # Filtramos NAs y nos quedamos solo con Hombre/Mujer para la pirámide clásica
  filter(!is.na(edad), !is.na(genero), genero %in% c("Hombre", "Mujer")) |> 
  
  # Creamos los tramos de edad
  mutate(
    grupo_edad = case_when(
      edad >= 18 & edad <= 24 ~ "18-24 años",
      edad >= 25 & edad <= 34 ~ "25-34 años",
      edad >= 35 & edad <= 44 ~ "35-44 años",
      edad >= 45 & edad <= 54 ~ "45-54 años",
      edad >= 55 & edad <= 64 ~ "55-64 años",
      edad >= 65 ~ "65+ años",
      TRUE ~ NA_character_
    ),
    # Convertimos a factor para que se ordenen bien en el eje
    grupo_edad = factor(grupo_edad, levels = c(
      "18-24 años", "25-34 años", "35-44 años", 
      "45-54 años", "55-64 años", "65+ años"
    ))
  ) |> 
  
  # Contamos cuántas personas hay de cada edad y género
  count(grupo_edad, genero, name = "total") |> 
  # Calculamos porcentajes
  mutate(pct = total/sum(total)
  )


df_piramide_muestra <- df |> 
  filter(perfil_voluntariado == "cibervoluntarios") |> 
  # Filtramos NAs y nos quedamos solo con Hombre/Mujer para la pirámide clásica
  filter(!is.na(genero), genero %in% c("Hombre", "Mujer")) |> 
  
  # Contamos cuántas personas hay de cada edad y género
  group_by(grupo_edad, genero) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop" # Rompemos la agrupación interna
  ) |> 

  mutate(pct = total_ponderado/sum(total_ponderado)
  )

df_piramide_propia <- cuestionario_propia |> 
  # Filtramos NAs y nos quedamos solo con Hombre/Mujer para la pirámide clásica
  filter(!is.na(genero), genero %in% c("Hombre", "Mujer")) |> 
  
  # Contamos cuántas personas hay de cada edad y género
  group_by(grupo_edad, genero) |> 
  summarise(
    total_ponderado = sum(weight, na.rm = TRUE),
    .groups = "drop" # Rompemos la agrupación interna
  ) |> 
  
  mutate(pct = total_ponderado/sum(total_ponderado)
  )


setdiff(cuestionario_propia |> select(1:6),
        df |> 
          filter(origen == "propia") |> 
          select(-c(perfil_voluntariado, edad_num, origen)) |> 
          select(1:6))


# ==============================================================================

# ==============================================================================

# 1. CÁLCULO DE PROPORCIONES (MÉTODO OPTIMIZADO)
# Calculamos la media ponderada directamente en formato ancho para ahorrar 
# memoria, y pivotamos el resultado final para la gráfica.

df_heatmap_plot2 <- df |> 
  # Opcional: validar que no hay NAs en la variable de agrupación
  filter(!is.na(grupo_edad), perfil_voluntariado %in% c("no_voluntario")) |> 
  group_by(grupo_edad) |> 
  # La media ponderada de dummies (0/1) devuelve la proporción exacta
  summarise(
    across(
      starts_with("vol_nunca_"), 
      \(x) weighted.mean(x, w = weight, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  # Pivotamos la tabla resumen (solo contiene tantas filas como grupos de edad)
  pivot_longer(
    cols = starts_with("vol_nunca_"),
    names_to = "motivo",
    values_to = "porcentaje"
  ) |> 
  # Limpieza de etiquetas para visualización
  mutate(
    motivo = str_remove(motivo, "vol_nunca_"),
    motivo = str_replace_all(motivo, "_", " "),
    motivo = str_to_sentence(motivo),
    # Opcional: escalar a 0-100 si prefieres el formato %
    porcentaje = porcentaje * 100 
  )

df_heatmap_plot2 |> view()
