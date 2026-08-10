library(readr)
library(tidyverse)
library(survey)
library(broom)
library(janitor)
library(readxl)
library(gt)
library(scales)
library(viridis)
library(weights)
library(corrplot)
library(stringr)
library(webshot2)
library(scales)
library(srvyr)
library(quantreg)
library(ggtext)
library(marginaleffects)
library(glmnet)
library(MatchIt)
library(patchwork)

# ==== CARGA ====

cuestionario_panel <- read_excel("cuestionario_panel.xlsx", 
                                 skip = 2) |> 
  rename(comunidad_autonoma = comunidad_autónoma) |> 
  filter(!is.na(genero), !is.na(grupo_edad), !is.na(comunidad_autonoma), genero != "Otro"
         ) |> 
  mutate(
    sector_lab = if_else(sector_lab == "ONG", "Sector servicios", sector_lab)
  )
  
cuestionario_propia <- read_excel("cuestionario_propia.xlsx", 
                                  skip = 2) |> 
  rename(comunidad_autonoma = comunidad_autónoma) |> 
  filter(!is.na(genero), !is.na(grupo_edad), !is.na(comunidad_autonoma)) |> 
  mutate(volu_tipo_tecnologico = "Tecnológico",
         sector_lab = if_else(sector_lab == "ONG", "Sector servicios", sector_lab))
# ==== TABLA DE POBLACIÓN ====

df_poblacion <- 
  read_delim("56940.csv",
             delim = ";", escape_double = FALSE, trim_ws = TRUE,
             locale = locale(encoding = "Latin1",
                             decimal_mark = ",",
                             grouping_mark = ".")) |> 
  clean_names() |> 
  rename(
    comunidad_autonoma = comunidades_y_ciudades_autonomas,
    genero = sexo
  ) |> 
  mutate(edad_simple = parse_number(edad_simple)) |> 
  mutate(grupo_edad = case_when(
    edad_simple >= 18 & edad_simple <= 24 ~ "Entre 18 y 24 años",
    edad_simple >= 25 & edad_simple <= 34 ~ "Entre 25 y 34 años",
    edad_simple >= 35 & edad_simple <= 44 ~ "Entre 35 y 44 años",
    edad_simple >= 45 & edad_simple <= 54 ~ "Entre 45 y 54 años",
    edad_simple >= 55 & edad_simple <= 64 ~ "Entre 55 y 64 años",
    edad_simple >= 65 ~ "Más de 65 años",
    TRUE ~ "Menor de edad"),
    genero = case_when(
      genero == "Hombres" ~ "Hombre",
      genero == "Mujeres" ~ "Mujer",
      TRUE ~ genero
    ),
    comunidad_autonoma = str_remove(comunidad_autonoma, "^[0-9]{2} "),
    comunidad_autonoma = case_when(
      comunidad_autonoma == "Asturias, Principado de" ~ "Asturias",
      comunidad_autonoma == "Madrid, Comunidad de" ~ "Madrid",
      comunidad_autonoma == "Murcia, Región de" ~ "Murcia",
      comunidad_autonoma == "Navarra, Comunidad Foral de" ~ "Navarra",
      comunidad_autonoma == "Balears, Illes" ~ "Islas Baleares",
      comunidad_autonoma == "Canarias" ~ "Islas Canarias",
      comunidad_autonoma == "Castilla - La Mancha" ~ "Castilla-La Mancha",
      comunidad_autonoma == "Comunitat Valenciana" ~ "Comunidad Valenciana",
      comunidad_autonoma == "Rioja, La" ~ "La Rioja",
      TRUE ~ comunidad_autonoma
    )) |> 
  group_by(grupo_edad, comunidad_autonoma, genero) |> 
  summarise(total = sum(total, na.rm = TRUE)) |> 
  ungroup() |>
  filter(grupo_edad != "Menor de edad",
         !comunidad_autonoma %in% c("Ceuta", "Melilla"))
# ==== TABLA DE CIBERVOLUNTARIOS ====
df_cibervol <- read_excel("datos_todos_cibers.xlsx") |> rename(comunidad_autonoma = comunidad,
                                                               genero = sexo) |> 
  mutate(
    # Creamos la etiqueta de texto según el valor numérico
    grupo_edad = case_when(
      edad >= 18 & edad <= 24 ~ "Entre 18 y 24 años",
      edad >= 25 & edad <= 34 ~ "Entre 25 y 34 años",
      edad >= 35 & edad <= 44 ~ "Entre 35 y 44 años",
      edad >= 45 & edad <= 54 ~ "Entre 45 y 54 años",
      edad >= 55 & edad <= 64 ~ "Entre 55 y 64 años",
      edad >= 65 ~ "Más de 65 años",
      TRUE ~ NA_character_ # Por si hay valores vacíos o menores de 18
    ),
    
    # Lo convertimos a factor con el orden oficial
    grupo_edad = factor(grupo_edad, levels = c(
      "Entre 18 y 24 años", 
      "Entre 25 y 34 años", 
      "Entre 35 y 44 años", 
      "Entre 45 y 54 años", 
      "Entre 55 y 64 años", 
      "Más de 65 años"
    ))
  ) |> 
  mutate(
    comunidad_autonoma = case_when(
      str_detect(tolower(comunidad_autonoma), "andaluc") ~ "Andalucía",
      str_detect(tolower(comunidad_autonoma), "arag") ~ "Aragón",
      str_detect(tolower(comunidad_autonoma), "asturias") ~ "Asturias",
      str_detect(tolower(comunidad_autonoma), "balears|baleares") ~ "Islas Baleares",
      str_detect(tolower(comunidad_autonoma), "canarias") ~ "Islas Canarias",
      str_detect(tolower(comunidad_autonoma), "cantabria") ~ "Cantabria",
      str_detect(tolower(comunidad_autonoma), "mancha") ~ "Castilla-La Mancha",
      str_detect(tolower(comunidad_autonoma), "león|leon") ~ "Castilla y León",
      str_detect(tolower(comunidad_autonoma), "catalu|cataluny") ~ "Cataluña",
      str_detect(tolower(comunidad_autonoma), "valencian|valencia") ~ "Comunidad Valenciana",
      str_detect(tolower(comunidad_autonoma), "extremadura") ~ "Extremadura",
      str_detect(tolower(comunidad_autonoma), "galicia") ~ "Galicia",
      str_detect(tolower(comunidad_autonoma), "madrid") ~ "Madrid",
      str_detect(tolower(comunidad_autonoma), "murcia") ~ "Murcia",
      str_detect(tolower(comunidad_autonoma), "navarra") ~ "Navarra",
      str_detect(tolower(comunidad_autonoma), "vasco|euskadi") ~ "País Vasco",
      str_detect(tolower(comunidad_autonoma), "rioja") ~ "La Rioja",
      str_detect(tolower(comunidad_autonoma), "ceuta") ~ NA_character_,
      str_detect(tolower(comunidad_autonoma), "melilla") ~ NA_character_,
      
      # Si no detecta nada de lo anterior, deja lo que ya estaba escrito
      TRUE ~ NA_character_ 
    ),
    genero = case_when(
      str_detect(tolower(genero), "hombre") ~ "Hombre",
      str_detect(tolower(genero), "mujer") ~ "Mujer",
      TRUE ~ NA_character_ 
  )) |> 
  filter(!is.na(genero), !is.na(grupo_edad), !is.na(comunidad_autonoma))
  
# =======================

# ==== PONDERADO PROPIA ====

# FASE 0: LIMPIEZA DE SEGURIDAD
# Nos aseguramos de no tener NAs y de que todas las categorías de la muestra 
# existen realmente en la base de datos del censo de la fundación.
cuestionario_propia <- cuestionario_propia |> 
  filter(
    !is.na(genero), 
    !is.na(grupo_edad), 
    !is.na(comunidad_autonoma),
    genero %in% df_cibervol$genero,
    grupo_edad %in% df_cibervol$grupo_edad,
    comunidad_autonoma %in% df_cibervol$comunidad_autonoma
  )


# FASE 1: CALCULAR TARGETS (Censo Interno)
# Usamos count() y le decimos que la columna de resultados se llame "Freq"
targets_genero <- df_cibervol |> 
  filter(!is.na(genero)) |> 
  count(genero, name = "Freq")

targets_edad <- df_cibervol |> 
  filter(!is.na(grupo_edad)) |> 
  count(grupo_edad, name = "Freq")

targets_CCAA <- df_cibervol |> 
  filter(!is.na(comunidad_autonoma)) |> 
  count(comunidad_autonoma, name = "Freq")


# FASE 2: DISEÑO Y RAKE
# Objeto de diseño inicial para la muestra de la fundación
design_propia <- svydesign(
  ids = ~1, 
  data = cuestionario_propia, 
  weights = NULL
)

# Calibración (Rake)
raked_propia <- rake(
  design = design_propia,
  sample.margins = list(~genero, ~grupo_edad, ~comunidad_autonoma),
  population.margins = list(targets_genero, targets_edad, targets_CCAA),
  control = list(maxit = 50, epsilon = 1e-7)
)

# Recortar pesos extremos
# LÍMITES BAJOS: Un peso de 1 = "Esta persona se representa a sí misma"
# raked_propia <- trimWeights(
#   raked_propia, 
#   lower = 0.1,  # Como mínimo representa a un 10% de persona
#   upper = 50,   # Como máximo representará a 50 voluntarios
#   strict = FALSE
# )

# Añadir columna de pesos al dataframe original
cuestionario_propia$weight <- weights(raked_propia)

# NORMALIZACIÓN DE PESOS
cuestionario_propia <- cuestionario_propia |> 
  mutate(
    weight = weight / mean(weight, na.rm = TRUE)
  )


# FASE 3: COMPROBACIÓN
message("--- RESULTADOS PONDERACIÓN ---")
message("\nResumen estadístico de los pesos:")
summary(cuestionario_propia$weight)
# ==== PONDERADO PANEL ====

# Extraer targets del dataframe df_población
targets_genero <- as.data.frame(xtabs(~genero, data = df_poblacion))
targets_edad <- as.data.frame(xtabs(~grupo_edad, data = df_poblacion))
targets_CCAA <- as.data.frame(xtabs(~comunidad_autonoma, data = df_poblacion))

targets_genero <- df_poblacion |> group_by(genero) |> summarise(Freq = sum(total))
targets_edad <- df_poblacion |> group_by(grupo_edad) |> summarise(Freq = sum(total))
targets_CCAA <- df_poblacion |> group_by(comunidad_autonoma) |> summarise(Freq = sum(total))

# Objeto de diseño inicial
design_panel <- svydesign(
  ids = ~1, 
  data = cuestionario_panel, 
  weights = NULL
)
# Calibración
raked_panel <- rake(
  design = design_panel,
  sample.margins = list(~genero, ~grupo_edad, ~comunidad_autonoma), # Variables en el dataset
  population.margins = list(genero = targets_genero, 
                            grupo_edad = targets_edad,
                            comunidad_autonoma = targets_CCAA),
  control = list(maxit = 50, epsilon = 1e-7)
)
# Recortar pesos extremos
raked_panel <- trimWeights(
  raked_panel, 
  lower = 10000, 
  upper = 100000, 
  strict = FALSE
)
# Añadir columna de pesos al dataframe
cuestionario_panel$weight <- weights(raked_panel)

# NORMALIZACIÓN DE PESOS
cuestionario_panel <- cuestionario_panel |> 
  mutate(
    # Dividimos cada peso entre la media de su grupo
    weight = weight / mean(weight, na.rm = TRUE)
  )

# Verificación de consistencia
message("Suma total de pesos (debe coincidir con la población): ", sum(cuestionario_panel$weight))
summary(cuestionario_panel$weight)
# ==== PONDERADO GENERAL ====
design_df <- svydesign(ids = ~1, data = df, weights = ~weight)
# =======================

# ==== UNIÓN Y LIMPIEZA ====

df <- rbind(cuestionario_panel, cuestionario_propia) |> 
  mutate(across(
    starts_with(c("bienestar_", "bienestartec_", "vol_retos_", "colab_", "posi_", 
                  "volu_razones_", "volu_beneficio_", "elementos_", 
                  "vol_dejarlo", "vol_nunca", "volu_tipo")),
    \(x) if_else(!is.na(x), 1, 0)
  )) |> 
  mutate(across(
    c(starts_with("def_vol_"), starts_with("resp_"), starts_with("resptec_"), starts_with("afi_")),
    \(x) as.numeric(str_extract(as.character(x), "\\d"))
  )) |> 
  # Creamos la variable jerárquica 'perfil_voluntariado'
  mutate(
    perfil_voluntariado = case_when(
      vol_12meses == "Sí" & volu_tipo_tecnologico == 1 ~ "cibervoluntarios",
      vol_12meses == "Sí" & volu_tipo_tecnologico == 0 ~ "voluntario_actual",
      vol_12meses == "No" & vol_past == "Sí" ~ "voluntario_pasado",
      vol_12meses == "No" & vol_past == "No" ~ "no_voluntario",
      TRUE ~ "no_clasificado"
    ),
    # Convertimos a factor para mantener un orden lógico en tablas y gráficas
    perfil_voluntariado = fct_relevel(
      perfil_voluntariado, 
      "no_voluntario", "voluntario_pasado", "voluntario_actual", "cibervoluntarios"
    ),
    edad_num = case_when(
      grupo_edad == "Entre 18 y 24 años" ~ 21,
      grupo_edad == "Entre 25 y 34 años" ~ 29.5,
      grupo_edad == "Entre 35 y 44 años" ~ 39.5,
      grupo_edad == "Entre 45 y 54 años" ~ 49.5,
      grupo_edad == "Entre 55 y 64 años" ~ 59.5,
      str_detect(grupo_edad, "Más de 65")  ~ 70, # str_detect por si hay espacios extra
      TRUE ~ NA_real_
    ),
    tamaño_pob = factor(tamaño_pob, levels = c(
      "Menos de 2.000 personas", 
      "De 2.000 a 5.000 personas", 
      "De 5.000 a 10.000 personas",
      "De 10.000 a 50.000 personas", 
      "De 50.000 a 200.000 personas", 
      "De 200.000 a 500.000 personas",
      "Más de 500.000 personas")
    ),
    origen = if_else(is.na(tend_politica), "propia", "panel"),
    # acortamos una respuesta de sector laboral
    sector_lab = if_else(
      sector_lab == "Administración pública de la Administración General del Estado", 
      "Administración General del Estado", 
      sector_lab
    )
  ) |> 
  filter(!is.na(genero), 
         !is.na(grupo_edad), 
         !is.na(comunidad_autonoma),
         genero != "Otro",
         perfil_voluntariado != "no_clasificado") |> 
  mutate(
    perfil_voluntariado = as.factor(perfil_voluntariado),
    perfil_voluntariado = relevel(perfil_voluntariado, ref = "no_voluntario"),
    edad_18 = edad_num - 18
  )

df_no_voluntarios <- df |> 
  filter(perfil_voluntariado == "no_voluntario")

df_actuales <- df |> 
  filter(perfil_voluntariado %in% c("voluntario_actual", "cibervoluntarios"))

df_no_actuales <- df |> 
  filter(!perfil_voluntariado %in% c("voluntario_actual", "cibervoluntarios"))
# =======================


# ==== TABLA DE PERFILES ====
tabla_perfiles <- df |> 
  count(perfil_voluntariado, name = "n") |> 
  mutate(porcentaje = (n / sum(n)) * 100)
# =======================
# ==== TESTEO DEL PONDERADO ====

# 1. Sacamos las proporciones de las 3 realidades para la EDAD
# A) Censo real
comp_pob <- df_cibervol |> 
  filter(!is.na(grupo_edad)) |> 
  count(grupo_edad, name = "total") |> 
  mutate(origen = "1. Población Real", pct = total / sum(total))

# B) Muestra SIN pesos
comp_bruta <- df |> 
  filter(origen == "propia", !is.na(grupo_edad)) |> 
  count(grupo_edad, name = "total") |> 
  mutate(origen = "2. Muestra Bruta", pct = total / sum(total))

# C) Muestra PONDERADA
comp_pond <- cuestionario_propia |>  
  count(grupo_edad, wt = weight, name = "total") |> 
  mutate(origen = "3. Muestra Ponderada", pct = total / sum(total))

# 2. Unimos y pivotamos para ver la tabla comparativa
tabla_auditoria <- bind_rows(comp_pob, comp_bruta, comp_pond) |> 
  select(grupo_edad, origen, pct) |> 
  pivot_wider(names_from = origen, values_from = pct) |> 
  mutate(across(where(is.numeric), ~percent(.x, accuracy = 0.1)))

print(tabla_auditoria)


# 