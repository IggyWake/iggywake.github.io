source("voluntec_procesado.R")
source("voluntec_funciones.R")

# ==== PORCENTAJES NO VOLUNTARIOS ====

# TABLA VOL_NUNCA
df |> 
  filter(perfil_voluntariado == "no_voluntario") |> 
  summarise(across(starts_with("vol_nunca_"), \(x) weighted.mean(x, w = weight, na.rm = TRUE))) |> 
  View()

# RESTO TABLAS NO VOLUNTARIOS
df |> 
  filter(perfil_voluntariado %in% c("no_voluntario", "voluntario_pasado")) |> 
  summarise(across(c(starts_with("colab"), starts_with("posi")), \(x) weighted.mean(x, w = weight, na.rm = TRUE))) |> 
  View()


# ==== TESTS AFI X PERFIL VOL ====

# 1. Defines las variables
variables_afi <- c("afi_quedarse", "afi_gente_atras", "afi_juventud_adiccion",
                   "afi_oportunidades", "afi_demasiado_pantallas", "afi_reducir_brechas")

# 2. Ejecutas la función pasando el dataframe Y el vector de variables
tests_afi_perfil <- run_weighted_tests(df, variables_afi)

# 3. (Opcional) Lo exportas a CSV para tener el registro
write.csv(tests_afi_perfil, file = "tests_afi_perfil.csv", row.names = FALSE)

# ==== TESTS BIENESTAR X PERFIL VOL ====

# 1. Definimos las variables y los perfiles a testear contra la base
variables_bienestar <- c("bienestar_satisfaccion", "bienestar_integridad", 
                         "bienestar_desarrollo", "bienestar_libertad", 
                         "bienestar_necesidades", "bienestar_pertenencia", 
                         "bienestar_agencia")

# 2. Ejecutas la función pasando el dataframe Y el vector de variables
tests_bienestar_perfil <- run_weighted_tests(df, variables_bienestar)

# 3. (Opcional) Lo exportas a CSV para tener el registro
write.csv(tests_bienestar_perfil, file = "tests_bienestar_perfil.csv", row.names = FALSE)


# ==== TESTS BIENESTARTEC X PERFIL VOL ====
# 1. Definimos las variables de bienestar tecnológico
variables_bienestartec <- c(
  "bienestartec_competencias", 
  "bienestartec_al_dia", 
  "bienestartec_no_adicto", 
  "bienestartec_conectado", 
  "bienestartec_critico"
)

# 2. Ejecutamos la función pasando el dataframe y el nuevo vector
tests_bienestartec_perfil <- run_weighted_tests(df, variables_bienestartec)

# 3. Exportamos a CSV para tener el registro y revisarlo si lo necesitamos
write.csv(
  tests_bienestartec_perfil, 
  file = "tests_bienestartec_perfil.csv", 
  row.names = FALSE
)

# ==== TESTS RETOS X PERFIL VOL ====
# 1. Definimos las variables de los retos del voluntariado extraídas del codebook
variables_retos <- c(
  "vol_retos_compromiso", 
  "vol_retos_recursos", 
  "vol_retos_burocracia", 
  "vol_retos_independencia", 
  "vol_retos_nuevas_form", 
  "vol_retos_medios_com"
)

# 2. Ejecutamos la función pasando el dataframe y el vector de variables
# (Asegúrate de que la función run_weighted_tests sigue cargada en tu entorno)
tests_retos_perfil <- run_weighted_tests(df, variables_retos)


# ==== TESTS RESP X PERFIL VOL ====

# 1. Definimos las variables y los perfiles a testear contra la base
variables_resp <- c("resp_estado", "resp_empresa", "resp_tercer", 
                    "resp_ciudadania", "resp_educativas")

# 2. Ejecutas la función pasando el dataframe Y el vector de variables
tests_resp_perfil <- run_weighted_tests(df, variables_resp)

# 3. (Opcional) Lo exportas a CSV para tener el registro
write.csv(tests_resp_perfil, file = "tests_resp_perfil.csv", row.names = FALSE)


# ==== TESTS RESPTEC X PERFIL VOL ====

# 1. Definimos las variables y los perfiles a testear contra la base
variables_resptec <- c("resptec_estado", "resptec_empresa", "resptec_tercer", 
                    "resptec_ciudadania", "resptec_educativas")

# 2. Ejecutas la función pasando el dataframe Y el vector de variables
tests_resptec_perfil <- run_weighted_tests(df, variables_resptec)

# 3. (Opcional) Lo exportas a CSV para tener el registro
write.csv(tests_resptec_perfil, file = "tests_resptec_perfil.csv", row.names = FALSE)


# ====
# ==== TESTS CHISQ MULTIRESPUESTA ====
test_multi_chisq("grupo_edad", "vol_nunca_")
# no_encontrado y no_planteado SIGNIFICATIVOS

test_multi_chisq("tamaño_pob", "vol_nunca_")
# NO SIGNIFICATIVO

test_multi_chisq("grupo_edad", "colab_")
# colab_no SIGNIFICATIVO

test_multi_chisq("tamaño_pob", "colab_")
# NO SIGNIFICATIVO

test_multi_chisq("grupo_edad", "posi_")
# posi_social, posi_ambiental, posi_deportivo, posi_proteccion, posi_tecnologico 
# SIGNIFICATIVOS

test_multi_chisq("tamaño_pob", "posi_")
# NO SIGNIFICATIVO

test_multi_chisq("grupo_edad", "vol_nunca_")

test_multi_chisq("grupo_edad", "vol_nunca_")
test_multi_chisq("grupo_edad", "vol_nunca_")


# ==== TESTS CHISQ DOBLE MULTIRESPUESTA ====
test_doble_multi_chisq(df_actuales, "volu_tipo_", "volu_razones_")

test_doble_multi_chisq(df_actuales, "volu_tipo_", "volu_beneficio_")


tests_pareados <- test_pareado(df, prefijo1 = "resp_", prefijo2 = "resptec_", var_grupo = "perfil_voluntariado")

# ==== REGRESIÓN MÚLTIPLE ====
# Creamos el objeto de diseño con tus datos
diseno <- svydesign(ids = ~1, data = df, weights = ~weight)

# Hacemos una regresión donde predecimos una variable (ej. resp_estado)
# usando el perfil Y la edad a la vez (+)
modelo_controlado <- svyglm(resp_estado ~ perfil_voluntariado + grupo_edad, design = diseno)

# Vemos los resultados
summary(modelo_controlado)
# ==== REGRESIONES BINOMIALES ====

# funciones batch
regresion_binomial(df, "bienestar_", "edad_18 * perfil_voluntariado") |> 
  reg_tidy() |> 
  mutate(aumento_pct_maximo = (estimate / 4) * 100, .after = estimate) |> 
  filter(sig != "ns") |> 
  view()

regresion_binomial(df, "bienestartec_", "edad_18 * perfil_voluntariado") |> 
  reg_tidy() |> 
  mutate(aumento_pct_maximo = (estimate / 4) * 100, .after = estimate) |> 
  view()

# una única regresión binomial
diseno <- svydesign(ids = ~1, weights = ~weight, data = df)

modelo_svy <- svyglm(
  bienestartec_critico ~ edad_num * perfil_voluntariado,
  design = diseno,
  family = quasibinomial()
)

summary(modelo_svy)

# ==== REGRESIONES LINEALES ====

regresion_lineal(df, "afi_", "edad_num * perfil_voluntariado") |> 
  reg_tidy() |> prediccion_manual_lineal(afi_pantallas)


# ==== AVERAGE MARGINAL EFFECTS ====

# bienestar
variables_target <- df |> select(starts_with(c("bienestar_", "bienestartec_"))) |> names()

resultados <- ame_analysis(
  datos = df,
  targets = variables_target,
  var_efecto = "edad_num", 
  var_agrupacion = "perfil_voluntariado",
  puntos_prediccion = c(20, 40, 60),
  tipo_modelo = "binomial" 
)

resultados$resumen_final |> 
  arrange(perfil_voluntariado) |> 
  tabla_voluntec(titulo = "Efectos marginales medios", subtitulo = "Variación media 
                   de la probabilidad de elegir una respuesta por año de edad adicional") |> 
  gtsave(filename = "ame_bienestar.png")

# responsabilidad
variables_target <- df |> select(starts_with(c("resp_", "resptec_"))) |> names()

resultados <- ame_analysis(
  datos = df,
  targets = variables_target,
  var_efecto = "edad_num", 
  var_agrupacion = "perfil_voluntariado",
  puntos_prediccion = c(20, 40, 60),
  tipo_modelo = "gaussian" 
)



# ====
# ==== REGRESIÓN LOGÍSTICA CIBERVOLUNTARIOS ====

# TODAS VARIABLES GENERALES
df_reg <- df |>
  mutate(es_ciber = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)) |>
  filter(sector_lab != "Construcción") |> 
  select(-c(perfil_voluntariado, tend_politica, vol_12meses)) |> 
  select(c(def_vol_apoyo_admin:resptec_educativas, es_ciber, weight)) |> 
  drop_na() |>
  droplevels()

modelo_logistico <- glm(es_ciber ~ ., data = df_reg, family = binomial(), weights = weight)
summary(modelo_logistico)
tidy(modelo_logistico) |> 
  arrange(p.value) |> 
  filter(p.value < 0.05) |> view()

resultados_slopes_ciber <- avg_slopes(modelo_logistico)
resultados_slopes_ciber |> filter(p.value < 0.05,
                     !term %in% c("sector_lab", "comunidad_autonoma"))

# SOLO DEMOGRÁFICAS
df_reg_demo <- df |>
  mutate(es_ciber = ifelse(perfil_voluntariado == "cibervoluntarios", 1, 0)) |>
  filter(sector_lab != "Construcción") |> 
  select(-c(perfil_voluntariado, tend_politica, vol_12meses)) |> 
  select(c(genero:sector_lab, es_ciber, weight)) |> 
  drop_na() |>
  droplevels()

modelo_logistico_demo <- glm(es_ciber ~ ., data = df_reg_demo, family = binomial(), weights = weight)
summary(modelo_logistico_demo)
tidy(modelo_logistico_demo) |> 
  arrange(p.value) |> 
  filter(p.value < 0.05) |> view()

resultados_slopes_ciber_demo <- avg_slopes(modelo_logistico_demo)
resultados_slopes_ciber_demo |> filter(p.value < 0.05)
         
# ==== PROPENSITY SCORES MATCHING ====

# pipeline para una variable
match_obj <- matchit(es_ciber ~ . - bienestartec_competencias, 
                     data = df_reg, 
                     method = "nearest",
                     distance = "glm",
                     s.weights = ~ weight)
datos_matched <- match.data(match_obj)
modelo_efecto <- lm(bienestartec_competencias ~ es_ciber, data = datos_matched)
summary(modelo_efecto)

# probamos regresión logit con AME para confirmar el resultado
modelo_efecto_binom <- glm(bienestartec_competencias ~ es_ciber, data = datos_matched, family = binomial())
summary(modelo_efecto_binom)
avg_slopes(modelo_efecto_binom)

# función batch
vector_targets <- c(
  # Variables def_vol_
  "def_vol_apoyo_admin", "def_vol_transfor_social", "def_vol_organizacion", 
  "def_vol_ocio", "def_vol_competencias", 
  
  # Variables vol_retos
  "vol_retos_compromiso", "vol_retos_recursos", "vol_retos_burocracia", 
  "vol_retos_independencia", "vol_retos_nuevas_form", "vol_retos_medios_com", 
  
  # Variables bienestar_
  "bienestar_satisfaccion", "bienestar_integridad", "bienestar_desarrollo", 
  "bienestar_libertad", "bienestar_necesidades", "bienestar_pertenencia", 
  "bienestar_agencia", 
  
  # Variables resp_
  "resp_estado", "resp_empresa", "resp_tercer", "resp_ciudadania", "resp_educativas", 
  
  # Variables bienestar_tec_ (bienestartec_)
  "bienestartec_competencias", "bienestartec_al_dia", "bienestartec_no_adicto", 
  "bienestartec_conectado", "bienestartec_critico", 
  
  # Variables afi_
  "afi_quedarse", "afi_gente_atras", "afi_juventud_adiccion", "afi_oportunidades", 
  "afi_demasiado_pantallas", "afi_reducir_brechas", 
  
  # Variables resp_tec_ (resptec_)
  "resptec_estado", "resptec_empresa", "resptec_tercer", "resptec_ciudadania", 
  "resptec_educativas"
)

resultados_psm_ciber <- calcular_efectos_psm(df_reg, vector_targets) 
resultados_psm_ciber |> filter(p.value < 0.05) |> view()


