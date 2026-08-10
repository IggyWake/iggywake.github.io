# === TABLAS ====
tabla_voluntec <- function(data, titulo = "Resumen de Datos", subtitulo = "Población ponderada") {
  data %>%
    gt() %>%
    # Títulos y subtítulos
    tab_header(
      title = md(paste0("**", titulo, "**")),
      subtitle = subtitulo
    ) %>%
    # Formatear números: miles con punto y decimales con coma (estilo español)
    fmt_number(
      columns = where(is.numeric) & !contains("Porcentaje"),
      decimals = 3,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    # Formatear porcentajes
    fmt_percent(
      columns = contains("Porcentaje"),
      decimals = 1,
      dec_mark = ","
    ) %>%
    # Estilo visual moderno
    tab_options(
      table.width = pct(80),
      column_labels.background.color = "#f2f2f2",
      table.border.top.color = "black",
      table.border.bottom.color = "black",
      heading.align = "left"
    ) %>%
    # Resaltar la fuente
    opt_table_font(font = "Segoe UI")
}
# ====
# ==== TESTS PONDERADOS X PERFIL ====
run_weighted_tests <- function(df_input, vars_to_test) {
  
  # 1. Definimos los perfiles a testear contra la base
  perfiles_target <- c("voluntario_pasado", "voluntario_actual", "cibervoluntarios")
  grupo_base <- "no_voluntario"
  
  # Generamos todas las combinaciones automáticamente basándonos en el vector aportado
  tasks <- expand_grid(
    col_name = vars_to_test,
    perfil_test = perfiles_target
  ) |> 
    mutate(test_label = paste(col_name, perfil_test, "vs_no_voluntario", sep = "_"))
  
  # 2. Función de ejecución del test
  do_weighted_t <- function(col_name, perfil_test, test_label) {
    
    # Filtramos quitando NAs en la variable de interés para evitar errores en el test
    df_clean <- df_input |> filter(!is.na(.data[[col_name]]), !is.na(weight))
    
    # Grupo X: El perfil de voluntariado específico
    x_group <- df_clean |> filter(perfil_voluntariado == perfil_test)
    
    # Grupo Y: El grupo base (Nunca ha sido voluntario)
    y_group <- df_clean |> filter(perfil_voluntariado == grupo_base)
    
    # Ejecutamos el test ponderado
    test <- wtd.t.test(
      x = x_group[[col_name]], 
      y = y_group[[col_name]], 
      weight = x_group$weight, 
      weighty = y_group$weight
    )
    
    # Extraemos el p-valor
    p_val <- test$coefficients["p.value"]
    
    # Devolvemos una fila limpia con los resultados
    tibble(
      variable    = col_name,
      perfil      = perfil_test,
      test_id     = test_label,
      mean_perfil = test$additional["Mean.x"],
      mean_base   = test$additional["Mean.y"],
      difference  = test$additional["Difference"],
      p_value     = p_val,
      significativo = case_when(
        p_val < 0.001 ~ "***",
        p_val < 0.01  ~ "**",
        p_val < 0.05  ~ "*",
        TRUE ~ "ns" 
      )
    )
  }
  
  # 3. Iteración y retorno
  message(paste("Calculando tests para", length(vars_to_test), "variables (Target vs No Voluntario)..."))
  final_results <- pmap_df(tasks, do_weighted_t)
  
  return(final_results)
}

# ==== CHISQ PARA MULTIRESPUESTA ====
test_multi_chisq <- function(grupo, prefijo, dis_obj = design_df) {
  # Extraer variables que empiezan por el prefijo
  columnas <- grep(paste0("^", prefijo), names(dis_obj$variables), value = TRUE)
  
  # Aplicar el test a cada variable
  resultados <- lapply(columnas, function(var) {
    formula_test <- as.formula(paste0("~", grupo, " + ", var))
    svychisq(formula_test, design = dis_obj)
  })
  
  # Nombrar y devolver
  names(resultados) <- columnas
  return(resultados)
}

# Ejemplo de uso: test_multi_chisq("perfil_voluntariado", "vol_nunca_")


# ==== CHISQ PARA DOBLE MULTIRESPUESTA ====
test_doble_multi_chisq <- function(data, prefijo1, prefijo2) {
  # Creamos el diseño general una sola vez
  diseno <- svydesign(ids = ~1, data = data, weights = ~weight)
  
  # Extraemos los nombres exactos de las variables
  vars1 <- grep(paste0("^", prefijo1), names(data), value = TRUE)
  vars2 <- grep(paste0("^", prefijo2), names(data), value = TRUE)
  
  # Generamos todas las combinaciones, iteramos y guardamos en tibble
  expand_grid(variable_1 = vars1, variable_2 = vars2) |> 
    mutate(
      p_value = map2_dbl(variable_1, variable_2, \(v1, v2) {
        formula_test <- as.formula(paste0("~", v1, " + ", v2))
        
        # tryCatch evita que el bucle entero se rompa si una combinación da error por falta de muestra
        tryCatch(
          svychisq(formula_test, design = diseno)$p.value,
          error = \(e) NA_real_
        )
      }),
      significativo = if_else(p_value < 0.05, "Sí", "No")
    ) |> 
    # Ordenamos dejando las relaciones más fuertes (menor p-value) arriba
    arrange(p_value)
}

# Ejemplo de uso:
# test_doble_multi_chisq(df_actuales, "volu_tipo_", "volu_razones_") |> view()

# ==== TESTS PAREADOS ====
test_pareado_generico <- function(df_input, prefijo1, prefijo2, var_grupo) {
  
  # 1. Identificar sufijos comunes (ej. "estado", "empresa"...)
  vars1 <- grep(paste0("^", prefijo1), names(df_input), value = TRUE)
  vars2 <- grep(paste0("^", prefijo2), names(df_input), value = TRUE)
  
  sufijos1 <- gsub(paste0("^", prefijo1), "", vars1)
  sufijos2 <- gsub(paste0("^", prefijo2), "", vars2)
  sufijos <- intersect(sufijos1, sufijos2)
  
  if (length(sufijos) == 0) stop("ERROR: No hay sufijos coincidentes entre los prefijos dados.")
  
  # 2. Obtener los grupos (perfiles)
  grupos <- df_input |> 
    filter(!is.na(.data[[var_grupo]]), .data[[var_grupo]] != "no_clasificado") |> 
    pull(.data[[var_grupo]]) |> 
    unique() |> 
    as.character()
  
  tareas <- expand_grid(sufijo = sufijos, grupo = grupos)
  
  # 3. Ejecutar el test
  do_test <- function(sufijo, grupo) {
    v1 <- paste0(prefijo1, sufijo)
    v2 <- paste0(prefijo2, sufijo)
    
    # Preparamos los datos restando las variables 
    df_clean <- df_input |> 
      filter(
        .data[[var_grupo]] == grupo,
        !is.na(.data[[v1]]),
        !is.na(.data[[v2]]),
        !is.na(weight)
      ) |> 
      mutate(dif_pareada = .data[[v1]] - .data[[v2]]) # Nueva variable de diferencia
    
    if (nrow(df_clean) < 2) return(NULL)
    
    # wtd.t.test con un solo vector (x) comprueba si la media ponderada difiere de 0
    test <- wtd.t.test(x = df_clean$dif_pareada, weight = df_clean$weight)
    
    pct1 <- weighted.mean(df_clean[[v1]], df_clean$weight)
    pct2 <- weighted.mean(df_clean[[v2]], df_clean$weight)
    p_val <- test$coefficients["p.value"]
    
    tibble(
      grupo = grupo,
      item = sufijo,
      media_1 = pct1,
      media_2 = pct2,
      diferencia = pct1 - pct2,
      p_value = p_val,
      significativo = case_when(
        p_val < 0.001 ~ "***",
        p_val < 0.01  ~ "**",
        p_val < 0.05  ~ "*",
        TRUE ~ "ns" 
      )
    )
  }
  
  return(pmap_dfr(tareas, do_test))
}

# Ejecución:
tests_pareados <- test_pareado_generico(df, prefijo1 = "resp_", prefijo2 = "resptec_", var_grupo = "perfil_voluntariado")

# ==== REGRESIÓN LINEAL ====
regresion_lineal <- function(df_input, prefijo_target, explicativas) {
  # 1. Buscar variables target
  targets <- grep(paste0("^", prefijo_target), names(df_input), value = TRUE)
  
  # 2. Limpiar datos y crear diseño
  diseno <- df_input |> filter(perfil_voluntariado != "no_clasificado" | is.na(perfil_voluntariado)) |> 
    svydesign(ids = ~1, data = _, weights = ~weight)
  
  # 3. Mapear la regresión a cada target
  map(targets, function(t) {
    f <- as.formula(paste(t, "~", explicativas))
    svyglm(f, design = diseno, family = gaussian())
  }) |> 
    set_names(targets) # Nombra los elementos de la lista devuelta
}
# ==== REGRESIÓN BINOMIAL ====
regresion_binomial <- function(df_input, prefijo_target, explicativas) {
  # 1. Buscar variables target
  targets <- grep(paste0("^", prefijo_target), names(df_input), value = TRUE)
  
  # 2. Limpiar datos y crear diseño
  diseno <- df_input |> filter(perfil_voluntariado != "no_clasificado" | is.na(perfil_voluntariado)) |> 
    svydesign(ids = ~1, data = _, weights = ~weight)
  
  # 3. Mapear la regresión a cada target
  map(targets, function(t) {
    f <- as.formula(paste(t, "~", explicativas))
    svyglm(f, design = diseno, family = quasibinomial())
  }) |> 
    set_names(targets) # Nombra los elementos de la lista devuelta
}
# ==== REGRESIÓN TIDY ====
reg_tidy <- function(modelos) {
  modelos |> 
    map_dfr(tidy, .id = "variable_target") |> 
    mutate(
      sig = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ "ns"
      )
    )
}

# ====
# ==== LOGIT TO PROB CALCULATOR ====
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(2.18)

# ==== PREDICCIÓN MANUAL LINEAL ====
prediccion_manual_lineal <- function(tibble_coeficientes, target) {
  
  coefs <- tibble_coeficientes |> filter(variable_target == target)
  
  get_coef <- function(nombre_termino) {
    valor <- coefs$estimate[coefs$term == nombre_termino]
    if(length(valor) == 0) return(0) else return(valor[1])
  }
  
  b_intercept  <- get_coef("(Intercept)")
  b_edad       <- get_coef("edad_num")
  
  b_vol_actual <- get_coef("perfil_voluntariadovoluntario_actual")
  b_vol_pasado <- get_coef("perfil_voluntariadovoluntario_pasado")
  b_ciber      <- get_coef("perfil_voluntariadocibervoluntarios")
  
  i_vol_actual <- get_coef("edad_num:perfil_voluntariadovoluntario_actual")
  i_vol_pasado <- get_coef("edad_num:perfil_voluntariadovoluntario_pasado")
  i_ciber      <- get_coef("edad_num:perfil_voluntariadocibervoluntarios")
  
  grid <- expand_grid(
    perfil_voluntariado = c("no_voluntario", "voluntario_actual", "voluntario_pasado", "cibervoluntarios"),
    edad_num = c(20, 40, 60)
  )
  
  resultados_calculados <- grid |> 
    mutate(
      coef_perfil = case_when(
        perfil_voluntariado == "voluntario_actual" ~ b_vol_actual,
        perfil_voluntariado == "voluntario_pasado" ~ b_vol_pasado,
        perfil_voluntariado == "cibervoluntarios"  ~ b_ciber,
        TRUE ~ 0 
      ),
      coef_interaccion = case_when(
        perfil_voluntariado == "voluntario_actual" ~ i_vol_actual,
        perfil_voluntariado == "voluntario_pasado" ~ i_vol_pasado,
        perfil_voluntariado == "cibervoluntarios"  ~ i_ciber,
        TRUE ~ 0
      ),
      puntuacion_estimada = b_intercept + (b_edad * edad_num) + coef_perfil + (coef_interaccion * edad_num),
      variable_target = target
    ) |> 
    select(variable_target, perfil_voluntariado, edad_num, puntuacion_estimada)
  
  return(resultados_calculados)
}

# ==== AME MASTER PIPELINE ====

ame_analysis <- function(datos, 
                         targets, 
                         var_efecto, 
                         var_agrupacion, 
                         puntos_prediccion, 
                         tipo_modelo = "binomial") {
  
  # --- 1. CÁLCULO DE AME PARA TODAS LAS VARIABLES ---
  message("Paso 1: Calculando AMEs para ", length(targets), " variables...")
  
  resultados_ame <- map_dfr(targets, function(target) {
    
    datos_modelo <- datos |> 
      select(all_of(target), all_of(var_efecto), all_of(var_agrupacion)) |> 
      drop_na() |> 
      mutate(!!sym(var_agrupacion) := droplevels(factor(!!sym(var_agrupacion))))
    
    if (nrow(datos_modelo) == 0 || length(unique(datos_modelo[[var_agrupacion]])) < 2) {
      return(tibble())
    }
    
    formula_dinamica <- as.formula(paste(target, "~", var_efecto, "*", var_agrupacion))
    
    tryCatch({
      modelo <- glm(formula_dinamica, data = datos_modelo, family = tipo_modelo)
      
      ame <- avg_slopes(
        modelo, 
        variables = var_efecto, 
        by = var_agrupacion, 
        newdata = datos_modelo
      )
      
      as_tibble(ame) |> mutate(variable_target = target)
      
    }, error = function(e) {
      message("⚠️ Saltando ", target, " por error: ", e$message)
      return(tibble())
    })
  })
  
  # --- 2. FILTRADO DE SIGNIFICATIVOS ---
  ame_sig <- resultados_ame |> 
    filter(p.value < 0.05) |> 
    select(variable_target, grupo = all_of(var_agrupacion), estimate, p.value, conf.low, conf.high)
  
  if (nrow(ame_sig) == 0) {
    message("No hay efectos significativos. Deteniendo pipeline.")
    return(list(ame_completos = resultados_ame, resumen_final = tibble()))
  }
  
  # --- 3. PREDICCIONES (Solo sobre significativos) ---
  message("Paso 2: Calculando predicciones para ", nrow(ame_sig), " casos significativos...")
  
  predicciones <- pmap_dfr(ame_sig, function(variable_target, grupo, ...) {
    
    grupo_actual <- as.character(grupo)
    
    datos_modelo <- datos |> 
      select(all_of(variable_target), all_of(var_efecto), all_of(var_agrupacion)) |> 
      drop_na() |> 
      mutate(!!sym(var_agrupacion) := droplevels(factor(!!sym(var_agrupacion))))
    
    nivel_referencia <- levels(datos_modelo[[var_agrupacion]])[1]
    
    formula_dinamica <- as.formula(paste(variable_target, "~", var_efecto, "*", var_agrupacion))
    modelo <- glm(formula_dinamica, data = datos_modelo, family = tipo_modelo)
    
    args_grid <- list(model = modelo)
    args_grid[[var_efecto]] <- puntos_prediccion
    args_grid[[var_agrupacion]] <- unique(c(nivel_referencia, grupo_actual))
    
    grid_dinamico <- do.call(marginaleffects::datagrid, args_grid)
    preds <- predictions(modelo, newdata = grid_dinamico)
    
    as_tibble(preds) |> mutate(variable_target = variable_target)
  })
  
  # --- 4. PIVOT Y JOIN FINAL ---
  # A. Descartamos intervalos de confianza y pivotamos las predicciones a formato ancho
  predicciones_ancha <- predicciones |> 
    select(
      variable_target, 
      !!sym(var_agrupacion), 
      !!sym(var_efecto), 
      valor_estimado = estimate
    ) |> 
    distinct() |> 
    pivot_wider(
      names_from = !!sym(var_efecto),
      values_from = valor_estimado,
      names_prefix = "pred_"
    )
  
  # B. Renombramos "grupo" a su nombre original en la tabla de significativos
  ame_sig_final <- ame_sig |> 
    rename(!!sym(var_agrupacion) := grupo) |> 
    select(variable_target, !!sym(var_agrupacion), ame_coeficiente = estimate, p.value)
  
  # C. Inner join de ambas tablas
  tabla_final <- inner_join(
    ame_sig_final,
    predicciones_ancha,
    by = c("variable_target", var_agrupacion)
  ) |> 
    arrange(variable_target, !!sym(var_agrupacion))
  
  # --- Formateo condicional a porcentaje ---
  if (tipo_modelo == "binomial") {
    tabla_final <- tabla_final |> 
      mutate(
        ame_coeficiente = scales::percent(ame_coeficiente, accuracy = 0.1),
        across(starts_with("pred_"), ~ scales::percent(.x, accuracy = 0.1))
      )
  }
  
  message("¡Completado!")
  
  return(list(
    ame_completos = resultados_ame,
    resumen_final = tabla_final
  ))
}

# ==== EFECTOS CON PSM ====
calcular_efectos_psm <- function(data, vector_targets) {
  map_dfr(vector_targets, function(target) {
    # 1. Matching excluyendo el target actual
    form_match <- as.formula(paste("es_ciber ~ . -", target))
    match_obj <- matchit(form_match, data = data, method = "nearest")
    datos_matched <- match.data(match_obj)
    
    # 2. Modelo lineal con el target actual
    form_lm <- as.formula(paste(target, "~ es_ciber"))
    modelo_efecto <- lm(form_lm, data = datos_matched)
    
    # 3. Extraer solo el efecto de es_ciber de forma limpia
    tidy(modelo_efecto) |> 
      filter(term == "es_ciber") |> 
      mutate(variable_target = target, .before = 1)
  })
}
