# I04. Valores de la Tabla 3 del articulo con el RGI 7.0 y el RGI 6.0 recortados al limite de la cuenca,
# con las mismas formulas que T01_Tablas_Articulo.R (error por remuestreo de escenas). La columna del
# inventario nacional sale de la corrida vigente (Tramos_2010), asi queda igual a la publicada; las del
# RGI salen de I03.
# Uso: Rscript I04_Tabla3_Recortados.R
# Sale: tablas/Tabla3_recortada.csv (una fila por grupo y variable, ya formateada) y
#       tablas/Resumen_Inventarios_Recortados.csv (cifras para el texto)
base <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
aqui <- file.path(base, "06_Memoria_Final", "Inventarios_Recortados")
RHO <- 850; S_RHO <- 60; S_AREA <- 0.05
GR_ES <- c("Cuenca alta del Maipo", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Cabecera del Maipo")
GR_EN <- setNames(c("Maipo basin", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Maipo headwaters"), GR_ES)
GR_AG <- setNames(c("Cuenca alta del Maipo", paste("Maipo |", GR_ES[-1])), GR_ES)

f2 <- function(x, d = 2, mas = FALSE) {
  s <- formatC(abs(x), format = "f", digits = d)
  ifelse(round(x, d) < 0, paste0("\u2212", s), ifelse(mas & round(x, d) > 0, paste0("+", s), s))
}
pm <- function(x, e, d = 2, mas = FALSE) paste0(f2(x, d, mas), " \u00B1 ", formatC(e, format = "f", digits = d))

# inventario nacional: lo mismo que lee T01
TT <- read.csv(file.path(base, "06_Memoria_Final/Tramos_2010/Tramos_2010_Cuenca_Grupos.csv"), encoding = "UTF-8")
TT <- TT[TT$corte == 2010.5, ]
A0 <- read.csv("C:/Users/sebas/Prueba_MMASTER/cadena_propia/gdem_cpd/tablas/M06_Agregados.csv", encoding = "UTF-8")
# RGI recortados: I03
A1 <- read.csv(file.path(aqui, "corte_2010.5/tablas/M06_Agregados.csv"), encoding = "UTF-8")

valores <- function(cj, g) {
  if (cj == "dga") {
    t <- TT[TT$inventario == "dga" & TT$grupo == g, ]; a <- A0[A0$conjunto == "dga" & A0$grupo == GR_AG[[g]], ]
    r <- t$tasa_2001_2025; e <- t$error_boot; r1 <- t$tasa_2001_2010; e1 <- t$error_2001_2010_boot
    r2 <- t$tasa_2010_2025; e2 <- t$error_2010_2025_boot
  } else {
    a <- A1[A1$conjunto == cj & A1$grupo == GR_AG[[g]], ]
    r <- round(a$tasa_m_a, 3); e <- round(a$error_tasa, 3); r1 <- round(a$tasa_2001_2013, 3); e1 <- round(a$error_t1, 3)
    r2 <- round(a$tasa_2013_2026, 3); e2 <- round(a$error_t2, 3)
  }
  stopifnot(nrow(a) == 1)
  rb <- sqrt((e / r)^2 + (S_RHO / RHO)^2); rb9 <- sqrt((e / r)^2 + (S_RHO / 900)^2)
  rm <- sqrt((e / r)^2 + S_AREA^2 + (S_RHO / RHO)^2)
  list(n = a$n, area = a$area_km2, r = r, e = e, r1 = r1, e1 = e1, r2 = r2, e2 = e2,
       b = a$balance_850, eb = abs(a$balance_850) * rb, b9 = a$balance_900, eb9 = abs(a$balance_900) * rb9,
       m = a$dM_Mt_850 / 1000, em = abs(a$dM_Mt_850 / 1000) * rm)
}
VARS <- c("Rate 2001\u20132025 (m a\u22121)", "Rate 2001\u20132010 (m a\u22121)", "Rate 2010\u20132025 (m a\u22121)",
          "Balance, 850 kg m\u22123 (m w.e. a\u22121)", "Balance, 900 kg m\u22123 (m w.e. a\u22121)", "Mass change, 850 kg m\u22123 (Gt)")
INV <- c("dga", "rgi70", "rgi60")
filas <- list(); res <- list()
for (g in GR_ES) {
  V <- lapply(INV, valores, g = g)
  celdas <- list(sapply(V, function(v) pm(v$r, v$e)), sapply(V, function(v) pm(v$r1, v$e1, mas = TRUE)),
                 sapply(V, function(v) pm(v$r2, v$e2)), sapply(V, function(v) pm(v$b, v$eb)),
                 sapply(V, function(v) pm(v$b9, v$eb9)), sapply(V, function(v) pm(v$m, v$em)))
  for (k in seq_along(VARS))
    filas[[length(filas) + 1]] <- data.frame(grupo = GR_EN[[g]], variable = VARS[k], national = celdas[[k]][1],
                                             rgi70 = celdas[[k]][2], rgi60 = celdas[[k]][3])
  for (i in seq_along(INV))
    res[[length(res) + 1]] <- data.frame(grupo = GR_EN[[g]], inventario = INV[i], glaciares = V[[i]]$n,
                                         area_km2 = round(V[[i]]$area, 1), tasa = V[[i]]$r, balance_850 = round(V[[i]]$b, 3),
                                         balance_900 = round(V[[i]]$b9, 3), masa_Gt = round(V[[i]]$m, 2))
}
T3 <- do.call(rbind, filas); R <- do.call(rbind, res)
dir.create(file.path(aqui, "tablas"), showWarnings = FALSE)
write.csv(T3, file.path(aqui, "tablas", "Tabla3_recortada.csv"), row.names = FALSE, fileEncoding = "UTF-8")
# diferencias con el inventario nacional, para el texto
n <- R[R$inventario == "dga", ]
R$dif_balance_vs_nacional <- round(R$balance_850 - n$balance_850[match(R$grupo, n$grupo)], 3)
R$dif_balance_pct <- round(100 * (R$balance_850 / n$balance_850[match(R$grupo, n$grupo)] - 1), 1)
R$dif_masa_pct <- round(100 * (R$masa_Gt / n$masa_Gt[match(R$grupo, n$grupo)] - 1), 1)
write.csv(R, file.path(aqui, "tablas", "Resumen_Inventarios_Recortados.csv"), row.names = FALSE, fileEncoding = "UTF-8")
options(width = 200)
print(T3[T3$grupo == "Maipo basin", ], row.names = FALSE)
print(R[R$grupo == "Maipo basin", ], row.names = FALSE)
