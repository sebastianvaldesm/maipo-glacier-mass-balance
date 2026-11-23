# I06. Columnas RGI 7.0 y RGI 6.0 de la Tabla 2 del articulo (glaciares, area y escenas por glaciar) con
# los RGI recortados al limite de la cuenca, con las mismas reglas que T01_Tablas_Articulo.R. Las del
# inventario nacional se calculan igual desde la rama, para comprobar contra el Word.
# Uso: Rscript I06_Tabla2_Recortados.R
# Sale: tablas/Tabla2_recortada.csv
base <- "C:/maipo/proyecto"
aqui <- file.path(base, "06_Memoria_Final", "Inventarios_Recortados")
GR_ES <- c("Cuenca alta del Maipo", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Cabecera del Maipo")
GR_EN <- setNames(c("Maipo basin", "Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Maipo headwaters"), GR_ES)
rng <- function(x) sprintf("%s (%s\u2013%s)", formatC(median(x), format = "f", digits = 0),
                           formatC(min(x), format = "f", digits = 0), formatC(max(x), format = "f", digits = 0))
glaciares <- function(dir_tab_m06, dir_tab_m05c, cj) {
  G <- read.csv(file.path(dir_tab_m06, "M06_Tasa_por_Glaciar.csv"), encoding = "UTF-8")
  G <- G[G$conjunto == cj & G$en_maipo %in% c(TRUE, "TRUE"), ]
  SE <- read.csv(file.path(dir_tab_m05c, "M05c_Serie.csv"))
  SE <- SE[SE$es_referencia %in% c(TRUE, "True") | SE$motivo_serie %in% "ok", ]
  n <- table(paste(SE$conjunto, SE$gid))
  G$n_fechas <- as.integer(n[paste(G$conjunto, G$gid)]); G$n_fechas[is.na(G$n_fechas)] <- 0L
  G
}
rama <- "C:/maipo/trabajo/cadena_propia/gdem_cpd/tablas"
L <- list(dga = glaciares(rama, rama, "dga"),
          rgi70 = glaciares(file.path(aqui, "corte_2010.5/tablas"), file.path(aqui, "tablas"), "rgi70"),
          rgi60 = glaciares(file.path(aqui, "corte_2010.5/tablas"), file.path(aqui, "tablas"), "rgi60"))
filas <- list()
for (g in GR_ES) {
  f <- list(grupo = GR_EN[[g]])
  for (cj in names(L)) {
    d <- L[[cj]]; if (g != "Cuenca alta del Maipo") d <- d[d$grupo_cuenca == g, ]
    f[[paste0(cj, "_glaciares")]] <- as.character(nrow(d))
    f[[paste0(cj, "_area")]] <- formatC(sum(d$area_utm_km2), format = "f", digits = 1)
    f[[paste0(cj, "_escenas")]] <- rng(d$n_fechas)
  }
  filas[[length(filas) + 1]] <- as.data.frame(f, check.names = FALSE)
}
T2 <- do.call(rbind, filas)
write.csv(T2, file.path(aqui, "tablas", "Tabla2_recortada.csv"), row.names = FALSE, fileEncoding = "UTF-8")
options(width = 220); print(T2, row.names = FALSE)
