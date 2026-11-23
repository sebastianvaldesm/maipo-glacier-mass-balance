# T02. Resume los dos periodos de T01 por cuenca y grupo, con las dos formas de error (media
# ponderada de los errores de los glaciares y remuestreo de escenas).
# Uso: Rscript T02_Resumen_Tramos.R [gdem_cpd|tdx90_cpd]
# Sale: Tramos_2010_Cuenca_Grupos.csv
options(width = 250)
args  <- commandArgs(trailingOnly = TRUE)
nrama <- if (length(args)) args[1] else "gdem_cpd"
if (!nrama %in% c("gdem_cpd", "tdx90_cpd")) stop("Uso: Rscript T02_Resumen_Tramos.R [gdem_cpd|tdx90_cpd]")
aqui <- "C:/maipo/proyecto/06_Memoria_Final/Tramos_2010"
if (nrama != "gdem_cpd") aqui <- file.path(aqui, nrama)
rama <- file.path("C:/maipo/trabajo/cadena_propia", nrama, "tablas")
GR <- c("Cuenca alta del Maipo", "Maipo | Mapocho", "Maipo | Olivares", "Maipo | Colorado", "Maipo | Yeso",
        "Maipo | Volcán", "Maipo | Cabecera del Maipo")
leer <- function(d) {
  a <- read.csv(file.path(d, "M06_Agregados.csv"), encoding = "UTF-8")
  a <- a[a$conjunto %in% c("dga", "rgi70", "rgi60") & a$grupo %in% GR, ]
  a[order(match(a$conjunto, c("dga", "rgi70", "rgi60")), match(a$grupo, GR)), ]
}
n13 <- leer(rama)
r <- function(x) round(x, 3)
filas <- list()
cortes <- c("2010.5", "2010.0")
cortes <- cortes[file.exists(file.path(aqui, paste0("corte_", cortes), "tablas", "M06_Agregados.csv"))]
if (!length(cortes)) stop("No hay corridas de T01 en ", aqui)
for (corte in cortes) {
  n10 <- leer(file.path(aqui, paste0("corte_", corte), "tablas"))
  stopifnot(identical(paste(n10$conjunto, n10$grupo), paste(n13$conjunto, n13$grupo)))
  dif <- max(abs(n10$tasa_m_a - n13$tasa_m_a), abs(n10$error_conservador - n13$error_conservador))
  if (dif > 1e-9) stop("El periodo completo cambio entre las corridas: ", dif)
  o <- data.frame(
    corte = corte, inventario = n10$conjunto, grupo = sub("Maipo \\| ", "", n10$grupo), glaciares = n10$n,
    area_km2 = r(n10$area_km2), tasa_2001_2025 = r(n10$tasa_m_a), error_fb = r(n10$error_conservador),
    error_boot = r(n10$error_tasa),
    tasa_2001_2010 = r(n10$tasa_2001_2013), error_2001_2010_fb = r(n10$error_t1_conservador), error_2001_2010_boot = r(n10$error_t1),
    tasa_2010_2025 = r(n10$tasa_2013_2026), error_2010_2025_fb = r(n10$error_t2_conservador), error_2010_2025_boot = r(n10$error_t2),
    balance_2001_2010 = r(n10$tasa_2001_2013 * 0.85), balance_2010_2025 = r(n10$tasa_2013_2026 * 0.85),
    diferencia = r(n10$tasa_2013_2026 - n10$tasa_2001_2013),
    error_diferencia_fb = r(sqrt(n10$error_t1_conservador^2 + n10$error_t2_conservador^2)),
    error_diferencia_boot = r(n10$error_diferencia_tramos),
    corte2013_tasa_2001_2013 = r(n13$tasa_2001_2013), corte2013_tasa_2013_2025 = r(n13$tasa_2013_2026))
  o$veces_error_fb <- r(abs(o$diferencia) / o$error_diferencia_fb)
  o$veces_error_boot <- r(abs(o$diferencia) / o$error_diferencia_boot)
  filas[[corte]] <- o

  t <- read.csv(file.path(aqui, paste0("corte_", corte), "tablas", "M06_Tasa_por_Glaciar.csv"), encoding = "UTF-8")
  t <- t[t$conjunto == "dga" & t$en_maipo %in% c(TRUE, "TRUE", 1), ]
  A <- t$area_utm_km2; mas <- t$tasa_t2_usada < t$tasa_t1_usada
  cat(sprintf("Corte %s, DGA: ajuste propio en 2001-2010 %d de %d glaciares (%.1f %% del area), en 2010-2025 %d (%.1f %%); adelgazan mas rapido en 2010-2025 %d (%.1f %% del area)\n",
              corte, sum(!is.na(t$tasa_t1)), nrow(t), 100 * sum(A[!is.na(t$tasa_t1)]) / sum(A), sum(!is.na(t$tasa_t2)),
              100 * sum(A[!is.na(t$tasa_t2)]) / sum(A), sum(mas, na.rm = TRUE), 100 * sum(A[mas], na.rm = TRUE) / sum(A)))
}
out <- do.call(rbind, filas)
write.csv(out, file.path(aqui, "Tramos_2010_Cuenca_Grupos.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(out[, c("corte", "inventario", "grupo", "tasa_2001_2025", "tasa_2001_2010", "error_2001_2010_fb", "error_2001_2010_boot",
              "tasa_2010_2025", "error_2010_2025_fb", "error_2010_2025_boot", "diferencia", "veces_error_fb",
              "veces_error_boot", "corte2013_tasa_2001_2013", "corte2013_tasa_2013_2025")], row.names = FALSE)
