# M04b. Resume el experimento de M04: NMAD del anillo de control sin alinear, alineando con toda la
# escena y con cada ventana local, sobre los mismos pares. De aqui sale la ventana de 2 km.
# Sale: tablas/M04_Ventanas*.csv y M04_Placebo_*.csv

suppressPackageStartupMessages(library(terra))
base    <- "C:/maipo/proyecto"
dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")
dir_tab <- file.path(dir_c, "tablas")

fs <- list.files(file.path(dir_c, "M04_salida"), "\\.csv$", full.names = TRUE)
A <- do.call(rbind, lapply(fs, function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE)
  if (!"gid" %in% names(d)) return(NULL)
  d
}))
A$clave <- paste(A$escena, A$gid)
A$var_met <- ifelse(A$variante == "sin alinear", "sin alinear", paste(A$variante, A$metodo, sep = " | "))
cat("Filas:", nrow(A), "| escenas:", length(unique(A$escena)), "| glaciares:", length(unique(A$gid)), "\n")

nmad <- function(x) 1.4826 * median(abs(x - median(x)))

# --- 1. Comparacion sobre los pares comunes -------------------------------------------
vm <- unique(A$var_met)
n_por <- tapply(A$clave, A$var_met, function(k) length(unique(k)))
print(sort(n_por))
comunes <- Reduce(intersect, lapply(split(A$clave, A$var_met), unique))
cat("Pares escena-glaciar con las once variantes:", length(comunes), "\n")
Ac <- A[A$clave %in% comunes, ]
orden <- c("sin alinear", "zona | NuthKaab", "zona | NuthKaab+Deramp",
           paste0("local ", rep(c(10, 5, 3, 2), each = 2), " km | ", c("NuthKaab", "NuthKaab+Deramp")))
V <- do.call(rbind, lapply(orden, function(v) {
  d <- Ac[Ac$var_met == v, ]
  data.frame(variante = v, pares = nrow(d),
             mediana_anillo = median(d$anillo_mediana),
             mediana_abs_anillo = median(abs(d$anillo_mediana)),
             NMAD_anillo_entre_pares = nmad(d$anillo_mediana),
             p90_abs_anillo = unname(quantile(abs(d$anillo_mediana), 0.9)),
             pct_dentro_2m = 100 * mean(abs(d$anillo_mediana) <= 2),
             NMAD_dentro_anillo = median(d$anillo_nmad))
}))
num <- sapply(V, is.numeric); V[num] <- lapply(V[num], round, 3)
write.csv(V, file.path(dir_tab, "M04_Ventanas.csv"), row.names = FALSE)
print(V, row.names = FALSE)

# Por tamano de glaciar: la ventana local deberia importar mas en los chicos
Ac$clase <- cut(Ac$area_km2, c(0, 0.1, 1, 5, Inf), labels = c("< 0,1 km2", "0,1-1 km2", "1-5 km2", "> 5 km2"))
VT <- aggregate(anillo_mediana ~ var_met + clase, Ac, function(x) round(nmad(x), 3))
VT <- reshape(VT, idvar = "var_met", timevar = "clase", direction = "wide")
write.csv(VT, file.path(dir_tab, "M04_Ventanas_por_tamano.csv"), row.names = FALSE)
print(VT, row.names = FALSE)

# --- 2. Prueba placebo: tendencia del anillo en 25 anios ------------------------------
gl <- vect(file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg"), "rgi70")
ga <- as.data.frame(gl)[, c("rgi_id", "area_utm_km2", "nivel", "sistema", "COD_SCUEN")]
placebo <- function(v) {
  # Mismo filtro que exige la cadena principal a la roca de cada par: si la
  # propia roca del anillo tiene NMAD > 15 m (nube, sombra), el par no cuenta.
  d <- A[A$var_met == v & A$anillo_nmad <= 15, ]
  do.call(rbind, lapply(split(d, d$gid), function(s) {
    if (nrow(s) < 8 || diff(range(s$anio_dec)) < 10) return(NULL)
    m <- summary(lm(anillo_mediana ~ anio_dec, s))$coefficients
    data.frame(variante = v, gid = s$gid[1], n = nrow(s), tasa_m_a = m[2, 1], error = m[2, 2],
               p = m[2, 4],
               temprano_m = median(s$anillo_mediana[s$anio_dec < 2005]),
               tardio_m = median(s$anillo_mediana[s$anio_dec >= 2021]))
  }))
}
P <- do.call(rbind, lapply(c("sin alinear", "zona | NuthKaab+Deramp", "zona | NuthKaab",
                             paste0("local ", c(2, 3, 5, 10), " km | NuthKaab+Deramp"),
                             paste0("local ", c(2, 3, 5, 10), " km | NuthKaab")), placebo))
P <- merge(P, ga, by.x = "gid", by.y = "rgi_id", all.x = TRUE)
write.csv(P, file.path(dir_tab, "M04_Placebo_por_glaciar.csv"), row.names = FALSE)
PR <- do.call(rbind, lapply(split(P, P$variante), function(d) {
  data.frame(variante = d$variante[1], glaciares = nrow(d),
             tasa_mediana = median(d$tasa_m_a), tasa_NMAD = nmad(d$tasa_m_a),
             tasa_media_area = weighted.mean(d$tasa_m_a, d$area_utm_km2),
             pct_significativa = 100 * mean(d$p < 0.05),
             pct_abs_mayor_0.1 = 100 * mean(abs(d$tasa_m_a) > 0.1),
             cambio_lejanos_mediana = median(d$tardio_m - d$temprano_m, na.rm = TRUE),
             cambio_lejanos_NMAD = nmad((d$tardio_m - d$temprano_m)[is.finite(d$tardio_m - d$temprano_m)]))
}))
num <- sapply(PR, is.numeric); PR[num] <- lapply(PR[num], round, 3)
write.csv(PR, file.path(dir_tab, "M04_Placebo_resumen.csv"), row.names = FALSE)
print(PR, row.names = FALSE)
