# M06. Tasas, volumen y masa
#
#   1. Ajusta dh = B + r (t - t0) a la serie de cada glaciar, con pesos 1 / NMAD(roca)^2, si tiene
#      al menos 5 escenas y la ultima 10 anios despues de la referencia. B absorbe el error de la
#      superficie de referencia. Los glaciares sin ajuste toman la media de su sistema.
#   2. Volumen dV = r A T y masa con 850 +- 60 y 900 kg/m3; balance especifico sobre el area del
#      inventario.
#   3. Agregados por cuenca, subcuenca, tamano y cota, con el error por remuestreo de escenas
#      (500 sorteos con reposicion, todos los glaciares reajustados con las mismas escenas).
#   4. Contraste con Dussaillant et al. (2019) y Farias-Barahona et al. (2020).
# Sale: tablas/M06_*.csv y M06_resultados.rds

suppressPackageStartupMessages(library(terra))
base    <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")
dir_tab <- file.path(dir_c, "tablas")
# Variantes: CADENA_RM_SUFIJO lee tablas/M05c_*<sufijo>.csv y escribe en
# tablas/variante<sufijo>/, sin tocar los resultados vigentes.
SUF <- Sys.getenv("CADENA_RM_SUFIJO", "")
dir_out <- if (SUF == "") dir_tab else file.path(dir_tab, paste0("variante", SUF))
dir.create(dir_out, showWarnings = FALSE)
f_inv   <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg")
dir_m5  <- file.path(dir_c, Sys.getenv("CADENA_RM_SALIDA", "M05_salida"))

T_INI <- 2001.0; T_FIN <- 2026.0; T <- T_FIN - T_INI      # enero de 2001 a enero de 2026
T_CORTE <- 2013.5                                           # dos tramos de 12,5 anios
RHO <- 850; S_RHO <- 60; RHO_ALT <- 900; S_AREA <- 0.05
# minimos de la serie; las variables de entorno sirven solo para probar el
# script con una corrida parcial
MIN_N <- as.numeric(Sys.getenv("CADENA_RM_MIN_N", "5"))
MIN_SPAN <- as.numeric(Sys.getenv("CADENA_RM_MIN_SPAN", "10"))

nmad <- function(x) { x <- x[is.finite(x)]; 1.4826 * median(abs(x - median(x))) }
es_v <- function(x) x %in% c(TRUE, "True", "TRUE")       # Python escribe True/False como texto
leer <- function(dir, patron) {
  fs <- list.files(dir, patron, full.names = TRUE)
  l <- lapply(fs, function(f) {
    d <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(d) && nrow(d) && !"nota" %in% names(d)) d else NULL
  })
  l <- Filter(Negate(is.null), l)
  cols <- unique(unlist(lapply(l, names)))
  do.call(rbind, lapply(l, function(d) { for (c in setdiff(cols, names(d))) d[[c]] <- NA; d[, cols] }))
}
P  <- read.csv(file.path(dir_tab, paste0("M05c_Pares", SUF, ".csv")), stringsAsFactors = FALSE)
S  <- read.csv(file.path(dir_tab, paste0("M05c_Serie", SUF, ".csv")), stringsAsFactors = FALSE)
UN <- leer(dir_m5, "_unidades\\.csv$")
MT <- leer(dir_m5, "_metodos\\.csv$")
ER <- leer(dir_m5, "_error\\.csv$")
HC <- read.csv(file.path(dir_tab, paste0("M05c_Hielo_Cota", SUF, ".csv")))
BU <- read.csv(file.path(dir_tab, paste0("M05c_Bandas_Union", SUF, ".csv")), stringsAsFactors = FALSE)
S$es_referencia <- es_v(S$es_referencia)
P$pasa <- P$motivo == "aceptado"
cat("Pares poligono-escena:", nrow(P), "| escenas:", length(unique(P$escena)), "\n")

# --- atributos ----------------------------------------------------------------------
SIS <- read.csv(file.path(dir_tab, "M02_Sistemas.csv"), stringsAsFactors = FALSE, colClasses = c(subcuenca = "character"))
SSC <- read.csv(file.path(dir_tab, "M02_Subsubcuencas_DGA.csv"), stringsAsFactors = FALSE, colClasses = c(COD_SSCUEN = "character"))
VU <- vect(f_inv, layer = "union"); V7 <- vect(f_inv, layer = "rgi70"); V6 <- vect(f_inv, layer = "rgi60")
AU <- as.data.frame(VU); AU$gid <- AU$uid; AU$conjunto <- "union"
A7 <- as.data.frame(V7); A7$gid <- A7$rgi_id; A7$conjunto <- "rgi70"
A6 <- as.data.frame(V6); A6$gid <- A6$RGIId; A6$conjunto <- "rgi60"
# Inventario de la DGA (IPG 2014, capa dga de M02b), el principal:
# entra si M05c lo midio. Solo trae los glaciares de la cuenca alta del Maipo, asi que
# sus filas de la Region Metropolitana son la cuenca alta.
f_dga <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventario_DGA_2014_cuenca.gpkg")
CONJ <- c("union", "rgi70", "rgi60", if ("dga" %in% P$conjunto) "dga")
AD <- NULL
if ("dga" %in% CONJ) {
  AD <- as.data.frame(vect(f_dga, layer = "dga")); AD$gid <- as.character(AD$gid); AD$conjunto <- "dga"
}
# cota mediana de cada poligono desde su hipsometria sobre el GDEM
zmed <- function(capa, idc) {
  h <- read.csv(file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER", paste0("01_Hipsometria_", capa, ".csv")))
  sapply(split(h, h[[idc]]), function(d) { d <- d[order(d$banda), ]; c <- cumsum(d$area_m2) / sum(d$area_m2); d$banda[which(c >= 0.5)[1]] + 50 })
}
z_u <- zmed("union", "uid"); AU$zmed <- unname(z_u[AU$uid])
A7$zmed <- A7$zmed_m; A6$zmed <- A6$Zmed
if (!is.null(AD)) { z_d <- zmed("dga", "gid"); AD$zmed <- unname(z_d[AD$gid]) }
# Subcuenca: la del IPG 2022 de la region (via el RGI 7.0). Los que no la tienen
# y quedan mayormente en Argentina son la vertiente argentina. El resto son
# glaciares que tocan la region pero el IPG inventaria en otra (los que escurren
# al Aconcagua o al Rapel, y algunos del Maipo): toman la subcuenca y la
# subsubcuenca del glaciar del IPG 2022 de todo Chile que mas se les superpone o,
# si ninguno, del mas cercano. Antes tomaban la de su sistema, y asi el Glaciar
# Cachapoal y el Juncal Norte quedaban contados en el Maipo Alto.
f_ipg <- file.path(base, "01_Datos/Brutos", "Inventario_DGA", "IPG_2022_v2", "INV_PG_2022_v2.shp")
IPG <- project(vect(f_ipg, extent = ext(-70.9, -69.4, -34.7, -32.5)), "EPSG:32719")
cod_ipg <- function(v, idc) {
  x <- intersect(v[, idc], IPG[, c("COD_SCUEN", "COD_SSCUEN")])
  d <- as.data.frame(x); d$a <- expanse(x)
  d <- d[order(d[[idc]], -d$a), ]; d <- d[!duplicated(d[[idc]]), c(idc, "COD_SCUEN", "COD_SSCUEN")]
  sin <- setdiff(v[[idc]][[1]], d[[idc]])
  if (length(sin)) {
    vs <- v[v[[idc]][[1]] %in% sin]
    nn <- nearest(vs, IPG)
    d <- rbind(d, setNames(data.frame(vs[[idc]][[1]], IPG$COD_SCUEN[nn$to_id], IPG$COD_SSCUEN[nn$to_id]), names(d)))
  }
  d
}
completar <- function(A, V, idc) {
  A$COD_SCUEN <- as.character(A$COD_SCUEN)
  A$COD_SSCUEN <- if ("COD_SSCUEN" %in% names(A)) as.character(A$COD_SSCUEN) else NA_character_
  f <- is.na(A$COD_SCUEN) | A$COD_SCUEN == ""
  arg <- f & A$frac_en_Chile < 0.5
  A$COD_SCUEN[arg] <- "ARG"
  r <- f & !arg
  if (any(r)) {
    L <- cod_ipg(V[V[[idc]][[1]] %in% A[[idc]][r]], idc)
    k <- match(A[[idc]][r], L[[idc]])
    A$COD_SCUEN[r] <- L$COD_SCUEN[k]; A$COD_SSCUEN[r] <- L$COD_SSCUEN[k]
  }
  A$COD_SCUEN[is.na(A$COD_SCUEN) | A$COD_SCUEN == ""] <- "sin codigo"
  A
}
AU <- completar(AU, VU, "uid"); A7 <- completar(A7, V7, "rgi_id"); A6 <- completar(A6, V6, "RGIId")
# Subsubcuenca que falta (los poligonos del RGI 6.0 no la traen, y dos unidades
# chicas del Maipo tampoco): la del poligono de la DGA que mas se les superpone
# (M01c, capa oficial de subsubcuencas BNA). La vertiente argentina no tiene.
f_ssc <- file.path(base, "01_Datos/Brutos", "Cuencas_DGA", "SubSubCuencas_BNA_057_054_060.geojson")
SSCV <- project(vect(f_ssc), "EPSG:32719")
llenar_ssc <- function(A, V, idc) {
  f <- (is.na(A$COD_SSCUEN) | A$COD_SSCUEN == "") & A$COD_SCUEN != "ARG"
  if (!any(f)) return(A)
  x <- intersect(V[V[[idc]][[1]] %in% A[[idc]][f], idc], SSCV[, "COD_SSUBC"])
  d <- as.data.frame(x); d$a <- expanse(x)
  d <- d[order(d[[idc]], -d$a), ]; d <- d[!duplicated(d[[idc]]), ]
  k <- match(A[[idc]][f], d[[idc]])
  A$COD_SSCUEN[f] <- d$COD_SSUBC[k]
  A
}
A6$COD_SSCUEN <- NA_character_
AU <- llenar_ssc(AU, VU, "uid"); A7 <- llenar_ssc(A7, V7, "rgi_id"); A6 <- llenar_ssc(A6, V6, "RGIId")
# Grupos de la cuenca alta del Maipo como en Farias-Barahona et al. (2020, Figura 1),
# con Molina y San Francisco juntos como el Mapocho.
# La cuenca alta del Maipo es la suma del Maipo en El Manzano (subcuenca 0570) y el
# Mapocho en Los Almendros (0572: Molina y San Francisco).
GRUPO_CUENCA <- c("05720" = "Mapocho", "05721" = "Mapocho", "05706" = "Olivares", "05705" = "Colorado",
                  "05703" = "Yeso", "05702" = "Volcán", "05700" = "Cabecera del Maipo",
                  "05701" = "Cabecera del Maipo")
ORDEN_GRUPOS <- c("Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Cabecera del Maipo")
grupo_de <- function(sc, ssc) {
  g <- unname(GRUPO_CUENCA[ssc])
  g[is.na(g) & sc %in% c("0570", "0572")] <- "Maipo, otras"
  g[sc %in% "ARG"] <- "Vertiente argentina"
  g[substr(sc, 1, 3) %in% c("054", "060")] <- "Aconcagua y Rapel"
  g
}
for (nm in c("AU", "A7", "A6", if (!is.null(AD)) "AD")) {
  A <- get(nm); A$grupo_cuenca <- grupo_de(A$COD_SCUEN, A$COD_SSCUEN); A$en_maipo <- A$COD_SCUEN %in% c("0570", "0572")
  assign(nm, A)
}
cat("Subcuencas (union):\n"); print(tapply(AU$area_utm_km2, AU$COD_SCUEN, sum))
cat("Grupos de la cuenca (union):\n"); print(tapply(AU$area_utm_km2, AU$grupo_cuenca, sum))
col <- c("gid", "conjunto", "area_utm_km2", "COD_SCUEN", "sistema", "zmed", "frac_en_Chile", "grupo_cuenca",
         "en_maipo")
LA <- Filter(Negate(is.null), list(AU, A7, A6, AD))
atr <- do.call(rbind, lapply(LA, function(A) A[, col]))
atr$COD_SSCUEN <- unlist(lapply(LA, function(A) as.character(A$COD_SSCUEN)))
n_resto <- nrow(atr) - nrow(AU) - nrow(A7)
atr$area_rgi70_km2 <- c(AU$area_rgi70_km2, A7$area_utm_km2, rep(NA, n_resto))
atr$area_solo_rgi60_km2 <- c(AU$area_solo_rgi60_km2, rep(0, nrow(A7)), rep(NA, n_resto))

# --- 1. control de calidad --------------------------------------------------------
Q <- as.data.frame.matrix(table(P$motivo, P$conjunto))
Q$motivo <- rownames(Q); rownames(Q) <- NULL
write.csv(Q, file.path(dir_out, "M06_Control_Motivos.csv"), row.names = FALSE)
print(Q, row.names = FALSE)

# --- 2. series contra la primera fecha y ajuste --------------------------------------
SE <- merge(S[!S$es_referencia & S$motivo_serie %in% "ok", ],
            P[, c("conjunto", "gid", "escena", "NMAD_roca")], by = c("conjunto", "gid", "escena"))
# (las filas apartadas por temporada no traen referencia)
REF <- unique(S[!is.na(S$t_ref), c("conjunto", "gid", "anio_ref", "t_ref", "n_esc_ref", "cob_ref_pct")])
ajuste <- function(d, t0) {
  if (nrow(d) < MIN_N || (max(d$anio_dec) - t0) < MIN_SPAN) return(NULL)
  w <- 1 / d$NMAD_roca^2; d$x <- d$anio_dec - t0
  m <- lm(dh_m ~ x, d, weights = w); s <- summary(m)$coefficients
  if (nrow(s) < 2) return(NULL)
  o <- data.frame(n = nrow(d), t_ref = t0, t_min = min(d$anio_dec), t_max = max(d$anio_dec),
                  anios = length(unique(floor(d$anio_dec))), B_m = s[1, 1],
                  tasa = s[2, 1], error = s[2, 2], R2 = summary(m)$r.squared,
                  resid_nmad = nmad(residuals(m)))
  o$termino_cuad <- NA; o$p_cuad <- NA; o$tasa_ini <- NA; o$tasa_fin <- NA
  if (nrow(d) >= 8 && (max(d$anio_dec) - t0) >= 15) {
    d$xc <- d$anio_dec - 2013.5
    q <- lm(dh_m ~ xc + I(xc^2), d, weights = w)
    qs <- summary(q)$coefficients
    if (nrow(qs) >= 3) {
      o$termino_cuad <- qs[3, 1]; o$p_cuad <- qs[3, 4]
      o$tasa_ini <- qs[2, 1] + 2 * qs[3, 1] * (T_INI - 2013.5)
      o$tasa_fin <- qs[2, 1] + 2 * qs[3, 1] * (T_FIN - 2013.5)
    }
  }
  for (k in 1:2) {
    dd <- if (k == 1) d[d$anio_dec < T_CORTE, ] else d[d$anio_dec >= T_CORTE, ]
    nm <- if (k == 1) "t1" else "t2"
    o[[paste0("tasa_", nm)]] <- NA; o[[paste0("error_", nm)]] <- NA
    if (nrow(dd) >= 4 && diff(range(dd$anio_dec)) >= 5) {
      s2 <- summary(lm(dh_m ~ anio_dec, dd, weights = 1 / dd$NMAD_roca^2))$coefficients
      if (nrow(s2) >= 2) { o[[paste0("tasa_", nm)]] <- s2[2, 1]; o[[paste0("error_", nm)]] <- s2[2, 2] }
    }
  }
  o
}
t_ref_de <- setNames(REF$t_ref, paste(REF$conjunto, REF$gid))
R <- do.call(rbind, lapply(split(SE, paste(SE$conjunto, SE$gid)), function(d) {
  a <- ajuste(d, t_ref_de[[paste(d$conjunto[1], d$gid[1])]])
  if (is.null(a)) NULL else cbind(conjunto = d$conjunto[1], gid = d$gid[1], a)
}))
R <- merge(atr, R, by = c("conjunto", "gid"), all.x = TRUE)
R <- merge(R, REF[, c("conjunto", "gid", "anio_ref", "n_esc_ref")], by = c("conjunto", "gid"), all.x = TRUE)
R$propio <- !is.na(R$tasa)

# Relleno: los poligonos sin ajuste propio toman la media de su sistema (area
# ponderada) y, si el sistema no tiene al menos dos, la de su conjunto.
rellenar <- function(R, var, err) {
  R[[paste0(var, "_usada")]] <- R[[var]]; R[[paste0(err, "_usado")]] <- R[[err]]
  for (cj in unique(R$conjunto)) {
    s <- R$conjunto == cj
    for (k in which(s & is.na(R[[var]]))) {
      p <- s & !is.na(R[[var]]) & R$sistema %in% R$sistema[k]
      if (sum(p) < 2) p <- s & !is.na(R[[var]])
      R[[paste0(var, "_usada")]][k] <- weighted.mean(R[[var]][p], R$area_utm_km2[p])
      R[[paste0(err, "_usado")]][k] <- max(nmad(R[[var]][p]), weighted.mean(R[[err]][p], R$area_utm_km2[p]))
    }
  }
  R
}
R <- rellenar(R, "tasa", "error")
R <- rellenar(R, "tasa_t1", "error_t1")
R <- rellenar(R, "tasa_t2", "error_t2")

# --- 2b. error de los agregados: bootstrap de escenas ---------------------------------
#. El error de un
# grupo se tomaba como la media de los errores de sus glaciares, es decir,
# suponiendo errores completamente correlacionados: una cota superior. La fuente
# comun de error entre glaciares son las escenas (cada escena aporta un punto a
# todos los glaciares que ve), asi que se remuestrean ESCENAS con reposicion, se
# reajustan todos los poligonos con las mismas escenas remuestreadas y se toma la
# desviacion estandar de la tasa agregada. Asi la correlacion entre glaciares se
# mide en vez de suponerse.
N_BOOT <- 500
boot_tasas <- function(SEb, MINN, MINSPAN, tramos = TRUE, semilla = 1) {
  set.seed(semilla)
  SEb <- SEb[order(SEb$conjunto, SEb$gid), ]
  key <- paste(SEb$conjunto, SEb$gid)
  t0 <- unname(t_ref_de[key])
  esc <- sort(unique(SEb$escena)); ie <- match(SEb$escena, esc)
  w0 <- 1 / SEb$NMAD_roca^2; x <- SEb$anio_dec - t0; y <- SEb$dh_m; ta <- SEb$anio_dec
  g <- factor(key); ng <- nlevels(g)
  pend <- function(W, xx, yy, gg) {
    S0 <- rowsum(W, gg); Sx <- rowsum(W * xx, gg); Sy <- rowsum(W * yy, gg)
    Sxx <- rowsum(W * xx * xx, gg); Sxy <- rowsum(W * xx * yy, gg)
    as.vector((S0 * Sxy - Sx * Sy) / (S0 * Sxx - Sx^2))
  }
  out <- list(tasa = matrix(NA_real_, N_BOOT, ng, dimnames = list(NULL, levels(g))))
  if (tramos) { out$t1 <- out$tasa; out$t2 <- out$tasa }
  t1m <- ta < T_CORTE
  for (b in seq_len(N_BOOT)) {
    cnt <- tabulate(sample.int(length(esc), length(esc), replace = TRUE), length(esc))[ie]
    W <- w0 * cnt; usa <- cnt > 0
    nd <- as.vector(rowsum(as.numeric(usa), g)); xmax <- as.vector(tapply(ifelse(usa, x, -Inf), g, max))
    r <- pend(W, x, y, g); r[nd < MINN | xmax < MINSPAN] <- NA
    out$tasa[b, ] <- r
    if (tramos) for (k in 1:2) {
      m <- if (k == 1) t1m else !t1m
      gg <- factor(key[m], levels = levels(g)); u <- usa[m]
      # tapply y no rowsum: rowsum devuelve solo los glaciares con escenas en el tramo y
      # desalineaba los conteos con rg (corregido el 29-10-2026); tapply deja NA en los demas
      nd <- as.vector(tapply(as.numeric(u), gg, sum))
      rg <- as.vector(tapply(ifelse(u, ta[m], -Inf), gg, max)) - as.vector(tapply(ifelse(u, ta[m], Inf), gg, min))
      rk <- rep(NA_real_, ng); names(rk) <- levels(g)
      rr <- pend(W[m], ta[m], y[m], gg); rk[rownames(rowsum(W[m], gg))] <- rr
      rk[is.na(nd) | nd < 4 | !is.finite(rg) | rg < 5] <- NA
      out[[paste0("t", k)]][b, ] <- rk
    }
  }
  out
}
# relleno igual al de 'rellenar' (media del sistema, o del conjunto) en cada replica
rellenar_boot <- function(M, cj) {
  d <- R[R$conjunto == cj, ]
  ids <- paste(cj, d$gid)
  X <- matrix(NA_real_, nrow(M), length(ids), dimnames = list(NULL, d$gid))
  comun <- intersect(ids, colnames(M)); X[, sub(paste0("^", cj, " "), "", comun)] <- M[, comun]
  a <- d$area_utm_km2; sis <- d$sistema
  for (b in seq_len(nrow(X))) {
    v <- X[b, ]; ok <- is.finite(v)
    if (all(ok)) next
    mc <- weighted.mean(v[ok], a[ok])
    for (k in which(!ok)) {
      p <- ok & sis %in% sis[k]
      X[b, k] <- if (sum(p) >= 2) weighted.mean(v[p], a[p]) else mc
    }
  }
  X
}
t_b <- Sys.time()
BB <- boot_tasas(SE, MIN_N, MIN_SPAN)
BT <- lapply(setNames(CONJ, CONJ), function(cj)
  lapply(BB, function(M) rellenar_boot(M, cj)))
cat(sprintf("Bootstrap de escenas: %d replicas (%.0f s)\n", N_BOOT, as.numeric(Sys.time() - t_b, units = "secs")))
# desviacion estandar de la media ponderada por area de un conjunto de poligonos
sd_boot <- function(cj, gids, w, var = "tasa") {
  M <- BT[[cj]][[var]][, gids, drop = FALSE]
  sd(as.vector(M %*% w) / sum(w), na.rm = TRUE)
}
R$dV_Mm3 <- R$tasa_usada * R$area_utm_km2 * T
R$balance_850 <- R$tasa_usada * RHO / 1000
R$balance_900 <- R$tasa_usada * RHO_ALT / 1000
num <- sapply(R, is.numeric); Rw <- R; Rw[num] <- lapply(Rw[num], round, 4)
write.csv(Rw, file.path(dir_out, "M06_Tasa_por_Glaciar.csv"), row.names = FALSE)
cat("\nAjuste propio por conjunto:\n")
print(aggregate(cbind(n = 1, propio = propio, area = area_utm_km2, area_propio = area_utm_km2 * propio) ~ conjunto, R, sum))
print(table(R$conjunto, R$anio_ref))

# --- 3 y 4. agregados ------------------------------------------------------------------
agregar <- function(d, grupo, cj) {
  if (!nrow(d)) return(NULL)
  A <- sum(d$area_utm_km2); w <- d$area_utm_km2
  r <- sum(w * d$tasa_usada) / A
  # error_tasa: bootstrap de escenas (seccion 2b); error_conservador: media de los
  # errores (correlacion completa, cota superior); error_cuadratura: independencia
  # (cota inferior)
  e_c <- sum(w * d$error_usado) / A; e_q <- sqrt(sum((w * d$error_usado)^2)) / A
  e_b <- sd_boot(cj, d$gid, w)
  dV <- r * A * T; s_dV <- sqrt((e_b * A * T)^2 + (r * S_AREA * A * T)^2)
  dM <- dV * RHO / 1000; s_dM <- sqrt((RHO * s_dV)^2 + (dV * S_RHO)^2) / 1000
  b <- r * RHO / 1000; s_b <- sqrt((e_b * RHO)^2 + (r * S_RHO)^2) / 1000
  r1 <- sum(w * d$tasa_t1_usada) / A; r2 <- sum(w * d$tasa_t2_usada) / A
  e1 <- sd_boot(cj, d$gid, w, "t1"); e2 <- sd_boot(cj, d$gid, w, "t2")
  M1 <- BT[[cj]]$t1[, d$gid, drop = FALSE] %*% w / A; M2 <- BT[[cj]]$t2[, d$gid, drop = FALSE] %*% w / A
  e_dif <- sd(as.vector(M2 - M1), na.rm = TRUE)
  data.frame(conjunto = cj, grupo = grupo, n = nrow(d), n_propio = sum(d$propio),
             area_km2 = A, area_propio_pct = 100 * sum(w[d$propio]) / A,
             tasa_m_a = r, error_tasa = e_b, error_conservador = e_c, error_cuadratura = e_q,
             dV_Mm3 = dV, error_dV = s_dV, dM_Mt_850 = dM, error_dM_850 = s_dM,
             dM_Mt_900 = dV * RHO_ALT / 1000, balance_850 = b, error_balance_850 = s_b,
             balance_900 = r * RHO_ALT / 1000,
             tasa_2001_2013 = r1, error_t1 = e1, tasa_2013_2026 = r2, error_t2 = e2,
             error_t1_conservador = sum(w * d$error_t1_usado) / A, error_t2_conservador = sum(w * d$error_t2_usado) / A,
             error_diferencia_tramos = e_dif, factor_aceleracion = r2 / r1)
}
GR <- list()
for (cj in CONJ) {
  d <- R[R$conjunto == cj, ]
  grupos <- list("Región Metropolitana" = d,
                 "Maipo Alto (0570)" = d[d$COD_SCUEN %in% "0570", ],
                 "Mapocho Alto (0572)" = d[d$COD_SCUEN %in% "0572", ],
                 "Vertiente argentina" = d[d$COD_SCUEN %in% "ARG", ],
                 "Aconcagua y Rapel" = d[substr(d$COD_SCUEN, 1, 3) %in% c("054", "060"), ],
                 "Mayores de 5 km2" = d[d$area_utm_km2 > 5, ],
                 "1 a 5 km2" = d[d$area_utm_km2 > 1 & d$area_utm_km2 <= 5, ],
                 "0,1 a 1 km2" = d[d$area_utm_km2 > 0.1 & d$area_utm_km2 <= 1, ],
                 "Menores de 0,1 km2" = d[d$area_utm_km2 <= 0.1, ],
                 # which(): un glaciar sin cota mediana (sin dato en la base, como seis de la DGA
                 # en el TanDEM-X 90 m) daba filas vacias y el bootstrap se caia
                 "Cota mediana bajo 4.000 m" = d[which(d$zmed < 4000), ],
                 "Cota mediana 4.000 a 4.500 m" = d[which(d$zmed >= 4000 & d$zmed < 4500), ],
                 "Cota mediana sobre 4.500 m" = d[which(d$zmed >= 4500), ])
  # cuenca alta del Maipo y sus seis grupos (Farias-Barahona et al. 2020), con
  # tamano y cota dentro de la cuenca
  dm <- d[d$en_maipo, ]
  grupos[["Cuenca alta del Maipo"]] <- dm
  for (g in ORDEN_GRUPOS) grupos[[paste("Maipo |", g)]] <- d[d$grupo_cuenca %in% g, ]
  grupos[["Maipo | mayores de 5 km2"]] <- dm[dm$area_utm_km2 > 5, ]
  grupos[["Maipo | 1 a 5 km2"]] <- dm[dm$area_utm_km2 > 1 & dm$area_utm_km2 <= 5, ]
  grupos[["Maipo | 0,1 a 1 km2"]] <- dm[dm$area_utm_km2 > 0.1 & dm$area_utm_km2 <= 1, ]
  grupos[["Maipo | menores de 0,1 km2"]] <- dm[dm$area_utm_km2 <= 0.1, ]
  grupos[["Maipo | cota bajo 4.000 m"]] <- dm[which(dm$zmed < 4000), ]
  grupos[["Maipo | cota 4.000 a 4.500 m"]] <- dm[which(dm$zmed >= 4000 & dm$zmed < 4500), ]
  grupos[["Maipo | cota sobre 4.500 m"]] <- dm[which(dm$zmed >= 4500), ]
  grupos[["Fuera de la cuenca del Maipo"]] <- d[!d$en_maipo, ]
  if (cj != "rgi60") for (k in seq_len(nrow(SSC))) {
    s <- d[d$COD_SSCUEN %in% SSC$COD_SSCUEN[k], ]
    grupos[[paste0("Subsubcuenca ", SSC$COD_SSCUEN[k], " ", SSC$nombre_subsubcuenca[k])]] <- s
  }
  for (k in names(grupos)) GR[[length(GR) + 1]] <- agregar(grupos[[k]], k, cj)
  for (s in SIS$sistema) {
    ds <- d[d$sistema %in% s, ]
    if (nrow(ds)) GR[[length(GR) + 1]] <- agregar(ds, paste("Sistema", s), cj)
  }
}
AG <- do.call(rbind, GR)
num <- sapply(AG, is.numeric); AGw <- AG; AGw[num] <- lapply(AGw[num], round, 4)
write.csv(AGw, file.path(dir_out, "M06_Agregados.csv"), row.names = FALSE)
print(AGw[AGw$grupo %in% c("Región Metropolitana", "Maipo Alto (0570)", "Mapocho Alto (0572)", "Vertiente argentina"),
          c("conjunto", "grupo", "n", "area_km2", "tasa_m_a", "error_tasa", "balance_850", "dM_Mt_850",
            "error_dM_850", "tasa_2001_2013", "tasa_2013_2026")], row.names = FALSE)
# Los tres inventarios lado a lado en la cuenca alta del Maipo y sus seis grupos
#: la DGA es el principal, el RGI 7.0 y el RGI 6.0 se comparan, y la
# union queda como el dominio anterior.
TI <- NULL
if ("dga" %in% CONJ) {
  gti <- c("Cuenca alta del Maipo", paste("Maipo |", ORDEN_GRUPOS))
  TI <- AGw[AGw$grupo %in% gti, c("conjunto", "grupo", "n", "n_propio", "area_km2", "area_propio_pct", "tasa_m_a",
                                   "error_tasa", "balance_850", "error_balance_850", "balance_900", "dV_Mm3",
                                   "error_dV", "dM_Mt_850", "error_dM_850", "dM_Mt_900", "tasa_2001_2013", "error_t1",
                                   "tasa_2013_2026", "error_t2")]
  TI <- TI[order(match(TI$grupo, gti), match(TI$conjunto, c("dga", "rgi70", "rgi60", "union"))), ]
  write.csv(TI, file.path(dir_out, "M06_Tres_Inventarios.csv"), row.names = FALSE)
  print(TI[TI$grupo == gti[1], c("conjunto", "n", "area_km2", "tasa_m_a", "error_tasa", "balance_850", "dM_Mt_850",
                                 "tasa_2001_2013", "tasa_2013_2026")], row.names = FALSE)
}

# --- 4b. que aporta cada inventario a la union -------------------------------------------
# La union = glaciares del RGI 7.0 + lo que solo incluye el RGI 6.0. Las dos partes
# se miden con la misma alineacion; la diferencia de volumen entre la union y los
# glaciares del RGI 7.0 es lo que aporta la superficie que solo el RGI 6.0 incluye.
ru <- AG[AG$conjunto == "union" & AG$grupo == "Región Metropolitana", ]
r7 <- AG[AG$conjunto == "rgi70" & AG$grupo == "Región Metropolitana", ]
r6 <- AG[AG$conjunto == "rgi60" & AG$grupo == "Región Metropolitana", ]
UI <- data.frame(parte = c("union", "glaciares del RGI 7.0", "solo en el RGI 6.0", "RGI 6.0 completo"),
                 area_km2 = c(ru$area_km2, r7$area_km2, ru$area_km2 - r7$area_km2, r6$area_km2),
                 dV_Mm3 = c(ru$dV_Mm3, r7$dV_Mm3, ru$dV_Mm3 - r7$dV_Mm3, r6$dV_Mm3))
UI$tasa_m_a <- UI$dV_Mm3 / (UI$area_km2 * T)
UI$pct_volumen_union <- 100 * UI$dV_Mm3 / ru$dV_Mm3
UI$pct_area_union <- 100 * UI$area_km2 / ru$area_km2
# franjas angostas: lo que solo el RGI 6.0 incluye en trozos de menos de 60 m de ancho
s6 <- vect(f_inv, layer = "solo_rgi60")
UI$area_solo_rgi60_sin_franjas_km2 <- sum(expanse(s6, unit = "km"))
write.csv(UI, file.path(dir_out, "M06_Union_Inventarios.csv"), row.names = FALSE)
print(UI, row.names = FALSE)
# Tasas de las unidades segun cuanta superficie de solo RGI 6.0 tienen
Ru <- R[R$conjunto == "union" & R$propio, ]
Ru$frac_solo6 <- Ru$area_solo_rgi60_km2 / Ru$area_utm_km2

# --- 5. contraste con lo publicado -------------------------------------------------------
# Ventana de Dussaillant et al. (2019) y Ferri et al. (2020): escenas hasta abril de 2018
S18 <- SE[SE$anio_dec <= 2018.3, ]
D18 <- do.call(rbind, lapply(split(S18, paste(S18$conjunto, S18$gid)), function(d) {
  a <- ajuste(d, t_ref_de[[paste(d$conjunto[1], d$gid[1])]])
  if (is.null(a)) NULL else data.frame(conjunto = d$conjunto[1], gid = d$gid[1], tasa18 = a$tasa, error18 = a$error, n18 = a$n)
}))
# bootstrap de escenas tambien en la ventana 2018 (sin relleno: solo los ajustados)
BB18 <- boot_tasas(S18, MIN_N, MIN_SPAN, tramos = FALSE, semilla = 2)$tasa
sd18 <- function(cj, gids, w) {
  M <- BB18[, paste(cj, gids), drop = FALSE]
  sd(apply(M, 1, function(z) { ok <- is.finite(z); sum(w[ok] * z[ok]) / sum(w[ok]) }), na.rm = TRUE)
}

# Dussaillant et al. (2019): mismo inventario (RGI 6.0), glaciar por glaciar
# (PANGAEA doi:10.1594/PANGAEA.903618)
du <- read.csv(file.path(dir_c, "datos_externos", "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv"))
names(du) <- c("RGIId", "tile", "MB", "MB_err", "area_km2", "medido_pct", "nb_dem", "sigma_stable", "lat", "lon")
DU <- merge(du, D18[D18$conjunto == "rgi60", c("gid", "tasa18", "error18", "n18")], by.x = "RGIId", by.y = "gid")
DU$MB_propio <- DU$tasa18 * RHO / 1000; DU$MB_propio_err <- DU$error18 * RHO / 1000
DU$dif <- DU$MB_propio - DU$MB; DU$z <- DU$dif / sqrt(DU$MB_err^2 + DU$MB_propio_err^2)
DU$en_maipo <- DU$RGIId %in% A6$RGIId[A6$en_maipo]
DU$grupo_cuenca <- A6$grupo_cuenca[match(DU$RGIId, A6$RGIId)]
write.csv(DU, file.path(dir_out, "M06_Dussaillant_por_glaciar.csv"), row.names = FALSE)
resumen_du <- function(D, grupo) {
  w <- D$area_km2
  data.frame(grupo = grupo, glaciares = nrow(D), area_km2 = sum(w),
             MB_Dussaillant = weighted.mean(D$MB, w), err_Dussaillant = weighted.mean(D$MB_err, w),
             MB_este = weighted.mean(D$MB_propio, w),
             err_este = sqrt((sd18("rgi60", D$RGIId, w) * RHO)^2 + (weighted.mean(D$tasa18, w) * S_RHO)^2) / 1000,
             err_este_conservador = weighted.mean(D$MB_propio_err, w),
             dif_mediana = median(D$dif), dif_nmad = nmad(D$dif),
             pct_dentro_1s = 100 * mean(abs(D$z) <= 1), pct_dentro_2s = 100 * mean(abs(D$z) <= 2),
             correlacion = cor(D$MB, D$MB_propio))
}
DUr <- rbind(resumen_du(DU[DU$en_maipo, ], "Cuenca alta del Maipo"), resumen_du(DU, "Región Metropolitana"))
DUr$n_sigma <- (DUr$MB_este - DUr$MB_Dussaillant) / sqrt(DUr$err_este^2 + DUr$err_Dussaillant^2)
num <- sapply(DUr, is.numeric); DUr[num] <- lapply(DUr[num], round, 3)
write.csv(DUr, file.path(dir_out, "M06_Dussaillant_resumen.csv"), row.names = FALSE)
print(DUr, row.names = FALSE)

# Ventana 2018 por grupo, para Braun (RGI 6.0) y Ferri (vertiente argentina, RGI 7.0,
# que reproduce el inventario argentino que ellos usaron)
R18 <- merge(atr, D18, by = c("conjunto", "gid"))
v18 <- function(d, grupo) {
  w <- d$area_utm_km2
  data.frame(conjunto = d$conjunto[1], grupo = grupo, glaciares = nrow(d), area_km2 = sum(w),
             tasa18 = sum(w * d$tasa18) / sum(w), error18 = sd18(d$conjunto[1], d$gid, w),
             error18_conservador = sum(w * d$error18) / sum(w))
}
V18 <- rbind(v18(R18[R18$conjunto == "union", ], "Región Metropolitana"),
             v18(R18[R18$conjunto == "rgi60", ], "Región Metropolitana"),
             v18(R18[R18$conjunto == "union" & R18$en_maipo, ], "Cuenca alta del Maipo"),
             v18(R18[R18$conjunto == "rgi60" & R18$en_maipo, ], "Cuenca alta del Maipo"),
             v18(R18[R18$conjunto == "rgi70" & R18$COD_SCUEN %in% "ARG", ], "Vertiente argentina"))
V18$balance18 <- V18$tasa18 * RHO / 1000
V18$error_balance18 <- sqrt((V18$error18 * RHO)^2 + (V18$tasa18 * S_RHO)^2) / 1000
write.csv(V18, file.path(dir_out, "M06_Ventana_2018.csv"), row.names = FALSE)
print(V18, row.names = FALSE)

# Echaurren Norte: el unico glaciar con balance medido en terreno (McPhee et al.,
# 2026: -1,57 m w.e./a en 2009-2024). Su unidad de la union y su poligono del RGI 6.0.
id_ech7 <- "RGI2000-v7.0-G-17-25863"; id_ech6 <- "RGI60-17.13715"
uid_ech <- AU$uid[AU$rgi_id %in% id_ech7]
ECH <- do.call(rbind, lapply(list(c("union", uid_ech), c("rgi60", id_ech6), c("rgi70", id_ech7)), function(k) {
  d <- SE[SE$conjunto == k[1] & SE$gid == k[2] & SE$anio_dec >= 2009 & SE$anio_dec < 2025, ]
  dd <- SE[SE$conjunto == k[1] & SE$gid == k[2], ]
  out <- data.frame(conjunto = k[1], gid = k[2], area_km2 = atr$area_utm_km2[atr$conjunto == k[1] & atr$gid == k[2]],
                    n_2009_2024 = nrow(d), n_total = nrow(dd), tasa_2009_2024 = NA, error_2009_2024 = NA)
  if (nrow(d) >= 4 && diff(range(d$anio_dec)) >= 5) {
    s <- summary(lm(dh_m ~ anio_dec, d, weights = 1 / d$NMAD_roca^2))$coefficients
    out$tasa_2009_2024 <- s[2, 1]; out$error_2009_2024 <- s[2, 2]
  }
  out
}))
ECH$balance_2009_2024 <- ECH$tasa_2009_2024 * RHO / 1000
ECH$error_balance_2009_2024 <- ECH$error_2009_2024 * RHO / 1000
write.csv(ECH, file.path(dir_out, "M06_Echaurren.csv"), row.names = FALSE)
print(ECH, row.names = FALSE)

# Farias-Barahona et al. (2020), 2000-2013, por subcuenca del Maipo (Tabla 2), contra
# el primer tramo propio (2001-2013). Ellos usaron el inventario de la DGA (IPG 2014,
# corregido sobre el hielo cubierto): se compara con esos mismos
# glaciares (conjunto dga) y el RGI 7.0 queda en M06_Farias_Barahona_rgi70.csv; sin
# la DGA, con el RGI 7.0, que la reproduce en Chile.
FB0 <- data.frame(subcuenca = c("Olivares", "Colorado", "Yeso", "Volcán", "Maipo alto", "Molina", "San Francisco"),
                  codigos = c("05706", "05705", "05703", "05702", "05700|05701", "05720", "05721"),
                  MB_FB = c(-0.32, 0.05, -0.15, -0.17, -0.16, -0.17, -0.17),
                  err_FB = c(0.13, 0.12, 0.12, 0.13, 0.13, 0.12, 0.13),
                  area_FB = c(71.94, 63.11, 21.58, 50.22, 46.56, 2.02, 3.01))
fb_de <- function(cj) {
  F <- cbind(FB0, do.call(rbind, lapply(FB0$codigos, function(cc) {
    d <- R[R$conjunto == cj & grepl(paste0("^(", cc, ")$"), R$COD_SSCUEN), ]
    w <- d$area_utm_km2
    data.frame(area_este = sum(w), MB_este_t1 = sum(w * d$tasa_t1_usada) / sum(w) * RHO / 1000,
               err_este_t1 = sqrt((sd_boot(cj, d$gid, w, "t1") * RHO)^2 +
                                    (sum(w * d$tasa_t1_usada) / sum(w) * S_RHO)^2) / 1000,
               MB_este_total = sum(w * d$tasa_usada) / sum(w) * RHO / 1000,
               err_este_total = sqrt((sd_boot(cj, d$gid, w) * RHO)^2 +
                                       (sum(w * d$tasa_usada) / sum(w) * S_RHO)^2) / 1000)
  })))
  F$n_sigma <- (F$MB_este_t1 - F$MB_FB) / sqrt(F$err_este_t1^2 + F$err_FB^2)
  F$inventario <- cj
  num <- sapply(F, is.numeric); F[num] <- lapply(F[num], round, 3)
  F
}
FB <- fb_de(if ("dga" %in% CONJ) "dga" else "rgi70")
write.csv(FB, file.path(dir_out, "M06_Farias_Barahona.csv"), row.names = FALSE)
print(FB, row.names = FALSE)
if ("dga" %in% CONJ) write.csv(fb_de("rgi70"), file.path(dir_out, "M06_Farias_Barahona_rgi70.csv"), row.names = FALSE)

# --- 5c. perfil con la cota (union): tasa por banda ---------------------------------------
BU <- merge(BU, SE[SE$conjunto == "union", c("gid", "escena", "anio_dec", "NMAD_roca")], by = c("gid", "escena"))
hipu <- read.csv(file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER", "01_Hipsometria_union.csv"))
PB <- do.call(rbind, lapply(split(BU, paste(BU$gid, BU$banda)), function(d) {
  if (nrow(d) < MIN_N || diff(range(d$anio_dec)) < MIN_SPAN) return(NULL)
  s <- summary(lm(mediana ~ anio_dec, d, weights = 1 / d$NMAD_roca^2))$coefficients
  data.frame(gid = d$gid[1], banda = d$banda[1], tasa = s[2, 1], error = s[2, 2])
}))
PB$area_m2 <- hipu$area_m2[match(paste(PB$gid, PB$banda), paste(hipu$uid, hipu$banda))]
PB <- PB[!is.na(PB$area_m2), ]
PERFIL <- do.call(rbind, lapply(split(PB, PB$banda), function(d)
  data.frame(banda = d$banda[1], glaciares = nrow(d), area_km2 = sum(d$area_m2) / 1e6,
             tasa = weighted.mean(d$tasa, d$area_m2), error = weighted.mean(d$error, d$area_m2))))
PERFIL$area_hipso_km2 <- tapply(hipu$area_m2, hipu$banda, sum)[as.character(PERFIL$banda)] / 1e6
write.csv(round(PERFIL, 3), file.path(dir_out, "M06_Perfil_Cota.csv"), row.names = FALSE)

# --- 6. escenas, nubes, metodos y error con la cota ------------------------------------
PU <- P[P$conjunto == "union", ]
esc_res <- do.call(rbind, lapply(split(PU, PU$escena), function(d)
  data.frame(escena = d$escena[1], anio_dec = d$anio_dec[1], nube_media_pct = d$nube_media_pct[1],
             unidades_vistas = nrow(d), unidades_aceptadas = sum(d$pasa))))
UN$roca_ok <- UN$decision_roca %in% "alineada" & UN$NMAD_roca <= 15
ue <- do.call(rbind, lapply(split(UN, UN$escena), function(d) {
  a <- d[d$decision_roca %in% "alineada", ]
  data.frame(escena = d$escena[1], roca_imposible_mediana_pct = median(d$roca_imposible_pct, na.rm = TRUE),
             NMAD_antes = median(a$NMAD_roca_antes, na.rm = TRUE), NMAD_despues = median(a$NMAD_roca, na.rm = TRUE),
             media_roca = median(a$media_roca, na.rm = TRUE), sd_roca = median(a$sd_roca, na.rm = TRUE),
             RMSE_roca = median(a$RMSE_roca, na.rm = TRUE), desplaz_m = median(a$desplaz_m, na.rm = TRUE),
             unidades_alineadas = nrow(a))
}))
esc_res <- merge(esc_res, ue, by = "escena", all = TRUE)
write.csv(esc_res, file.path(dir_out, "M06_Escenas.csv"), row.names = FALSE)
PU$clase_nube <- cut(PU$nube_media_pct, c(-1, 10, 25, 50, 75, 101), labels = c("0-10 %", "10-25 %", "25-50 %", "50-75 %", "75-100 %"))
NU <- do.call(rbind, lapply(split(PU, PU$clase_nube), function(d)
  data.frame(clase_nube = d$clase_nube[1], escenas = length(unique(d$escena)), pares = nrow(d),
             aceptados = sum(d$pasa), pct_aceptados = 100 * mean(d$pasa))))
write.csv(NU, file.path(dir_out, "M06_Nubes.csv"), row.names = FALSE)
print(NU, row.names = FALSE)
nb <- PU[PU$nube_media_pct > 75 & PU$pasa, ]
NB <- data.frame(escenas_nubladas = length(unique(PU$escena[PU$nube_media_pct > 75])),
                 escenas_con_algun_glaciar_aceptado = length(unique(nb$escena)),
                 pares_aceptados = nrow(nb), glaciares_distintos = length(unique(nb$gid)),
                 pct_aceptados_nube_mas_10 = 100 * mean(PU$nube_media_pct[PU$pasa] > 10))
write.csv(NB, file.path(dir_out, "M06_Escenas_Nubladas.csv"), row.names = FALSE)
print(NB, row.names = FALSE)

# Metodos: cual gana en los pares aceptados de la union y cuanto mejora
AC <- PU[PU$pasa, ]
ME <- as.data.frame(table(metodo = AC$metodo)); ME <- ME[ME$Freq > 0, ]
ME$pct <- round(100 * ME$Freq / sum(ME$Freq), 1)
write.csv(ME, file.path(dir_out, "M06_Metodo_Ganador.csv"), row.names = FALSE)
print(ME[order(-ME$Freq), ], row.names = FALSE)
MT$diverge <- es_v(MT$diverge)
MTv <- MT[!is.na(MT$NMAD), ]
MM <- do.call(rbind, lapply(split(MTv, MTv$metodo), function(d)
  data.frame(metodo = d$metodo[1], ajustes = nrow(d), NMAD_mediana = median(d$NMAD),
             desplaz_mediano = median(d$desplaz_m, na.rm = TRUE), pct_diverge = 100 * mean(d$diverge),
             segundos_medio = mean(d$segundos, na.rm = TRUE))))
fallas <- tapply(is.na(MT$NMAD) & !is.na(MT$falla) & !grepl("insuficiente", MT$falla), MT$metodo, sum)
MM$fallas <- as.numeric(fallas[MM$metodo])
MM <- rbind(data.frame(metodo = "sin alinear", ajustes = sum(UN$decision_roca %in% "alineada"),
                       NMAD_mediana = median(UN$NMAD_roca_antes, na.rm = TRUE),
                       desplaz_mediano = NA, pct_diverge = NA, segundos_medio = NA, fallas = NA), MM)
write.csv(MM, file.path(dir_out, "M06_Metodos_NMAD.csv"), row.names = FALSE)
print(MM, row.names = FALSE)
# Pares en que divergen todas las variantes con Nuth y Kaab: que roca tenian y si se aceptaron
nkv <- c("NuthKaab", "VerticalShift+NuthKaab", "NuthKaab+TerrainBias", "NuthKaab+Deramp",
         "NuthKaab+DirectionalBias", "ICP+NuthKaab")
nk <- MT[MT$metodo %in% nkv & !is.na(MT$NMAD), ]
todos_div <- tapply(nk$diverge, paste(nk$escena, nk$uid), all)
UN$nk_div <- todos_div[paste(UN$escena, UN$uid)]
acept_u <- paste(AC$escena, AC$gid)
NKD <- data.frame(pares = length(todos_div), pares_div = sum(todos_div),
                  nmad_antes_div = median(UN$NMAD_roca_antes[UN$nk_div %in% TRUE], na.rm = TRUE),
                  nmad_antes_conv = median(UN$NMAD_roca_antes[UN$nk_div %in% FALSE], na.rm = TRUE),
                  roca_imposible_div = median(UN$roca_imposible_pct[UN$nk_div %in% TRUE], na.rm = TRUE),
                  aceptados_div = sum(paste(UN$escena, UN$uid)[UN$nk_div %in% TRUE] %in% acept_u))
print(NKD)
# NMAD antes y despues en los mismos pares aceptados, y corrimiento con Nuth y Kaab
ua <- UN[paste(UN$escena, UN$uid) %in% acept_u, ]
NMA <- data.frame(nmad_antes = median(ua$NMAD_roca_antes, na.rm = TRUE), nmad_despues = median(ua$NMAD_roca, na.rm = TRUE),
                  despl_mediano_nk = median(ua$desplaz_m[grepl("NuthKaab", ua$metodo)], na.rm = TRUE),
                  pares_aceptados = nrow(ua))
print(NMA)
# CPD en la muestra de M05b, junto a los otros trece
CP <- leer(file.path(dir_c, "M05b_salida"), "\\.csv$")
CPr <- NULL
if (!is.null(CP) && nrow(CP)) {
  CP$diverge <- es_v(CP$diverge)
  CP$par <- paste(CP$escena, CP$uid)
  okc <- CP[!is.na(CP$NMAD) & !CP$diverge, ]
  # Solo los pares cuya roca pasaria el control (la mejor variante deja NMAD <= 15 m):
  # en los demas la roca esta bajo nube y ningun metodo la recupera.
  mejor_par <- tapply(okc$NMAD, okc$par, min)
  validos <- names(mejor_par)[mejor_par <= 15]
  okv <- okc[okc$par %in% validos, ]
  gan <- okv[ave(okv$NMAD, okv$par, FUN = function(x) x == min(x)) == 1, ]
  CPr <- do.call(rbind, lapply(split(CP, CP$metodo), function(d) {
    dv <- d[d$par %in% validos, ]
    data.frame(metodo = d$metodo[1], pares = length(validos), pares_muestra = length(unique(d$par)),
               NMAD_mediana = median(dv$NMAD[!dv$diverge], na.rm = TRUE),
               pct_diverge = 100 * mean(dv$diverge %in% TRUE | is.na(dv$NMAD)),
               segundos_mediana = median(d$segundos, na.rm = TRUE), segundos_max = max(d$segundos, na.rm = TRUE),
               segundos_total = sum(d$segundos, na.rm = TRUE), gana = sum(gan$metodo == d$metodo[1]))
  }))
  write.csv(CPr, file.path(dir_out, "M06_Muestra_CPD.csv"), row.names = FALSE)
  print(CPr, row.names = FALSE)
}

# Error con la cota: NMAD de la roca e imposibles del hielo por banda de 100 m
okp <- paste(UN$escena, UN$uid)[UN$roca_ok]
ERr <- ER[paste(ER$escena, ER$uid) %in% okp, ]
EC1 <- do.call(rbind, lapply(split(ERr, ERr$banda), function(d)
  data.frame(banda = d$banda[1], ventanas = nrow(d), px = sum(d$n), NMAD_mediana = median(d$NMAD),
             NMAD_p90 = unname(quantile(d$NMAD, 0.9)), mediana_mediana = median(d$mediana))))
EC2 <- do.call(rbind, lapply(split(HC, HC$banda), function(d)
  data.frame(banda = d$banda[1], px = sum(d$n), imposibles_pct = 100 * sum(d$n_imp) / sum(d$n))))
EC <- merge(EC1, EC2, by = "banda", all = TRUE, suffixes = c("_roca", "_hielo"))
write.csv(round(EC, 3), file.path(dir_out, "M06_Error_Cota.csv"), row.names = FALSE)
print(round(EC, 2), row.names = FALSE)

# --- 6b. lo mismo por grupo de la cuenca, con la hipsometria de fondo ---------------------
# (una linea por subcuenca, la mediana del Maipo con su rango intercuartil, y de
# fondo cuanta superficie glaciar hay a cada cota)
gr_u <- setNames(AU$grupo_cuenca, AU$uid); mp_u <- setNames(AU$en_maipo, AU$uid)
ERr$grupo <- gr_u[ERr$uid]; HC$grupo <- gr_u[HC$gid]
ec_grupo <- function(er, hc, g) {
  a <- do.call(rbind, lapply(split(er, er$banda), function(d)
    data.frame(banda = d$banda[1], px_roca = sum(d$n), NMAD_mediana = median(d$NMAD),
               NMAD_q25 = unname(quantile(d$NMAD, 0.25)), NMAD_q75 = unname(quantile(d$NMAD, 0.75)))))
  b <- do.call(rbind, lapply(split(hc, hc$banda), function(d)
    data.frame(banda = d$banda[1], px_hielo = sum(d$n), imposibles_pct = 100 * sum(d$n_imp) / sum(d$n))))
  m <- merge(a, b, by = "banda", all = TRUE); m$grupo <- g; m
}
ECG <- rbind(ec_grupo(ERr[mp_u[ERr$uid] %in% TRUE, ], HC[mp_u[HC$gid] %in% TRUE, ], "Cuenca alta del Maipo"),
             do.call(rbind, lapply(ORDEN_GRUPOS, function(g)
               ec_grupo(ERr[ERr$grupo %in% g, ], HC[HC$grupo %in% g, ], g))))
hip_m <- hipu[hipu$uid %in% AU$uid[AU$en_maipo], ]
HIPM <- aggregate(area_m2 ~ banda, hip_m, sum); HIPM$area_km2 <- HIPM$area_m2 / 1e6
ECG$area_hielo_maipo_km2 <- HIPM$area_km2[match(ECG$banda, HIPM$banda)]
write.csv(ECG, file.path(dir_out, "M06_Error_Cota_Grupos.csv"), row.names = FALSE)

# --- 6c. descripcion de la cuenca y sus subcuencas -----------------------------------------
# (numero de glaciares, que no es el de poligonos; area y elevacion con cuartiles,
# media y DE; tipo de glaciar segun el inventario chileno)
# El numero de glaciares es el del Inventario Publico de Glaciares 2022 de la DGA:
# un glaciar es un codigo COD_GLA sin su letra final (Echaurren Norte, por
# ejemplo, son tres poligonos 010@, 011A y 011B). El campo CUBIERTO del IPG viene
# vacio en esta zona: el tipo que si trae es CLASIFICA (de valle, de montana,
# glaciarete, rocoso).
ipg <- as.data.frame(IPG)
ipg_all <- as.data.frame(vect(f_ipg, extent = ext(-70.9, -69.4, -34.7, -32.5)))
ipg_all$grupo <- grupo_de(ipg_all$COD_SCUEN, ipg_all$COD_SSCUEN)
ipg_all$en_maipo <- ipg_all$COD_SCUEN %in% c("0570", "0572")
ipg_all$raiz <- sub(".$", "", ipg_all$COD_GLA)
q <- function(x, p) unname(quantile(x, p, na.rm = TRUE))
desc_grupo <- function(u, ig, g) {
  noroc <- ig[ig$CLASIFICA != "GLACIAR ROCOSO", ]
  at <- tapply(ig$AREA_KM2, ig$CLASIFICA, sum)
  data.frame(grupo = g, unidades = nrow(u), area_union_km2 = sum(u$area_utm_km2),
             area_q25 = q(u$area_utm_km2, .25), area_q50 = q(u$area_utm_km2, .5), area_q75 = q(u$area_utm_km2, .75),
             area_media = mean(u$area_utm_km2), area_de = sd(u$area_utm_km2),
             cota_q25 = q(u$zmed, .25), cota_q50 = q(u$zmed, .5), cota_q75 = q(u$zmed, .75),
             cota_media = mean(u$zmed, na.rm = TRUE), cota_de = sd(u$zmed, na.rm = TRUE),
             glaciares_ipg = length(unique(noroc$raiz)), poligonos_ipg = nrow(noroc),
             rocosos_ipg = length(unique(ig$raiz[ig$CLASIFICA == "GLACIAR ROCOSO"])),
             area_ipg_km2 = sum(ig$AREA_KM2),
             pct_valle = 100 * sum(at[names(at) == "GLACIAR DE VALLE"]) / sum(ig$AREA_KM2),
             pct_montana = 100 * sum(at[grepl("MONTA", names(at))]) / sum(ig$AREA_KM2),
             pct_glaciarete = 100 * sum(at[names(at) == "GLACIARETE"]) / sum(ig$AREA_KM2),
             pct_rocoso = 100 * sum(at[names(at) == "GLACIAR ROCOSO"]) / sum(ig$AREA_KM2))
}
u0 <- R[R$conjunto == "union", ]
DESC <- rbind(desc_grupo(u0[u0$en_maipo, ], ipg_all[ipg_all$en_maipo, ], "Cuenca alta del Maipo"),
              do.call(rbind, lapply(ORDEN_GRUPOS, function(g)
                desc_grupo(u0[u0$grupo_cuenca %in% g, ], ipg_all[ipg_all$grupo %in% g, ], g))))
write.csv(DESC, file.path(dir_out, "M06_Subcuencas_Descripcion.csv"), row.names = FALSE)
print(DESC[, c("grupo", "unidades", "area_union_km2", "glaciares_ipg", "rocosos_ipg", "cota_q50")], row.names = FALSE)

# --- 6d. glaciares piloto ------------------
# Se busca cada uno por su nombre en el IPG 2022 y se toma la unidad de la union que
# mas se le superpone. "Estrecho", que tambien nombra, no figura con ese nombre en
# el IPG 2022 de la cuenca.
PILOTO <- data.frame(nombre = c("Olivares Alfa", "Olivares Beta", "Olivares Gama", "Juncal Sur", "Bello", "Yeso",
                                "Echaurren Norte", "San Francisco"),
                     ipg = c("OLIVARES ALFA", "OLIVARES BETA A", "OLIVARES GAMMA A", "JUNCAL SUR", "BELLO A", "YESO",
                             "ECHAURREN NORTE", "SAN FRANCISCO"),
                     ssc = c("05706", "05706", "05706", "05706", "05703", "05703", "05703", "05702"))
PIL <- do.call(rbind, lapply(seq_len(nrow(PILOTO)), function(k) {
  g <- IPG[IPG$NOMBRE == PILOTO$ipg[k] & IPG$COD_SSCUEN == PILOTO$ssc[k]]
  if (!nrow(g)) return(NULL)
  x <- intersect(VU[, "uid"], g[, "NOMBRE"]); a <- expanse(x)
  uid <- x$uid[which.max(a)]
  r <- R[R$conjunto == "union" & R$gid == uid, ]
  data.frame(nombre = PILOTO$nombre[k], grupo = r$grupo_cuenca, uid = uid, area_ipg_km2 = expanse(g, unit = "km"),
             area_unidad_km2 = r$area_utm_km2, escenas = r$n, propio = r$propio, tasa = r$tasa_usada,
             error = r$error_usado, B_m = r$B_m, balance_850 = r$tasa_usada * RHO / 1000,
             balance_900 = r$tasa_usada * RHO_ALT / 1000, error_balance_850 = sqrt((r$error_usado * RHO)^2 +
                                                                                    (r$tasa_usada * S_RHO)^2) / 1000,
             tasa_t1 = r$tasa_t1_usada, error_t1 = r$error_t1_usado, tasa_t2 = r$tasa_t2_usada,
             error_t2 = r$error_t2_usado, anio_ref = r$anio_ref)
}))
write.csv(PIL, file.path(dir_out, "M06_Glaciares_Piloto.csv"), row.names = FALSE)
print(PIL[, c("nombre", "uid", "area_unidad_km2", "escenas", "tasa", "error")], row.names = FALSE)

# --- 6e. registro por glaciar y escena -------------------------------
# Una fila por unidad de la union y escena que llego a la serie: fecha, mes, metodo
# de coregistracion, NMAD de la roca, cambio contra la primera fecha y su volumen y
# masa con las dos densidades, y si se uso, si es la referencia o por que se aparto.
# Sirve para ver si un quiebre de la serie coincide con el mes, el metodo o la escena.
SR <- S[S$conjunto == "union", ]
SR <- merge(SR, P[P$conjunto == "union", c("gid", "escena", "metodo", "NMAD_roca", "desplaz_m", "nube_media_pct")],
            by = c("gid", "escena"), all.x = TRUE)
SR$nombre <- AU$nombre[match(SR$gid, AU$uid)]; SR$grupo_cuenca <- gr_u[SR$gid]
SR$area_km2 <- AU$area_utm_km2[match(SR$gid, AU$uid)]
SR$fecha <- substr(SR$escena, 1, 10); SR$anio <- as.integer(substr(SR$escena, 1, 4))
SR$mes <- c("enero", "febrero", "marzo", "abril")[as.integer(substr(SR$escena, 6, 7))]
SR$uso <- ifelse(SR$es_referencia %in% TRUE, "referencia",
                 ifelse(SR$motivo_serie %in% "ok", "usada", SR$motivo_serie))
SR$dV_Mm3 <- SR$dh_m * SR$area_km2
SR$dM_Mt_850 <- SR$dV_Mm3 * RHO / 1000; SR$dM_Mt_900 <- SR$dV_Mm3 * RHO_ALT / 1000
SR <- SR[order(SR$gid, SR$escena), c("gid", "nombre", "grupo_cuenca", "area_km2", "escena", "fecha", "anio", "mes",
                                     "uso", "metodo", "NMAD_roca", "desplaz_m", "nube_media_pct", "anio_ref", "dh_m",
                                     "dV_Mm3", "dM_Mt_850", "dM_Mt_900")]
num <- sapply(SR, is.numeric); SR[num] <- lapply(SR[num], round, 4)
write.csv(SR, file.path(dir_out, "M06_Serie_por_Glaciar.csv"), row.names = FALSE)

saveRDS(list(P = P, S = S, SE = SE, REF = REF, R = R, AG = AG, UI = UI, DU = DU, DUr = DUr, FB = FB,
             PERFIL = PERFIL, NU = NU, NB = NB, ME = ME, MM = MM, EC = EC, esc_res = esc_res, SIS = SIS,
             SSC = SSC, T = T, V18 = V18, ECH = ECH, Q = Q, AU = AU, A7 = A7, A6 = A6, NKD = NKD, NMA = NMA,
             CPr = CPr, UN = UN, ECG = ECG, DESC = DESC, PIL = PIL, SR = SR, ORDEN_GRUPOS = ORDEN_GRUPOS,
             AD = AD, TI = TI, CONJ = CONJ),
        file.path(if (SUF == "") dir_c else dir_out, "M06_resultados.rds"))
cat("Listo.\n")
