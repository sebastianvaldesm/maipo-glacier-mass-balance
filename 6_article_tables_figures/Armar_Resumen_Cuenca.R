# Resumen de las metodologias por cuenca, subcuencas y glaciares piloto, y la incertidumbre de la cuenca
# calculada de otras formas sobre la metodologia final (Tabla A3 del articulo).
#
# Una fila = un inventario, una unidad y una variable. El ajuste y el relleno son los de M06. Las
# columnas MM_GDEM_E_* repiten la metodologia final con otra forma de combinar los errores:
#   E_INDEP        glaciares independientes, raiz de la suma de cuadrados
#   E_BOOT         remuestreo de escenas de M06 (500 sorteos)
#   E_COVAR        doble suma de covarianzas (Hugonnet et al., 2021, ec. 6), con la correlacion
#                  entre glaciares medida en los residuos de sus series
#   E_CELDAS       suma dentro de celdas de 55 km y cuadratura entre ellas (Shean et al., 2020)
#   E_ROLSTAD500   error de cada glaciar con Rolstad sobre el NMAD de la roca, L = 500 m
#   E_ROLSTAD1000  lo mismo con L = 1 km
#   E_TRIANG       la unidad como un solo cuerpo de hielo mas el error de triangulacion
#                  (Dussaillant et al., 2019)
# Uso: Rscript Armar_Resumen_Cuenca.R
# Sale: Resumen_Cuenca_Subcuencas_Piloto.csv
options(stringsAsFactors = FALSE, warn = 1)
suppressPackageStartupMessages(library(terra))
RAIZ <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
AQUI <- file.path(RAIZ, "06_Memoria_Final", "Metodologias_anteriores")
TRES <- file.path(RAIZ, "06_Memoria_Final", "Metodologias_anteriores", "datos", "ast14dem", "tablas")
MM   <- "C:/Users/sebas/Prueba_MMASTER/cadena_propia"
VAR  <- "C:/Users/sebas/Prueba_MMASTER/variantes_m05c"
INV  <- file.path(RAIZ, "01_Datos", "Procesados", "Inventarios_UTM19S")

DIRS <- c(AST_13M_ALT50 = file.path(VAR, "AST_13M_ALT50"), AST_13M = TRES,
          AST_13M_TODOS_MESES = file.path(VAR, "AST_13M_TODOS_MESES"),
          AST_13M_MAR_PRIMERO = file.path(VAR, "AST_13M_MAR_PRIMERO"),
          AST_13M_SOLO_ENE = file.path(VAR, "AST_13M_SOLO_ENE"),
          AST_13M_SOLO_FEB = file.path(VAR, "AST_13M_SOLO_FEB"),
          AST_13M_SOLO_MARABR = file.path(VAR, "AST_13M_SOLO_MARABR"),
          AST_NKD = file.path(VAR, "AST_NKD"),
          MM_TDX90_13M = file.path(MM, "tdx90", "tablas"), MM_GDEM_13M = file.path(MM, "gdem", "tablas"))
FINAL <- "MM_GDEM_13M"
CONJ <- c("dga", "rgi70", "rgi60")
GRUPOS <- c("Mapocho", "Olivares", "Colorado", "Yeso", "Volcán", "Cabecera del Maipo")
T_AN <- 25; T_CORTE <- 2013.5; RHO <- 850; S_RHO <- 60; S_AREA <- 0.05; MIN_N <- 5; MIN_SPAN <- 10
N_BOOT <- 500

nmad <- function(x) { x <- x[is.finite(x)]; 1.4826 * median(abs(x - median(x))) }
ATR <- read.csv(file.path(MM, "gdem", "tablas", "M06_Tasa_por_Glaciar.csv"))
ATR <- ATR[ATR$conjunto %in% CONJ, c("conjunto", "gid", "area_utm_km2", "sistema", "grupo_cuenca", "en_maipo")]
ATR$en_maipo <- ATR$en_maipo %in% c(TRUE, "True", "TRUE")

# --- los piloto en cada inventario ---------------------------------------------------------
PILOTO <- data.frame(nombre = c("Olivares Alfa", "Olivares Beta", "Olivares Gama", "Juncal Sur", "Bello", "Yeso",
                                "Echaurren Norte", "San Francisco"),
                     ipg = c("OLIVARES ALFA", "OLIVARES BETA A", "OLIVARES GAMMA A", "JUNCAL SUR", "BELLO A", "YESO",
                             "ECHAURREN NORTE", "SAN FRANCISCO"),
                     ssc = c("05706", "05706", "05706", "05706", "05703", "05703", "05703", "05702"))
IPG <- project(vect(file.path(RAIZ, "01_Datos/Brutos/Inventario_DGA/IPG_2022_v2/INV_PG_2022_v2.shp"),
                    extent = ext(-70.9, -69.4, -34.7, -32.5)), "EPSG:32719")
CAPAS <- list(dga = list(vect(file.path(INV, "Inventario_DGA_2014_cuenca.gpkg"), layer = "dga"), "gid"),
              rgi70 = list(vect(file.path(INV, "Inventarios_RM_ASTER.gpkg"), layer = "rgi70"), "rgi_id"),
              rgi60 = list(vect(file.path(INV, "Inventarios_RM_ASTER.gpkg"), layer = "rgi60"), "RGIId"))
PIL <- do.call(rbind, lapply(CONJ, function(cj) do.call(rbind, lapply(seq_len(nrow(PILOTO)), function(k) {
  g <- IPG[IPG$NOMBRE == PILOTO$ipg[k] & IPG$COD_SSCUEN == PILOTO$ssc[k]]
  v <- CAPAS[[cj]][[1]]; idc <- CAPAS[[cj]][[2]]
  x <- suppressWarnings(intersect(v[, idc], g[, "NOMBRE"]))
  if (!nrow(x)) return(data.frame(conjunto = cj, unidad = PILOTO$nombre[k], gid = "", area_ipg = sum(expanse(g, unit = "km")), comun = 0))
  a <- expanse(x, unit = "km")
  data.frame(conjunto = cj, unidad = PILOTO$nombre[k], gid = as.character(x[[idc]][[1]])[which.max(a)],
             area_ipg = sum(expanse(g, unit = "km")), comun = max(a))
}))))
PIL <- merge(PIL, ATR[, c("conjunto", "gid", "area_utm_km2")], by = c("conjunto", "gid"), all.x = TRUE)
cat("Piloto por inventario (area del IPG 2022, area comun y area del poligono, km2):\n")
print(PIL[order(PIL$conjunto, PIL$unidad), c("conjunto", "unidad", "gid", "area_ipg", "comun", "area_utm_km2")], digits = 3, row.names = FALSE)
NOTA <- function(cj, u) {
  if (cj == "rgi60" && u %in% c("Olivares Beta", "Olivares Gama")) return("En el RGI 6.0 Olivares Beta y Olivares Gama son un solo poligono")
  if (cj == "rgi60" && u %in% c("Bello", "Yeso")) return("En el RGI 6.0 Bello y Yeso son un solo poligono")
  if (u != "Echaurren Norte") return("")
  if (cj == "dga") return("Sin poligono: en el IPG 2014 es glaciar rocoso (CL105703010) y la capa DGA no los incluye")
  if (cj == "rgi70") return("Sin poligono: ninguno del RGI 7.0 cruza el contorno del IPG 2022")
  ""
}

# --- ajuste y relleno, iguales a M06 -------------------------------------------------------
pendiente <- function(d) {
  s <- summary(lm(dh_m ~ anio_dec, d, weights = 1 / d$NMAD_roca^2))$coefficients
  if (nrow(s) < 2) c(NA, NA) else s[2, 1:2]
}
rellenar <- function(R) {
  for (v in c("", "_t1", "_t2")) {
    tv <- paste0("tasa", v); ev <- paste0("error", v); R[[paste0(tv, "_u")]] <- R[[tv]]; R[[paste0(ev, "_u")]] <- R[[ev]]
    for (cj in CONJ) {
      s <- R$conjunto == cj
      for (k in which(s & is.na(R[[tv]]))) {
        p <- s & !is.na(R[[tv]]) & R$sistema %in% R$sistema[k]
        if (sum(p) < 2) p <- s & !is.na(R[[tv]])
        R[[paste0(tv, "_u")]][k] <- weighted.mean(R[[tv]][p], R$area_utm_km2[p])
        R[[paste0(ev, "_u")]][k] <- max(nmad(R[[tv]][p]), weighted.mean(R[[ev]][p], R$area_utm_km2[p]))
      }
    }
  }
  R
}
ajustar <- function(dir) {
  P <- read.csv(file.path(dir, "M05c_Pares.csv")); S <- read.csv(file.path(dir, "M05c_Serie.csv"))
  P <- P[P$conjunto %in% CONJ, c("conjunto", "gid", "escena", "NMAD_roca")]; S <- S[S$conjunto %in% CONJ, ]
  ref <- S$es_referencia %in% c(TRUE, "True", "TRUE")
  t0 <- unique(S[!is.na(S$t_ref), c("conjunto", "gid", "t_ref")])
  SE <- merge(S[!ref & S$motivo_serie %in% "ok", c("conjunto", "gid", "escena", "anio_dec", "dh_m")], P,
              by = c("conjunto", "gid", "escena"))
  SE <- merge(SE, t0, by = c("conjunto", "gid"))
  Q <- do.call(rbind, lapply(split(SE, paste(SE$conjunto, SE$gid)), function(d) {
    if (nrow(d) < MIN_N || (max(d$anio_dec) - d$t_ref[1]) < MIN_SPAN) return(NULL)
    p <- pendiente(d); if (is.na(p[1])) return(NULL)
    o <- data.frame(conjunto = d$conjunto[1], gid = d$gid[1], n = nrow(d), tasa = p[1], error = p[2],
                    tasa_t1 = NA_real_, error_t1 = NA_real_, tasa_t2 = NA_real_, error_t2 = NA_real_)
    for (k in 1:2) {
      dd <- if (k == 1) d[d$anio_dec < T_CORTE, ] else d[d$anio_dec >= T_CORTE, ]
      if (nrow(dd) >= 4 && diff(range(dd$anio_dec)) >= 5) {
        q <- pendiente(dd); o[[paste0("tasa_t", k)]] <- q[1]; o[[paste0("error_t", k)]] <- q[2]
      }
    }
    o
  }))
  R <- merge(ATR, Q, by = c("conjunto", "gid"), all.x = TRUE); R$propio <- !is.na(R$tasa)
  list(R = rellenar(R), SE = SE)
}

# --- las variables de una unidad -----------------------------------------------------------
balance <- function(r, e) sqrt((e * RHO)^2 + (r * RHO * S_AREA)^2 + (r * S_RHO)^2) / 1000
VARIABLES <- c("tasa_m_a", "error_tasa_m_a", "balance_850_m_we_a", "error_balance_850_m_we_a", "masa_850_Gt",
               "error_masa_850_Gt", "tasa_2001_2013_m_a", "error_2001_2013_m_a", "tasa_2013_2026_m_a",
               "error_2013_2026_m_a", "escenas_mediana", "area_con_ajuste_pct")
media <- function(d, col) sum(d$area_utm_km2 * d[[col]]) / sum(d$area_utm_km2)
resumen <- function(d, suf = "_u", agg = media) {
  w <- d$area_utm_km2; A <- sum(w); m <- function(v) sum(w * d[[paste0(v, suf)]]) / A
  r <- m("tasa"); e <- agg(d, paste0("error", suf)); b <- r * RHO / 1000; eb <- balance(r, e)
  c(r, e, b, eb, b * A * T_AN / 1000, eb * A * T_AN / 1000, m("tasa_t1"), agg(d, paste0("error_t1", suf)),
    m("tasa_t2"), agg(d, paste0("error_t2", suf)), median(d$n, na.rm = TRUE), 100 * sum(w[d$propio]) / A)
}
UNIDADES <- do.call(rbind, lapply(CONJ, function(cj) {
  a <- ATR[ATR$conjunto == cj, ]
  rbind(data.frame(conjunto = cj, tipo = "cuenca", unidad = "Cuenca alta del Maipo", glaciar = "",
                   glaciares = sum(a$en_maipo), area_km2 = sum(a$area_utm_km2[a$en_maipo]), nota = ""),
        do.call(rbind, lapply(GRUPOS, function(g) data.frame(conjunto = cj, tipo = "subcuenca", unidad = g, glaciar = "",
          glaciares = sum(a$grupo_cuenca %in% g), area_km2 = sum(a$area_utm_km2[a$grupo_cuenca %in% g]), nota = ""))),
        do.call(rbind, lapply(PILOTO$nombre, function(p) { q <- PIL[PIL$conjunto == cj & PIL$unidad == p, ]
          data.frame(conjunto = cj, tipo = "piloto", unidad = p, glaciar = q$gid, glaciares = as.numeric(nzchar(q$gid)),
                     area_km2 = q$area_utm_km2, nota = NOTA(cj, p)) })))
}))
# agg_de(u) devuelve la forma de agregar de la unidad u
columna <- function(R, agg_de = function(u) media) {
  unlist(lapply(seq_len(nrow(UNIDADES)), function(i) {
    u <- UNIDADES[i, ]; d <- R[R$conjunto == u$conjunto, ]; a <- agg_de(u)
    if (u$tipo == "cuenca") return(resumen(d[d$en_maipo, ], "_u", a))
    if (u$tipo == "subcuenca") return(resumen(d[d$grupo_cuenca %in% u$unidad, ], "_u", a))
    if (!nzchar(u$glaciar)) return(rep(NA, 12))
    d <- d[d$gid == u$glaciar, ]
    if (!nrow(d) || !d$propio) return(c(rep(NA, 11), 0))
    resumen(d, "", a)
  }))
}

X <- UNIDADES[rep(seq_len(nrow(UNIDADES)), each = length(VARIABLES)), ]
X$variable <- rep(VARIABLES, nrow(UNIDADES))
AJ <- list()
for (m in names(DIRS)) {
  AJ[[m]] <- ajustar(DIRS[[m]]); R <- AJ[[m]]$R
  X[[m]] <- columna(R)
  b <- R[R$conjunto == "dga" & R$en_maipo, ]
  cat(sprintf("%-20s cuenca DGA: %.3f +- %.3f m/a | %d de %d glaciares con ajuste\n", m, resumen(b)[1], resumen(b)[2], sum(b$propio), nrow(b)))
}

# --- las formas de medir el error, sobre la metodologia final -------------------------------
R0 <- AJ[[FINAL]]$R; SE <- AJ[[FINAL]]$SE
key <- function(d) paste(d$conjunto, d$gid)
XY <- lapply(setNames(CONJ, CONJ), function(cj) {
  v <- CAPAS[[cj]][[1]]; p <- crds(centroids(v)) / 1000
  rownames(p) <- as.character(v[[CAPAS[[cj]][[2]]]][[1]]); p
})

# E_INDEP
indep <- function(d, col) sqrt(sum((d$area_utm_km2 * d[[col]])^2)) / sum(d$area_utm_km2)

# E_COVAR: rho(d) de los residuos de la serie, por inventario
BINS <- c(0, 2, 5, 10, 20, 30, 50, 75, 100, 200)
RHO_D <- lapply(setNames(CONJ, CONJ), function(cj) {
  gs <- R0$gid[R0$conjunto == cj & R0$en_maipo & R0$propio]
  S1 <- SE[SE$conjunto == cj & SE$gid %in% gs, ]
  res <- Filter(Negate(is.null), lapply(split(S1, S1$gid), function(d) {
    if (nrow(d) < 6) return(NULL)
    setNames(residuals(lm(dh_m ~ anio_dec, d, weights = 1 / d$NMAD_roca^2)), d$escena)
  }))
  esc <- sort(unique(unlist(lapply(res, names))))
  M <- matrix(NA_real_, length(esc), length(res), dimnames = list(esc, names(res)))
  for (g in names(res)) M[names(res[[g]]), g] <- res[[g]]
  C <- suppressWarnings(cor(M, use = "pairwise.complete.obs")); N <- crossprod(1 * !is.na(M)); C[N < 10] <- NA
  D <- as.matrix(dist(XY[[cj]][colnames(M), ]))
  u <- upper.tri(C) & is.finite(C)
  b <- cut(D[u], BINS); t <- tapply(C[u], b, mean); n <- tapply(C[u], b, length)
  ok <- is.finite(t)
  cat(sprintf("rho(d) %-6s %s\n", cj, paste(sprintf("%s:%.2f", names(t)[ok], t[ok]), collapse = " ")))
  list(x = ((BINS[-1] + BINS[-length(BINS)]) / 2)[ok], y = as.numeric(t[ok]))
})
covar <- function(d, col) {
  cj <- d$conjunto[1]; v <- d$area_utm_km2 * d[[col]] / sum(d$area_utm_km2)
  if (nrow(d) == 1) return(abs(v))
  D <- as.matrix(dist(XY[[cj]][d$gid, ])); f <- RHO_D[[cj]]
  P <- matrix(pmin(pmax(approx(f$x, f$y, xout = D, rule = 2)$y, 0), 1), nrow(D)); diag(P) <- 1
  as.numeric(sqrt(t(v) %*% P %*% v))
}

# E_CELDAS: cuadrados de 55 km, promedio de nueve origenes de la malla
celdas <- function(d, col, lado = 55) {
  p <- XY[[d$conjunto[1]]][d$gid, , drop = FALSE]; v <- d$area_utm_km2 * d[[col]] / sum(d$area_utm_km2)
  o <- c(0, lado / 3, 2 * lado / 3)
  mean(apply(expand.grid(o, o), 1, function(s) {
    k <- paste(floor((p[, 1] + s[1]) / lado), floor((p[, 2] + s[2]) / lado))
    sqrt(sum(tapply(v, k, sum)^2))
  }))
}

# error de la pendiente con NMAD de la roca como error de cada escena (factor de Rolstad = 1)
E1 <- do.call(rbind, lapply(split(SE, paste(SE$conjunto, SE$gid)), function(d) {
  e <- function(q) { w <- 1 / q$NMAD_roca^2; tb <- sum(w * q$anio_dec) / sum(w); 1 / sqrt(sum(w * (q$anio_dec - tb)^2)) }
  a <- d[d$anio_dec < T_CORTE, ]; b <- d[d$anio_dec >= T_CORTE, ]
  data.frame(k = paste(d$conjunto[1], d$gid[1]), e1 = e(d), e1_t1 = if (nrow(a) >= 4) e(a) else NA, e1_t2 = if (nrow(b) >= 4) e(b) else NA)
}))
i1 <- match(key(R0), E1$k)
factor_rolstad <- function(A, L) ifelse(A > pi * L^2, sqrt(pi * L^2 / (5 * A)), 1)
con_rolstad <- function(L) {
  R <- R0; f <- factor_rolstad(R$area_utm_km2, L)
  R$error <- ifelse(is.na(R0$error), NA, E1$e1[i1] * f)
  R$error_t1 <- ifelse(is.na(R0$error_t1), NA, E1$e1_t1[i1] * f)
  R$error_t2 <- ifelse(is.na(R0$error_t2), NA, E1$e1_t2[i1] * f)
  rellenar(R)
}

# E_TRIANG: un solo cuerpo de hielo mas la triangulacion, que no se define por tramos
TRI <- sapply(setNames(CONJ, CONJ), function(cj) {
  d <- R0[R0$conjunto == cj & R0$en_maipo & R0$propio & !is.na(R0$tasa_t1) & !is.na(R0$tasa_t2), ]
  quantile(abs((d$tasa_t1 + d$tasa_t2) / 2 - d$tasa), 0.68, names = FALSE)   # en m/anio
})
cat("Triangulacion (percentil 68, m w.e./anio):", paste(names(TRI), round(TRI * RHO / 1000, 3)), "\n")
R0$e1 <- E1$e1[i1]
triang <- function(d, col) {
  if (grepl("_t[12]", col)) return(NA_real_)
  a <- is.finite(d$e1); A <- sum(d$area_utm_km2)
  e <- weighted.mean(d$e1[a], d$area_utm_km2[a]) * factor_rolstad(A, 0.5)
  sqrt(e^2 + TRI[[d$conjunto[1]]]^2)
}

# E_BOOT: los agregados vienen de M06; los piloto se sortean aqui, con el mismo procedimiento
AG <- read.csv(file.path(DIRS[[FINAL]], "M06_Agregados.csv"))
set.seed(1)
esc <- sort(unique(SE$escena)); CNT <- replicate(N_BOOT, tabulate(sample.int(length(esc), length(esc), replace = TRUE), length(esc)))
pend_w <- function(x, y, w) { s0 <- sum(w); sx <- sum(w * x); (s0 * sum(w * x * y) - sx * sum(w * y)) / (s0 * sum(w * x * x) - sx^2) }
boot_glaciar <- function(cj, gid) {
  d <- SE[SE$conjunto == cj & SE$gid == gid, ]; ie <- match(d$escena, esc); w0 <- 1 / d$NMAD_roca^2
  r <- apply(CNT, 2, function(n) {
    k <- n[ie]; u <- k > 0; o <- c(NA, NA, NA)
    if (sum(u) >= MIN_N && max(d$anio_dec[u] - d$t_ref[u]) >= MIN_SPAN) o[1] <- pend_w(d$anio_dec, d$dh_m, w0 * k)
    for (j in 1:2) {
      m <- if (j == 1) d$anio_dec < T_CORTE else d$anio_dec >= T_CORTE
      if (sum(u & m) >= 4 && diff(range(d$anio_dec[u & m])) >= 5) o[j + 1] <- pend_w(d$anio_dec[m], d$dh_m[m], (w0 * k)[m])
    }
    o
  })
  apply(r, 1, sd, na.rm = TRUE)
}
RB <- R0
for (i in which(UNIDADES$tipo == "piloto" & nzchar(UNIDADES$glaciar))) {
  u <- UNIDADES[i, ]; k <- which(RB$conjunto == u$conjunto & RB$gid == u$glaciar)
  if (length(k) && RB$propio[k]) { b <- boot_glaciar(u$conjunto, u$glaciar); RB$error[k] <- b[1]; RB$error_t1[k] <- b[2]; RB$error_t2[k] <- b[3] }
}
boot_de <- function(u) {
  if (u$tipo == "piloto") return(media)
  g <- AG[AG$conjunto == u$conjunto & AG$grupo == (if (u$tipo == "cuenca") u$unidad else paste("Maipo |", u$unidad)), ]
  function(d, col) if (grepl("_t1", col)) g$error_t1 else if (grepl("_t2", col)) g$error_t2 else g$error_tasa
}

FORMAS <- list(MM_GDEM_E_INDEP = columna(R0, function(u) indep), MM_GDEM_E_BOOT = columna(RB, boot_de),
               MM_GDEM_E_COVAR = columna(R0, function(u) covar), MM_GDEM_E_CELDAS = columna(R0, function(u) celdas),
               MM_GDEM_E_ROLSTAD500 = columna(con_rolstad(0.5)), MM_GDEM_E_ROLSTAD1000 = columna(con_rolstad(1)),
               MM_GDEM_E_TRIANG = columna(R0, function(u) triang))
for (m in names(FORMAS)) X[[m]] <- FORMAS[[m]]

COLS <- c(names(DIRS), names(FORMAS))
dec <- ifelse(X$variable %in% c("escenas_mediana", "area_con_ajuste_pct"), 1, 3)
for (m in COLS) X[[m]] <- round(X[[m]], dec)
names(X)[names(X) == "conjunto"] <- "inventario"; X$area_km2 <- round(X$area_km2, 3)
write.csv(X[, c("inventario", "tipo", "unidad", "glaciar", "glaciares", "area_km2", "variable", COLS, "nota")],
          file.path(AQUI, "Resumen_Cuenca_Subcuencas_Piloto.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
cat(sprintf("\nFilas: %d, columnas de metodologia: %d\n", nrow(X), length(COLS)))
v <- X[X$inventario == "dga" & X$unidad == "Cuenca alta del Maipo" & grepl("^error", X$variable), c("variable", FINAL, names(FORMAS))]
print(v, row.names = FALSE)

# --- la tabla de las formas de medir el error, para el anexo (en ingles) --------------------
DESC <- c(MM_GDEM_13M = "Area weighted mean of the error of each glacier, which is the standard error of the slope of its series. The form of Far\\'ias-Barahona et al. (2020) and Braun et al. (2019), and the one used in this work.",
          MM_GDEM_E_INDEP = "Glaciers treated as independent, with the root of the sum of squares. A lower limit.",
          MM_GDEM_E_BOOT = "Bootstrap of scenes with 500 draws. All glaciers are fitted again with the same resampled scenes.",
          MM_GDEM_E_COVAR = "Double sum of covariances between glaciers (Hugonnet et al. 2021, Dussaillant et al. 2025), with a correlation that falls with distance, taken from the residuals of the series in the scenes that two glaciers share.",
          MM_GDEM_E_CELDAS = "Errors added inside cells of 55 km and in quadrature between cells (Shean et al. 2020). Shean uses hexagons, and here the cells are squares, with the mean of nine grid origins.",
          MM_GDEM_E_ROLSTAD500 = "The error of each glacier comes from the NMAD of the rock of each scene, reduced with Rolstad et al. (2009) for a correlation length of 500 m. Then the area weighted mean.",
          MM_GDEM_E_ROLSTAD1000 = "The same with a correlation length of 1 km.",
          MM_GDEM_E_TRIANG = "Each unit as a single ice body plus the triangulation error (Dussaillant et al. 2019), the 68th percentile of the residual between the full period and its two halves. It has no value for the two periods.")
cu <- X[X$inventario == "dga" & X$tipo == "cuenca", ]
val <- function(m, v) sprintf("%.3f", cu[[m]][cu$variable == v])
lin <- sprintf("\\texttt{%s} & %s & %s & %s \\\\", gsub("_", "\\\\_", names(DESC)), DESC,
               sapply(names(DESC), val, "error_tasa_m_a"), sapply(names(DESC), val, "error_balance_850_m_we_a"))
cab <- "Column & How the error is measured & Rate & Balance \\\\"
writeLines(c("{\\small", "\\begin{longtable}{L{4.2cm}L{7.7cm}rr}",
             "\\caption{The ways of measuring the error on the final methodology, with the error of the upper Maipo basin with the DGA inventory, at one standard deviation. Rate in m a$^{-1}$ and balance in m w.e. a$^{-1}$.}\\\\",
             "\\toprule", cab, "\\midrule", "\\endfirsthead", "\\toprule", cab, "\\midrule", "\\endhead",
             lin, "\\bottomrule", "\\end{longtable}", "}"),
           file.path(AQUI, "anexo", "tab", "errores.tex"), useBytes = TRUE)

# --- comprobacion contra M06 ---------------------------------------------------------------
comparar <- function(m, dir) {
  G <- read.csv(file.path(dir, "M06_Agregados.csv"))
  G <- G[G$conjunto %in% CONJ & (G$grupo == "Cuenca alta del Maipo" | G$grupo %in% paste("Maipo |", GRUPOS)), ]
  dif <- sapply(seq_len(nrow(G)), function(i) {
    # sin los piloto: Yeso es a la vez subcuenca y glaciar piloto
    x <- X[X$inventario == G$conjunto[i] & X$tipo != "piloto" & X$unidad == sub("^Maipo \\| ", "", G$grupo[i]), ]
    g <- function(v, c = m) x[[c]][x$variable == v]
    o <- c(g("tasa_m_a") - G$tasa_m_a[i], g("error_tasa_m_a") - G$error_conservador[i],
           g("tasa_2001_2013_m_a") - G$tasa_2001_2013[i], g("tasa_2013_2026_m_a") - G$tasa_2013_2026[i],
           g("error_2001_2013_m_a") - G$error_t1_conservador[i], g("error_2013_2026_m_a") - G$error_t2_conservador[i])
    if (m == FINAL) o <- c(o, g("error_tasa_m_a", "MM_GDEM_E_INDEP") - G$error_cuadratura[i])
    max(abs(o))
  })
  cat(sprintf("%-14s contra M06_Agregados: %d filas, diferencia maxima %.4f\n", m, nrow(G), max(dif)))
}
comparar("MM_GDEM_13M", file.path(MM, "gdem", "tablas"))
comparar("MM_TDX90_13M", file.path(MM, "tdx90", "tablas"))
comparar("AST_13M", TRES)
cat("Listo.\n")
