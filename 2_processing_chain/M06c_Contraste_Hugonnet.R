# M06c. Contraste con Hugonnet et al. (2021), glaciar por glaciar sobre el RGI 6.0, con la serie
# propia cortada a su ventana (escenas anteriores a 2020) y por decadas (2000-2010 y 2010-2020).
# Entra: M06_resultados.rds y el archivo por glaciar de Hugonnet (region 17, publicado por OGGM)
# Sale: tablas/M06c_*.csv

base    <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")
dir_tab <- file.path(dir_c, "tablas")
dir_tex <- file.path(base, "02_Balance_Masa_RM", "Reporte_1_RM", "tab")
f_hug   <- file.path(dir_c, "datos_externos", "hugonnet_2021_ds_rgi60_pergla_rates_10_20_region17.csv")

RHO <- 850; S_RHO <- 60
MIN_N <- 5; MIN_SPAN <- 10            # minimos de la serie completa (M06)
MIN_N_DEC <- 4; MIN_SPAN_DEC <- 5     # minimos de un tramo (M06)
T_FIN <- 2020.0                       # 1 de enero de 2020, el final de Hugonnet
T_DEC <- 2010.0                       # corte entre sus dos decenios
N_BOOT <- 500
MAIPO <- "Cuenca alta del Maipo"

nmad <- function(x) { x <- x[is.finite(x)]; 1.4826 * median(abs(x - median(x))) }
wm <- function(x, w) sum(w * x) / sum(w)

Z <- readRDS(file.path(dir_c, "M06_resultados.rds"))
A6 <- Z$A6; ORDEN <- Z$ORDEN_GRUPOS
REF <- Z$REF[Z$REF$conjunto == "rgi60", ]
t0_de <- setNames(REF$t_ref, REF$gid)
# SE ya trae solo las escenas usadas (sin la referencia ni las apartadas por
# temporada), con la NMAD de la roca de cada par
SE <- Z$SE[Z$SE$conjunto == "rgi60" & Z$SE$anio_dec < T_FIN, ]
cat("Escenas propias antes de 2020 (RGI 6.0):", length(unique(SE$escena)), "| filas:", nrow(SE),
    "| ultima:", max(SE$escena), "\n")

# --- 1 y 2. ajuste propio por poligono en la ventana de Hugonnet -------------------------
F <- do.call(rbind, lapply(split(SE, SE$gid), function(d) {
  t0 <- t0_de[[d$gid[1]]]
  o <- data.frame(RGIId = d$gid[1], t_ref = t0, n20 = NA, tasa20 = NA, error20 = NA,
                  n_d1 = NA, tasa_d1 = NA, error_d1 = NA, n_d2 = NA, tasa_d2 = NA, error_d2 = NA)
  if (nrow(d) >= MIN_N && (max(d$anio_dec) - t0) >= MIN_SPAN) {
    d$x <- d$anio_dec - t0
    s <- summary(lm(dh_m ~ x, d, weights = 1 / d$NMAD_roca^2))$coefficients
    if (nrow(s) >= 2) { o$n20 <- nrow(d); o$tasa20 <- s[2, 1]; o$error20 <- s[2, 2] }
  }
  for (k in 1:2) {
    dd <- if (k == 1) d[d$anio_dec < T_DEC, ] else d[d$anio_dec >= T_DEC, ]
    if (nrow(dd) >= MIN_N_DEC && diff(range(dd$anio_dec)) >= MIN_SPAN_DEC) {
      s <- summary(lm(dh_m ~ anio_dec, dd, weights = 1 / dd$NMAD_roca^2))$coefficients
      if (nrow(s) >= 2) {
        o[[paste0("n_d", k)]] <- nrow(dd); o[[paste0("tasa_d", k)]] <- s[2, 1]; o[[paste0("error_d", k)]] <- s[2, 2]
      }
    }
  }
  o
}))

# --- Hugonnet por poligono ----------------------------------------------------------------
H <- read.csv(f_hug, stringsAsFactors = FALSE)
G <- data.frame(RGIId = A6$RGIId, nombre = A6$Name, grupo_cuenca = A6$grupo_cuenca, en_maipo = A6$en_maipo,
                area_km2 = A6$area_utm_km2, stringsAsFactors = FALSE)
G <- merge(G, F, by = "RGIId", all.x = TRUE)
hp <- function(per, var) { h <- H[H$period == per, ]; h[[var]][match(G$RGIId, h$rgiid)] }
P20 <- "2000-01-01_2020-01-01"; P1 <- "2000-01-01_2010-01-01"; P2 <- "2010-01-01_2020-01-01"
G$Hug <- hp(P20, "dmdtda"); G$Hug_err <- hp(P20, "err_dmdtda")
G$Hug_dhdt <- hp(P20, "dhdt"); G$Hug_err_dhdt <- hp(P20, "err_dhdt")
G$Hug_area_medida_pct <- 100 * hp(P20, "perc_area_meas")
G$Hug_d1 <- hp(P1, "dmdtda"); G$Hug_d1_err <- hp(P1, "err_dmdtda")
G$Hug_d2 <- hp(P2, "dmdtda"); G$Hug_d2_err <- hp(P2, "err_dmdtda")
# balance propio (m w.e./a con 850) y diferencia "este trabajo - Hugonnet"
for (s in c("20", "_d1", "_d2")) {
  hs <- if (s == "20") "" else s
  G[[paste0("MB", s)]] <- G[[paste0("tasa", s)]] * RHO / 1000
  G[[paste0("MB", s, "_err")]] <- G[[paste0("error", s)]] * RHO / 1000
  G[[paste0("dif", s)]] <- G[[paste0("MB", s)]] - G[[paste0("Hug", hs)]]
  G[[paste0("z", s)]] <- G[[paste0("dif", s)]] /
    sqrt(G[[paste0("MB", s, "_err")]]^2 + G[[paste0("Hug", hs, "_err")]]^2)
}
# Dussaillant et al. (2019) sobre los mismos poligonos, solo como contexto
G$Dus <- Z$DU$MB[match(G$RGIId, Z$DU$RGIId)]; G$Dus_err <- Z$DU$MB_err[match(G$RGIId, Z$DU$RGIId)]
G$comparable <- is.finite(G$Hug) & is.finite(G$MB20)
G$comparable_dec <- is.finite(G$Hug_d1) & is.finite(G$Hug_d2) & is.finite(G$MB_d1) & is.finite(G$MB_d2)
G <- G[order(!G$en_maipo, G$grupo_cuenca, G$RGIId), ]
Gw <- G; num <- sapply(Gw, is.numeric); Gw[num] <- lapply(Gw[num], round, 4)
write.csv(Gw, file.path(dir_tab, "M06c_Hugonnet_por_glaciar.csv"), row.names = FALSE)

# --- bootstrap de escenas -------------------------------------------------------------------
# Como en M06 (seccion 2b): se sortean ESCENAS con reposicion y se reajustan todos
# los poligonos con las mismas escenas, para que la correlacion del error entre
# glaciares vecinos entre al error del agregado. Las sumas por poligono se hacen
# con codigos enteros y largo fijo, para que un poligono sin escenas en un tramo
# no corra a los demas.
set.seed(3)
SEb <- SE[order(SE$gid), ]
gi <- as.integer(factor(SEb$gid)); ids <- levels(factor(SEb$gid)); ng <- length(ids)
esc <- sort(unique(SEb$escena)); ie <- match(SEb$escena, esc)
w0 <- 1 / SEb$NMAD_roca^2; x <- SEb$anio_dec - unname(t0_de[SEb$gid]); y <- SEb$dh_m; ta <- SEb$anio_dec
suma <- function(v, g) { s <- numeric(ng); r <- rowsum(v, g); s[as.integer(rownames(r))] <- r; s }
pend <- function(W, xx, yy, g) {
  S0 <- suma(W, g); Sx <- suma(W * xx, g); Sy <- suma(W * yy, g)
  Sxx <- suma(W * xx * xx, g); Sxy <- suma(W * xx * yy, g)
  (S0 * Sxy - Sx * Sy) / (S0 * Sxx - Sx^2)
}
rango <- function(v, u, g, f, inicio) { s <- rep(inicio, ng); vv <- tapply(v[u], g[u], f); s[as.integer(names(vv))] <- vv; s }
BT <- list(tasa = matrix(NA_real_, N_BOOT, ng, dimnames = list(NULL, ids)))
BT$d1 <- BT$tasa; BT$d2 <- BT$tasa
m1 <- ta < T_DEC
t_b <- Sys.time()
for (b in seq_len(N_BOOT)) {
  cnt <- tabulate(sample.int(length(esc), length(esc), replace = TRUE), length(esc))[ie]
  W <- w0 * cnt; usa <- cnt > 0
  nd <- suma(as.numeric(usa), gi); xmax <- rango(x, usa, gi, max, -Inf)
  r <- pend(W, x, y, gi); r[nd < MIN_N | xmax < MIN_SPAN] <- NA
  BT$tasa[b, ] <- r
  for (k in 1:2) {
    m <- if (k == 1) m1 else !m1
    u <- usa & m
    nd <- suma(as.numeric(u), gi)
    rg <- rango(ta, u, gi, max, -Inf) - rango(ta, u, gi, min, Inf)
    r <- pend(W * m, ta, y, gi); r[nd < MIN_N_DEC | !is.finite(rg) | rg < MIN_SPAN_DEC] <- NA
    BT[[paste0("d", k)]][b, ] <- r
  }
}
cat(sprintf("Bootstrap de escenas: %d sorteos de %d escenas (%.0f s)\n", N_BOOT, length(esc),
            as.numeric(Sys.time() - t_b, units = "secs")))
sd_b <- function(M, gids, w) {
  X <- M[, gids, drop = FALSE]
  v <- apply(X, 1, function(z) { ok <- is.finite(z); if (!any(ok)) NA else sum(w[ok] * z[ok]) / sum(w[ok]) })
  sd(v, na.rm = TRUE)
}
# error del balance propio de un agregado: remuestreo de escenas mas densidad
err_bal <- function(M, d, var) {
  w <- d$area_km2; t <- wm(d[[var]], w)
  sqrt((sd_b(M, d$RGIId, w) * RHO)^2 + (t * S_RHO)^2) / 1000
}

# --- 3. agregados -----------------------------------------------------------------------------
resumen <- function(A, grupo) {
  c0 <- A[A$comparable, ]; w <- c0$area_km2
  cd <- A[A$comparable_dec, ]; wd <- cd$area_km2
  o <- data.frame(grupo = grupo, poligonos = nrow(A), area_total_km2 = sum(A$area_km2),
                  glaciares = nrow(c0), area_km2 = sum(w), pct_area = 100 * sum(w) / sum(A$area_km2))
  if (!nrow(c0)) return(o)
  o$Hug_area_medida_pct <- wm(c0$Hug_area_medida_pct, w)
  o$MB_Hugonnet <- wm(c0$Hug, w); o$err_Hugonnet <- wm(c0$Hug_err, w)
  o$MB_este <- wm(c0$MB20, w); o$err_este <- err_bal(BT$tasa, c0, "tasa20")
  o$err_este_conservador <- wm(c0$MB20_err, w)
  o$dif <- o$MB_este - o$MB_Hugonnet
  o$n_sigma <- o$dif / sqrt(o$err_este^2 + o$err_Hugonnet^2)
  o$dif_mediana <- median(c0$dif20); o$dif_nmad <- nmad(c0$dif20)
  o$pct_dentro_1 <- 100 * mean(abs(c0$z20) <= 1); o$pct_dentro_2 <- 100 * mean(abs(c0$z20) <= 2)
  o$pct_area_dentro_1 <- 100 * sum(w[abs(c0$z20) <= 1]) / sum(w)
  o$pct_area_dentro_2 <- 100 * sum(w[abs(c0$z20) <= 2]) / sum(w)
  o$correlacion <- if (nrow(c0) >= 3) cor(c0$Hug, c0$MB20) else NA
  # Dussaillant contra Hugonnet en los mismos glaciares (contexto)
  cdu <- c0[is.finite(c0$Dus), ]
  o$glaciares_con_Dussaillant <- nrow(cdu)
  o$correlacion_Dus_Hug <- if (nrow(cdu) >= 3) cor(cdu$Dus, cdu$Hug) else NA
  o$pct_dentro_1_Dus_Hug <- if (nrow(cdu)) 100 * mean(abs(cdu$Dus - cdu$Hug) <= sqrt(cdu$Dus_err^2 + cdu$Hug_err^2)) else NA
  # decenios
  o$glaciares_dec <- nrow(cd); o$area_dec_km2 <- sum(wd)
  if (nrow(cd)) {
    for (k in 1:2) {
      s <- paste0("_d", k)
      o[[paste0("MB_Hugonnet", s)]] <- wm(cd[[paste0("Hug", s)]], wd)
      o[[paste0("err_Hugonnet", s)]] <- wm(cd[[paste0("Hug", s, "_err")]], wd)
      o[[paste0("MB_este", s)]] <- wm(cd[[paste0("MB", s)]], wd)
      o[[paste0("err_este", s)]] <- err_bal(BT[[paste0("d", k)]], cd, paste0("tasa", s))
      o[[paste0("pct_dentro_1", s)]] <- 100 * mean(abs(cd[[paste0("z", s)]]) <= 1)
    }
  }
  o
}
grupos <- list(); grupos[[MAIPO]] <- G[G$en_maipo, ]
for (g in ORDEN) grupos[[g]] <- G[G$grupo_cuenca %in% g, ]
grupos[["Otras de la cuenca"]] <- G[G$grupo_cuenca %in% "Maipo, otras", ]
grupos[["Fuera de la cuenca del Maipo"]] <- G[!G$en_maipo, ]
grupos[["Región Metropolitana"]] <- G
# (un grupo sin glaciares comparables trae menos columnas: se igualan antes de unir)
RL <- lapply(names(grupos), function(g) resumen(grupos[[g]], g))
cols <- unique(unlist(lapply(RL, names)))
RS <- do.call(rbind, lapply(RL, function(o) { for (c in setdiff(cols, names(o))) o[[c]] <- NA; o[, cols] }))
# cinco decimales: M08 y la presentacion leen este CSV y redondean a dos; con tres
# se redondeaba dos veces (un -0,405 salia -0,40 en la lamina y -0,41 en el documento)
RSw <- RS; num <- sapply(RSw, is.numeric); RSw[num] <- lapply(RSw[num], round, 5)
write.csv(RSw, file.path(dir_tab, "M06c_Hugonnet_resumen.csv"), row.names = FALSE)
print(RSw[, c("grupo", "glaciares", "area_km2", "MB_Hugonnet", "err_Hugonnet", "MB_este", "err_este", "dif",
              "n_sigma", "pct_dentro_1", "pct_area_dentro_1", "correlacion", "correlacion_Dus_Hug")], row.names = FALSE)
print(RSw[, c("grupo", "glaciares_dec", "MB_Hugonnet_d1", "MB_este_d1", "err_este_d1", "MB_Hugonnet_d2",
              "MB_este_d2", "err_este_d2")], row.names = FALSE)

# --- LaTeX: cifras y tabla del documento ------------------------------------------------------
f <- function(x, d = 2) {
  s <- formatC(round(x, d), format = "f", digits = d, big.mark = ".", decimal.mark = ",")
  s <- sub("^-", "$-$", s); s[is.na(x)] <- "--"; s
}
fn <- function(x, d = 2) {   # cifras del texto: sirven dentro y fuera de modo matematico
  s <- formatC(round(x, d), format = "f", digits = d, big.mark = "{.}", decimal.mark = "{,}")
  s <- sub("^-", "\\\\ensuremath{-}", s); s[is.na(x)] <- "--"; s
}
fs <- function(x, d = 2) ifelse(round(x, d) > 0, paste0("+", fn(x, d)), fn(x, d))
ent <- function(x) formatC(round(x), format = "d", big.mark = ".", decimal.mark = ",")
pm <- function(x, e, d = 2) paste0(f(x, d), " $\\pm$ ", f(e, d))
lista <- function(s) if (length(s) > 1) paste(paste(head(s, -1), collapse = ", "), "y", tail(s, 1)) else s
num_es <- c("cero", "uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
palabra <- function(n) if (is.na(n)) "--" else if (n <= 10) num_es[n + 1] else ent(n)
cif <- list(); def <- function(n, v) cif[[n]] <<- v

m <- RS[RS$grupo == MAIPO, ]
def("hugNtotal", ent(m$poligonos)); def("hugN", ent(m$glaciares)); def("hugPctArea", f(m$pct_area, 0))
def("hugAreaMedida", f(m$Hug_area_medida_pct, 0))
def("hugMB", fn(m$MB_Hugonnet)); def("hugErr", fn(m$err_Hugonnet))
def("esteMBhug", fn(m$MB_este)); def("esteErrhug", fn(m$err_este))
def("hugDif", fs(m$dif)); def("hugDifMediana", fn(m$dif_mediana))
def("hugPctUno", f(m$pct_dentro_1, 0)); def("hugPctDos", f(m$pct_dentro_2, 0))
def("hugAreaUno", f(m$pct_area_dentro_1, 0)); def("hugAreaDos", f(m$pct_area_dentro_2, 0))
def("hugCor", f(m$correlacion, 2)); def("hugCorDus", f(m$correlacion_Dus_Hug, 2))
def("hugPctUnoDus", f(m$pct_dentro_1_Dus_Hug, 0)); def("hugNDus", ent(m$glaciares_con_Dussaillant))
def("hugDecN", ent(m$glaciares_dec)); def("hugDecPctArea", f(100 * m$area_dec_km2 / m$area_total_km2, 0))
def("hugDecUno", fn(m$MB_Hugonnet_d1)); def("ehugDecUno", fn(m$err_Hugonnet_d1))
def("hugDecDos", fn(m$MB_Hugonnet_d2)); def("ehugDecDos", fn(m$err_Hugonnet_d2))
def("esteDecUno", fn(m$MB_este_d1)); def("eesteDecUno", fn(m$err_este_d1))
def("esteDecDos", fn(m$MB_este_d2)); def("eesteDecDos", fn(m$err_este_d2))
# frases que dependen del resultado (para no escribir a mano una conclusion que
# los datos podrian contradecir)
def("fraseHugCuenca", if (abs(m$n_sigma) <= 1)
  sprintf("la diferencia, de %s, es menor que el error de los dos juntos", fs(m$dif)) else
  sprintf("la diferencia, de %s, es %s veces el error de los dos juntos", fs(m$dif), fn(abs(m$n_sigma), 1)))
sg <- RS[RS$grupo %in% c(ORDEN, "Otras de la cuenca") & RS$glaciares > 0, ]
fu <- sg[abs(sg$n_sigma) > 1, ]
nombre_g <- function(g) ifelse(g == "Cabecera del Maipo", "la cabecera del Maipo",
                               ifelse(g == "Otras de la cuenca", "las otras de la cuenca", g))
def("fraseHugGrupos", if (!nrow(fu)) "En todas las subcuencas la diferencia es menor que el error de los dos juntos." else
  sprintf("Por subcuenca, la diferencia es menor que el error de los dos juntos salvo en %s, %s aquí.",
          lista(nombre_g(fu$grupo)),
          if (all(fu$dif < 0)) "con más pérdida" else if (all(fu$dif > 0)) "con menos pérdida" else "con valores distintos"))
d1h <- m$MB_Hugonnet_d1; d2h <- m$MB_Hugonnet_d2; d1e <- m$MB_este_d1; d2e <- m$MB_este_d2
def("fraseHugDecenios", if (d2h < d1h && d2e < d1e)
  "En los dos trabajos el segundo decenio es más negativo que el primero" else
  if (d2h < d1h) "Hugonnet \\emph{et al.} (2021) dan un segundo decenio más negativo que el primero, y la serie propia no" else
  "La serie propia da un segundo decenio más negativo que el primero, y Hugonnet \\emph{et al.} (2021) no")
writeLines(c("% Generado por Cadena_RM/M06c_Contraste_Hugonnet.R. No editar a mano.",
             sprintf("\\newcommand{\\%s}{%s}", names(cif), unlist(cif))),
           file.path(dir_tex, "cifras_hugonnet.tex"), useBytes = TRUE)

# Tabla: subcuencas en el periodo completo y la cuenca por decenio. Sin columna
# sigma: la coincidencia
# se dice como el porcentaje de glaciares cuya diferencia es menor que el error
# de los dos juntos.
fila <- function(et, r, s = "", con_error = FALSE) {
  if (s == "") { g <- r$glaciares; a <- r$area_km2; h <- r$MB_Hugonnet; eh <- r$err_Hugonnet
    e <- r$MB_este; ee <- r$err_este; p <- r$pct_dentro_1
  } else { g <- r$glaciares_dec; a <- r$area_dec_km2; h <- r[[paste0("MB_Hugonnet", s)]]
    eh <- r[[paste0("err_Hugonnet", s)]]; e <- r[[paste0("MB_este", s)]]; ee <- r[[paste0("err_este", s)]]
    p <- r[[paste0("pct_dentro_1", s)]] }
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\", et, ent(g), f(a, 1),
          if (con_error) pm(h, eh) else f(h), if (con_error) pm(e, ee) else f(e), f(e - h), f(p, 0))
}
filas <- c("\\multicolumn{7}{l}{\\textit{Hugonnet: 2000--2019; este trabajo: 2001--2019}} \\\\")
for (k in seq_len(nrow(sg))) filas <- c(filas, fila(sg$grupo[k], sg[k, ]))
filas <- c(filas, fila(paste0("\\textbf{", MAIPO, "}"), m, con_error = TRUE), "\\midrule",
           "\\multicolumn{7}{l}{\\textit{Por decenio. Hugonnet: 2000--2009 y 2010--2019; este trabajo: 2001--2009 y 2010--2019}} \\\\",
           fila("Primer decenio", m, "_d1", TRUE),
           fila("Segundo decenio", m, "_d2", TRUE))
writeLines(c("\\begin{tabular}{P{3.8cm} r r r r r r}", "\\toprule",
             "& & & \\multicolumn{2}{c}{\\textbf{Balance (m w.e./año)}} & & \\textbf{Dentro del} \\\\",
             "\\textbf{Subcuenca} & \\textbf{Glaciares} & \\textbf{km$^2$} & \\textbf{Hugonnet} & \\textbf{Este trabajo} & \\textbf{Diferencia} & \\textbf{error (\\porc)} \\\\",
             "\\midrule", filas, "\\bottomrule", "\\end{tabular}",
             paste("\\par\\smallskip\\parbox{0.97\\linewidth}{\\footnotesize Balances con 850~kg/m$^3$, la densidad de",
                   "Hugonnet \\emph{et al.} (2021). Sus períodos van del 1 de enero de 2000 al 1 de enero de 2020, y",
                   "por decenio, al 1 de enero de 2010 y desde esa fecha; la serie propia se corta en la última escena",
                   "anterior a 2020, de abril de 2019, y por decenio se ajusta una recta a las escenas de 2001 a 2009 y",
                   "otra a las de 2010 a 2019. Se usan sólo los polígonos del RGI~6.0 con valor en los dos trabajos,",
                   "y cada fila los promedia ponderando por su área.",
                   if (any(sg$grupo == "Otras de la cuenca"))
                     sprintf("Otras de la cuenca: %s polígonos de la cuenca alta del Maipo que no caen en ninguna de las seis subcuencas.",
                             palabra(sg$glaciares[sg$grupo == "Otras de la cuenca"])) else "",
                   "Diferencia: este trabajo menos Hugonnet. Dentro",
                   "del error: porcentaje de los glaciares cuya diferencia es menor que el error de los dos juntos.",
                   "En las filas de la cuenca, el error de Hugonnet es el promedio de sus errores por glaciar, y el",
                   "propio sale de remuestrear las escenas y de la incertidumbre de la densidad.}")),
           file.path(dir_tex, "T_hugonnet.tex"), useBytes = TRUE)
cat("cifras_hugonnet:", length(cif), "| T_hugonnet.tex escrita\n")
cat("Listo.\n")
