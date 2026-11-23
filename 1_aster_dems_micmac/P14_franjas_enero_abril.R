# P14. Franjas que se construyen con MicMac en la cuenca alta del Maipo
#   Todas las tomas L1A de enero a abril de 2000 a 2026 que tocan algun glaciar de la cuenca. Los
#   granulos de una misma pasada con menos de 12 s entre si se agrupan en franjas de hasta tres,
#   sin saltarse un granulo, como Hugonnet et al. (2021). El orden de proceso es de van der Corput
#   sobre la fecha, para que cualquier tramo inicial cubra todo el periodo.
# Uso: Rscript P14_franjas_enero_abril.R
# Sale: resultados/P14_franjas_nuevas.csv (la lista para P05f) y P14_franjas_usadas.csv
suppressPackageStartupMessages(library(terra))
B <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
R <- file.path(B, "03_DEM_MicMac/resultados")
M <- "C:/Users/sebas/Prueba_MMASTER"
MESES <- 1:4

g <- vect(file.path(M, "referencia/rgi60_cuenca.shp"))
a <- expanse(g, unit = "km")

C <- read.csv(file.path(R, "P01_catalogo_cuenca_2026.csv"), stringsAsFactors = FALSE)
C$t <- as.POSIXct(C$fecha, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
C <- C[order(C$t), ]
C$mes <- as.integer(format(C$t, "%m"))
C$nube <- ifelse(is.na(suppressWarnings(as.numeric(C$nube))), 100, as.numeric(C$nube))
cat(sprintf("%d granulos L1A de dia en 2000-2026; %d de enero a abril\n", nrow(C), sum(C$mes %in% MESES)))
C <- C[C$mes %in% MESES, ]
wkt <- vapply(C$poligono, function(p) {
  v <- as.numeric(strsplit(trimws(p), " +")[[1]])
  m <- matrix(v, ncol = 2, byrow = TRUE)
  if (any(m[1, ] != m[nrow(m), ])) m <- rbind(m, m[1, ])
  paste0("POLYGON((", paste(m[, 2], m[, 1], collapse = ", "), "))")
}, "")
H <- project(vect(unname(wkt), crs = "EPSG:4326"), crs(g))

# fraccion de cada glaciar dentro de una huella (filas: huellas, columnas: glaciares)
cobertura <- function(P) {
  P$i <- seq_len(nrow(P)); gg <- g; gg$j <- seq_len(nrow(gg))
  x <- intersect(P, gg)
  Mx <- matrix(0, nrow(P), nrow(gg))
  if (nrow(x)) {
    s <- tapply(expanse(x, unit = "km"), list(x$i, x$j), sum)
    s[is.na(s)] <- 0
    Mx[as.integer(rownames(s)), as.integer(colnames(s))] <- s
  }
  pmin(sweep(Mx, 2, a, "/"), 1)
}
C$ve_km2 <- as.vector(cobertura(H) %*% a)
H <- H[C$ve_km2 > 0]; C <- C[C$ve_km2 > 0, ]
cat(sprintf("%d tocan algun glaciar de la cuenca\n", nrow(C)))

# franjas ya construidas (DEM final de MicMac en trabajo/), con sus granulos segun P05b
Fv <- read.csv(file.path(R, "P05b_franjas_volcan.csv"), stringsAsFactors = FALSE)
Fv$nombre <- substr(sub(" .*", "", Fv$granulos), 1, 25)
hechas <- basename(Sys.glob(file.path(M, "trabajo", "AST_L1A_*", "PROCESSED_FINAL", "AST_L1A_*", "AST_L1A_*_Z.tif")))
hechas <- sub("_Z.tif$", "", hechas)
stopifnot(all(hechas %in% Fv$nombre))
Fh <- Fv[match(hechas, Fv$nombre), ]
Fh$mes <- as.integer(substr(Fh$fecha, 6, 7))
cat(sprintf("%d franjas ya construidas: %d de enero a abril (%s) y %d de otros meses (%s)\n", nrow(Fh),
            sum(Fh$mes %in% MESES), paste(Fh$fecha[Fh$mes %in% MESES], collapse = ", "),
            sum(!Fh$mes %in% MESES), paste(Fh$fecha[!Fh$mes %in% MESES], collapse = ", ")))
writeLines(Fh$nombre[!Fh$mes %in% MESES], file.path(M, "fuera_de_temporada.txt"))
Fh <- Fh[Fh$mes %in% MESES, ]
franjas <- lapply(strsplit(Fh$granulos, " "), function(x) match(x, C$granulo))
franjas <- lapply(franjas, function(f) f[!is.na(f)])       # un granulo de la franja puede no tocar la cuenca
hecha <- rep(TRUE, length(franjas))
usados <- C$granulo %in% unlist(strsplit(Fh$granulos, " "))

C$pasada <- cumsum(c(TRUE, diff(as.numeric(C$t)) >= 12))
for (p in unique(C$pasada)) {
  k <- which(C$pasada == p)
  libre <- !usados[k]
  r <- rle(libre); fin <- cumsum(r$lengths); ini <- fin - r$lengths + 1
  for (q in which(r$values)) {
    tramo <- k[ini[q]:fin[q]]
    for (b in split(tramo, ceiling(seq_along(tramo) / 3))) {
      franjas[[length(franjas) + 1]] <- b; hecha <- c(hecha, FALSE)
    }
  }
}
S <- data.frame(hecha = hecha, n = lengths(franjas),
                fecha = as.Date(vapply(franjas, function(f) format(C$t[f[1]], "%Y-%m-%d"), "")),
                nube = vapply(franjas, function(f) max(C$nube[f]), 0),
                granulos = vapply(franjas, function(f) paste(C$granulo[f], collapse = " "), ""))
S$granulos[S$hecha] <- Fh$granulos                          # las construidas, con todos sus granulos
S$anio <- as.integer(format(S$fecha, "%Y"))
S$franja <- substr(sub(" .*", "", S$granulos), 1, 25)
cat(sprintf("%d franjas en total: %d ya construidas y %d nuevas, con %d granulos nuevos\n", nrow(S), sum(S$hecha),
            sum(!S$hecha), sum(S$n[!S$hecha])))
cat("franjas por anio:", paste(sprintf("%d:%d", sort(unique(S$anio)), as.vector(table(S$anio))), collapse = " "), "\n")

# cobertura efectiva por glaciar (fraccion del glaciar dentro de la huella x (1 - nube))
U <- do.call(rbind, lapply(franjas, function(f) aggregate(H[f])))
E <- cobertura(U) * (1 - S$nube / 100)
cob <- colSums(E)
n50 <- colSums(cobertura(U) >= 0.5)
cat(sprintf("por glaciar en 2000-2026: franjas que ven >= 50 %% del glaciar, mediana %d (minimo %d); cobertura efectiva mediana %.0f (minimo %.0f)\n",
            as.integer(median(n50)), min(n50), median(cob), min(cob)))

van_der_corput <- function(i) { v <- 0; d <- 0.5; while (i > 0) { v <- v + d * (i %% 2); i <- i %/% 2; d <- d / 2 }; v }
N <- S[!S$hecha, ]
N <- N[order(N$fecha), ]
N <- N[order(vapply(seq_len(nrow(N)) - 1, van_der_corput, 0)), ]
salida <- function(X) data.frame(franja = X$franja, fecha = X$fecha, n_granulos = X$n, granulos = X$granulos,
                                 nube_max = X$nube, verano = TRUE)
write.csv(salida(N), file.path(R, "P14_franjas_nuevas.csv"), row.names = FALSE, quote = FALSE)
write.csv(salida(S[order(S$fecha), ]), file.path(R, "P14_franjas_usadas.csv"), row.names = FALSE, quote = FALSE)
write.csv(data.frame(RGIId = g$RGIId, km2 = round(a, 3), franjas_50pct = n50, cobertura_efectiva = round(cob, 1)),
          file.path(R, "P14_cobertura_glaciares.csv"), row.names = FALSE)

# AST14DEM de las mismas tomas (misma marca de tiempo MMDDAAAAhhmmss que el L1A)
gr <- unlist(strsplit(S$granulos, " "))
D14 <- read.csv(file.path(R, "P01_catalogo_cuenca_2026_AST14DEM.csv"), stringsAsFactors = FALSE)
D14 <- D14[substr(D14$granulo, 10, 26) %in% substr(gr, 9, 25), ]
writeLines(D14$granulo, file.path(R, "P14_ast14dem_mismas.txt"))
cat(sprintf("%d granulos L1A en las franjas; %d con AST14DEM de la misma toma\n", length(gr), nrow(D14)))

# descargas que faltan
existe <- function(dirs, f) Reduce(`|`, lapply(dirs, function(d) file.exists(file.path(M, d, f))))
Cn <- C[C$granulo %in% gr & !existe(c("L1A/volcan", "L1A/cuenca"), paste0(C$granulo, ".hdf")), ]
write.csv(Cn[, c("granulo", "fecha", "nube", "mb", "url")], file.path(M, "L1A/descarga_enero_abril.csv"),
          row.names = FALSE, quote = FALSE)
Dn <- D14[!existe(c("AST14DEM/volcan", "AST14DEM/cuenca"), paste0(D14$granulo, ".tif")), ]
write.csv(Dn[, c("granulo", "fecha", "nube", "mb", "url")], file.path(M, "AST14DEM/descarga_enero_abril.csv"),
          row.names = FALSE, quote = FALSE)
cat(sprintf("faltan %d L1A (%.1f GB) y %d AST14DEM (%.1f GB)\n", nrow(Cn), sum(Cn$mb) / 1024, nrow(Dn),
            sum(as.numeric(Dn$mb), na.rm = TRUE) / 1024))

# R en Windows escribe fin de linea \r\n; las listas las leen scripts de bash, que no lo quitan
for (f in c(file.path(R, c("P14_franjas_nuevas.csv", "P14_franjas_usadas.csv", "P14_ast14dem_mismas.txt")),
            file.path(M, c("fuera_de_temporada.txt", "L1A/descarga_enero_abril.csv", "AST14DEM/descarga_enero_abril.csv")))) {
  x <- readBin(f, "raw", file.size(f))
  writeBin(x[x != as.raw(13)], f)
}
