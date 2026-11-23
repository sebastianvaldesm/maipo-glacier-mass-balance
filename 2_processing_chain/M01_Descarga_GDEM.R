# M01. Descarga del ASTER GDEM V3 (ASTGTM.003) sobre la Region Metropolitana
#
# Baja del LP DAAC las seis teselas de 1 x 1 grado que cubren el rectangulo regional, cada una con
# su archivo NUM (cuantas escenas ASTER hay en cada pixel). Es incremental: solo baja lo que falta.
# Necesita una cuenta Earthdata en ~/.netrc.
#
# El GDEM es la base topografica (pendiente, mascara de roca, hipsometria) y la referencia a la que
# se alinea cada modelo. Se usan solo los pixeles hechos con escenas ASTER (NUM 3-50 y 63-110).
#
# Sale: 01_Datos/Brutos/ASTER_GDEM_V3/

base    <- "C:/Users/sebas/OneDrive/Desktop/Memoria de Titulo"
destino <- file.path(base, "01_Datos/Brutos", "ASTER_GDEM_V3")
dir.create(destino, recursive = TRUE, showWarnings = FALSE)

# Las teselas se nombran por la esquina suroeste: S34W071 va de -34 a -33 de
# latitud y de -71 a -70 de longitud. El rectangulo regional va de -34,39 a
# -32,93 y de -70,37 a -69,64.
teselas <- c("S33W071", "S33W070", "S34W071", "S34W070", "S35W071", "S35W070")
url_base <- "https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-protected/ASTGTM.003/"

if (Sys.which("curl") == "") stop("No se encontro curl en el PATH.")
cookies <- file.path(tempdir(), "cookies_gdem.txt")

bajar <- function(url, salida) {
  tmp <- paste0(salida, ".parcial")
  cod <- system2("curl", c("-s", "-n", "-L", "-b", shQuote(cookies), "-c", shQuote(cookies),
                           "--retry", "3", "--retry-delay", "5", "-m", "900",
                           "-w", "%{http_code}", "-o", shQuote(tmp), shQuote(url)), stdout = TRUE)
  if (identical(cod, "200") && file.exists(tmp) && file.size(tmp) > 1e5) {
    file.rename(tmp, salida); return(TRUE)
  }
  if (file.exists(tmp)) file.remove(tmp)
  message("  fallo HTTP ", cod); FALSE
}

filas <- list()
for (t in teselas) for (tipo in c("dem", "num")) {
  nom <- sprintf("ASTGTMV003_%s_%s.tif", t, tipo)
  sal <- file.path(destino, nom)
  if (!file.exists(sal)) {
    message("Bajando ", nom)
    bajar(paste0(url_base, nom), sal)
  }
  filas[[length(filas) + 1]] <- data.frame(tesela = t, tipo = tipo, archivo = nom,
                                           url = paste0(url_base, nom),
                                           bytes = if (file.exists(sal)) file.size(sal) else NA,
                                           estado = if (file.exists(sal)) "ok" else "falta")
}
inv <- do.call(rbind, filas)
write.csv(inv, file.path(destino, "Inventario_GDEM.csv"), row.names = FALSE)
print(inv[, c("archivo", "bytes", "estado")], row.names = FALSE)
