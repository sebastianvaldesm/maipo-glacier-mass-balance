# C02. Tabla A2 del articulo: la cuenca alta del Maipo (inventario de la DGA) con cada version del metodo
#
# Junta las corridas de C01 (versiones anteriores), la metodologia final y su variante con dos rectas
# separadas (Tramos_2010), y la rama con el TanDEM-X de 90 m con CPD. Todas con error por remuestreo de
# escenas; los periodos con la recta quebrada en 2010,5, salvo la fila de dos rectas separadas.
# Entra: C:/maipo/trabajo/metodologias_m06/<metodologia>/tablas y 06_Memoria_Final/Tramos_2010
# Sale:  Tabla_A2_Metodologias.csv
# Uso:   Rscript C02_Tabla_A2.R

base <- "C:/maipo/proyecto"
TR <- file.path(base, "06_Memoria_Final", "Tramos_2010")
M  <- "C:/maipo/trabajo/metodologias_m06"

filas <- data.frame(
  metodologia = c("AST_13M_ALT50", "AST_13M", "AST_NKD", "MM_TDX90_13M", "MM_TDX90_CPD", "MM_GDEM_13M",
                  "MM_GDEM_CPD_SEPARADAS", "MM_GDEM_CPD"),
  modelos = c("AST14DEM", "AST14DEM", "AST14DEM", "MicMac", "MicMac", "MicMac", "MicMac", "MicMac"),
  referencia = c("GDEM", "GDEM", "GDEM", "TanDEM-X 90 m", "TanDEM-X 90 m", "GDEM", "GDEM", "GDEM"),
  alineacion = c("13 variantes, sin la regla del 90 %", "13 variantes", "Nuth y Kaab mas plano", "13 variantes",
                 "14 variantes", "13 variantes", "14 variantes, dos rectas separadas", "14 variantes (final)"),
  carpeta = c(file.path(M, c("AST_13M_ALT50", "AST_13M", "AST_NKD", "MM_TDX90_13M")),
              file.path(TR, "tdx90_cpd", "corte_2010.5"), file.path(M, "MM_GDEM_13M"),
              file.path(TR, "corte_2010.5_rectas_separadas"), file.path(TR, "corte_2010.5")),
  stringsAsFactors = FALSE)

valores <- do.call(rbind, lapply(filas$carpeta, function(d) {
  a <- read.csv(file.path(d, "tablas", "M06_Agregados.csv"))
  x <- a[a$conjunto == "dga" & a$grupo == "Cuenca alta del Maipo", ]
  data.frame(tasa_2001_2025 = x$tasa_m_a, error_tasa = x$error_tasa,
             balance_850 = x$balance_850, error_balance_850 = x$error_balance_850,
             masa_Gt = x$dM_Mt_850 / 1000, error_masa_Gt = x$error_dM_850 / 1000,
             tasa_2001_2010 = x$tasa_2001_2013, error_2001_2010 = x$error_t1,
             tasa_2010_2025 = x$tasa_2013_2026, error_2010_2025 = x$error_t2,
             area_serie_propia_pct = x$area_propio_pct)
}))
out <- cbind(filas[, c("metodologia", "modelos", "referencia", "alineacion")], round(valores, 4))
write.csv(out, file.path(base, "06_Memoria_Final", "Metodologias_anteriores", "Tabla_A2_Metodologias.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
print(out[, c("metodologia", "tasa_2001_2025", "tasa_2001_2010", "tasa_2010_2025")], row.names = FALSE)
