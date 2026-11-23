# V02. Prueba de los meses: corre M06 sobre cada variante de V01, con los periodos cortados en 2010 y
# la recta quebrada, igual que T01.
# Uso: Rscript V02_M06_Meses.R <variante>
# Sale: C:/maipo/trabajo/variantes_meses_mm/<variante>/m06/
args <- commandArgs(trailingOnly = TRUE)
VAR <- c("solo_enero", "solo_febrero", "solo_marzo", "solo_abril", "solo_marzo_abril", "todos_los_meses")
if (length(args) != 1 || !args[1] %in% VAR) stop("Uso: Rscript V02_M06_Meses.R <", paste(VAR, collapse = "|"), ">")
v <- args[1]
dir_v  <- file.path("C:/maipo/trabajo/variantes_meses_mm", v)
tab_v  <- file.path(dir_v, "tablas")
out_v  <- file.path(dir_v, "m06")
cadena <- "C:/maipo/proyecto/02_Balance_Masa_RM/Cadena_RM"
rama   <- "C:/maipo/trabajo/cadena_propia/gdem_cpd"
if (!file.exists(file.path(tab_v, "M05c_Serie.csv"))) stop("Falta V01 de la variante: ", tab_v)
dir.create(file.path(out_v, "tablas"), showWarnings = FALSE, recursive = TRUE)
Sys.setenv(CADENA_RM_SUFIJO = "", CADENA_RM_SALIDA = "M05_salida")

cambiar <- function(src, viejo, nuevo, veces = 1) {
  i <- grep(viejo, src, fixed = TRUE)
  if (length(i) != veces) stop("El texto aparece ", length(i), " veces en vez de ", veces, ": ", viejo)
  src[i] <- gsub(viejo, nuevo, src[i], fixed = TRUE)
  src
}
q <- function(x) sprintf('"%s"', x)

src <- readLines(file.path(cadena, "M06_Series_y_Balance.R"), encoding = "UTF-8")
# lo mismo que P17 y T01 para la rama gdem_cpd
src <- cambiar(src, 'dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")', paste("dir_c   <-", q(rama)))
src <- cambiar(src, 'file.path(dir_tab, "M02_Sistemas.csv")', sprintf('file.path(%s, "M02_Sistemas.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_tab, "M02_Subsubcuencas_DGA.csv")',
               sprintf('file.path(%s, "M02_Subsubcuencas_DGA.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_c, "datos_externos", "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")',
               sprintf('file.path(%s, "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")', q(file.path(cadena, "datos_externos"))))
# lo propio de la prueba de los meses: el M05c de la variante, salida aparte y tramos cortados en 2010
src <- cambiar(src, 'file.path(dir_tab, paste0("M05c_', sprintf('file.path(%s, paste0("M05c_', q(tab_v)), veces = 4)
src <- cambiar(src, 'dir_out <- if (SUF == "") dir_tab else file.path(dir_tab, paste0("variante", SUF))', paste("dir_out <-", q(file.path(out_v, "tablas"))))
src <- cambiar(src, 'file.path(if (SUF == "") dir_c else dir_out, "M06_resultados.rds")', sprintf('file.path(%s, "M06_resultados.rds")', q(out_v)))
src <- cambiar(src, 'T_CORTE <- 2013.5', 'T_CORTE <- 2010.5')
# los tramos como recta quebrada continua, igual que en Tramos_2010; la corrida con dos rectas
# separadas quedo en <variante>/m06_rectas_separadas/
source("C:/maipo/proyecto/06_Memoria_Final/Tramos_2010/quebrada.R")
src <- aplicar_quebrada(src)
cat("M06 de la cadena, variante", v, "->", out_v, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())
cat("Listo.\n")
