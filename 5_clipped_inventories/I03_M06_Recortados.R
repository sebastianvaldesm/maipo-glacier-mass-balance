# I03. M06 de la rama MicMac + GDEM con CPD sobre las tablas de I02 (RGI recortados al limite de la
# cuenca), con los periodos cortados a mitad de 2010 y la recta quebrada, igual que T01 de Tramos_2010.
# Cambia solo el texto de M06 al vuelo: lee las tablas de I02, el inventario recortado de I01 y sus
# hipsometrias, y escribe aqui. El inventario nacional tiene que dar lo mismo que Tramos_2010/corte_2010.5.
# Uso: Rscript I03_M06_Recortados.R
# Sale: corte_2010.5/tablas/ y corte_2010.5/M06_resultados.rds
aqui   <- "C:/maipo/proyecto/06_Memoria_Final/Inventarios_Recortados"
raiz_t <- "C:/maipo/proyecto/06_Memoria_Final/Tramos_2010"
cadena <- "C:/maipo/proyecto/02_Balance_Masa_RM/Cadena_RM"
rama   <- "C:/maipo/trabajo/cadena_propia/gdem_cpd"
salida <- file.path(aqui, "corte_2010.5")
dir_t  <- file.path(salida, "tablas")
dir.create(dir_t, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(aqui, "tablas", "M05c_Serie.csv"))) stop("Falta I02")
Sys.setenv(CADENA_RM_SUFIJO = "", CADENA_RM_SALIDA = "M05_salida")

cambiar <- function(src, viejo, nuevo, veces = 1) {
  i <- grep(viejo, src, fixed = TRUE)
  if (length(i) != veces) stop("El texto aparece ", length(i), " veces en vez de ", veces, ": ", viejo)
  src[i] <- gsub(viejo, nuevo, src[i], fixed = TRUE)
  src
}
q <- function(x) sprintf('"%s"', x)

src <- readLines(file.path(cadena, "M06_Series_y_Balance.R"), encoding = "UTF-8")
# lo mismo que T01 para la rama gdem_cpd
src <- cambiar(src, 'dir_c   <- file.path(base, "02_Balance_Masa_RM", "Cadena_RM")', paste("dir_c   <-", q(rama)))
src <- cambiar(src, 'file.path(dir_tab, "M02_Sistemas.csv")', sprintf('file.path(%s, "M02_Sistemas.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_tab, "M02_Subsubcuencas_DGA.csv")',
               sprintf('file.path(%s, "M02_Subsubcuencas_DGA.csv")', q(file.path(cadena, "tablas"))))
src <- cambiar(src, 'file.path(dir_c, "datos_externos", "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")',
               sprintf('file.path(%s, "MB_all_glaciers_Andes_rgi60_2000.0-2018.3.csv")', q(file.path(cadena, "datos_externos"))))
src <- cambiar(src, 'dir_out <- if (SUF == "") dir_tab else file.path(dir_tab, paste0("variante", SUF))', paste("dir_out <-", q(dir_t)))
src <- cambiar(src, 'file.path(if (SUF == "") dir_c else dir_out, "M06_resultados.rds")', sprintf('file.path(%s, "M06_resultados.rds")', q(salida)))
src <- cambiar(src, 'T_CORTE <- 2013.5', 'T_CORTE <- 2010.5')
# lo propio de esta variante: tablas de I02, inventario recortado de I01 y sus hipsometrias
src <- cambiar(src, 'dir_tab <- file.path(dir_c, "tablas")', paste("dir_tab <-", q(file.path(aqui, "tablas"))))
src <- cambiar(src, 'f_inv   <- file.path(base, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg")',
               paste("f_inv   <-", q(file.path(aqui, "datos", "Inventarios_recortados.gpkg"))))
src <- cambiar(src, 'h <- read.csv(file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER", paste0("01_Hipsometria_", capa, ".csv")))',
               sprintf('h <- read.csv(file.path(if (capa %%in%% c("rgi70", "rgi60")) %s else file.path(base, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER"), paste0("01_Hipsometria_", capa, ".csv")))',
                       q(file.path(aqui, "datos"))))
# con el recorte no quedan glaciares del RGI 7.0 en la vertiente argentina: esa fila de la ventana 2018 sale
src <- cambiar(src, 'v18(R18[R18$conjunto == "rgi70" & R18$COD_SCUEN %in% "ARG", ], "Vertiente argentina"))', 'NULL)')
source(file.path(raiz_t, "quebrada.R"))
src <- aplicar_quebrada(src)
cat("M06 con los RGI recortados ->", salida, "\n")
eval(parse(text = src, encoding = "UTF-8"), envir = new.env())
cat("Listo.\n")
