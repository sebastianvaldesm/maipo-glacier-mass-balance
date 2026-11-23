#!/bin/bash
# Corre la prueba de los meses completa: V01 y V02 para cada variante y despues V03.
cd "$(dirname "$0")"
mkdir -p logs
export PROJ_DATA="C:/Users/sebas/miniconda3/envs/hydro/Library/share/proj"
export GDAL_DATA="C:/Users/sebas/miniconda3/envs/hydro/Library/share/gdal"
export PYTHONIOENCODING=utf-8
VARS="${@:-solo_enero solo_febrero solo_marzo solo_abril solo_marzo_abril todos_los_meses}"
for v in $VARS; do
  if [ ! -f "/c/Users/sebas/Prueba_MMASTER/variantes_meses_mm/$v/tablas/M05c_Serie.csv" ]; then
    /c/Users/sebas/venvs/xdem/Scripts/python.exe -u -W ignore V01_M05c_Meses.py "$v" > "logs/V01_$v.log" 2>&1 || { echo "falla V01 $v"; exit 1; }
  fi
  if [ ! -f "/c/Users/sebas/Prueba_MMASTER/variantes_meses_mm/$v/m06/tablas/M06_Agregados.csv" ]; then
    # M06 puede caer en sus contrastes finales (ventana de 2018) cuando un mes deja pocos glaciares; lo que usa
    # V03 (agregados y tasa por glaciar) ya esta escrito para entonces, y eso es lo que se exige
    "/c/Program Files/R/R-4.5.1/bin/Rscript.exe" V02_M06_Meses.R "$v" > "logs/V02_$v.log" 2>&1 || echo "aviso V02 $v: M06 no termino sus contrastes"
    [ -f "/c/Users/sebas/Prueba_MMASTER/variantes_meses_mm/$v/m06/tablas/M06_Tasa_por_Glaciar.csv" ] || { echo "falla V02 $v"; exit 1; }
  fi
  echo "listo $v $(date +%H:%M)"
done
echo "todas listas"
