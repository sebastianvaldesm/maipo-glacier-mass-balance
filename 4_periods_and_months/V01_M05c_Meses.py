# -*- coding: utf-8 -*-
"""
V01. Prueba de los meses: repite M05c de la rama MicMac + GDEM con CPD usando solo las escenas de un
mes, de marzo y abril, o todas sin la regla de temporada. No vuelve a alinear: enlaza el M05_salida
de la rama.
Uso: python V01_M05c_Meses.py <variante>
Sale: C:/maipo/trabajo/variantes_meses_mm/<variante>/tablas/
"""
import ctypes
import glob
import os
import runpy
import shutil
import sys

MESES = {"solo_enero": [1], "solo_febrero": [2], "solo_marzo": [3], "solo_abril": [4],
         "solo_marzo_abril": [3, 4], "todos_los_meses": [1, 2, 3, 4]}
RAMA = r"C:\maipo\trabajo\cadena_propia\gdem_cpd"
P15 = r"C:\maipo\proyecto\03_DEM_MicMac\scripts\P15_cadena_propia.py"
RAIZ_V = r"C:\maipo\trabajo\variantes_meses_mm"

if len(sys.argv) != 2 or sys.argv[1] not in MESES:
    sys.exit(__doc__)
VARIANTE = sys.argv[1]
DIR_V = os.path.join(RAIZ_V, VARIANTE)
ENTRADA = os.path.join(DIR_V, "entrada")
TABLAS = os.path.join(DIR_V, "tablas")
os.makedirs(TABLAS, exist_ok=True)

# entrada: enlaces duros a los archivos de las escenas del mes (mismo disco, no ocupa espacio)
if os.path.isdir(ENTRADA):
    shutil.rmtree(ENTRADA)
os.makedirs(ENTRADA)
n = 0
escenas = set()
for f in glob.glob(os.path.join(RAMA, "M05_salida", "*")):
    b = os.path.basename(f)
    if int(b[5:7]) in MESES[VARIANTE]:
        os.link(f, os.path.join(ENTRADA, b))
        n += 1
        escenas.add(b[:17])
print("%s | meses %s | %d escenas, %d archivos enlazados" % (VARIANTE, MESES[VARIANTE], len(escenas), n), flush=True)

ctypes.windll.kernel32.SetPriorityClass(ctypes.windll.kernel32.GetCurrentProcess(), 0x00004000)  # bajo lo normal

# P15 fija la entrada, la regla de meses y la carpeta de tablas de la rama; aqui se cambian justo despues
_update = os.environ.update


def _update_variante(*a, **k):
    _update(*a, **k)
    if "CADENA_RM_TEMPORADA" in k:
        _update(CADENA_RM_SALIDA=ENTRADA, CADENA_RM_TEMPORADA="todas", CADENA_RM_SUFIJO="")
        sys.modules["comun_rm"].DIR_TAB = TABLAS
        print("Variante %s: entrada %s, tablas %s" % (VARIANTE, ENTRADA, TABLAS), flush=True)


os.environ.update = _update_variante
sys.argv = [P15, "gdem_cpd", "M05c"]
runpy.run_path(P15, run_name="__main__")
