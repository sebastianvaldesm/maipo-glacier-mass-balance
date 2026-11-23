# -*- coding: utf-8 -*-
"""
I02. M05c de la rama MicMac + GDEM con CPD sobre los RGI recortados al limite de la cuenca (I01).

Corre P15 con la etapa M05c tal como se corrio la rama, con dos cambios en el texto de los scripts (no
en los archivos):
  - M05c lee las capas rgi70 y rgi60 y sus hipsometrias de datos/ (I01) en vez del inventario completo;
  - las tablas van a tablas/ de esta carpeta, no a las de la rama.
La alineacion, el dominio y el dh por pixel son los mismos de la rama, porque la asignacion de unidades
de P15 sigue leyendo el inventario completo. La union y el inventario nacional se vuelven a medir igual:
sirven de control (tienen que dar lo mismo que la rama).
Uso (entorno venvs/xdem, con PROJ_DATA y GDAL_DATA): python I02_M05c_Recortados.py
"""
import os
import runpy
import sys

RAIZ = r"C:\Users\sebas\OneDrive\Desktop\Memoria de Titulo"
AQUI = os.path.join(RAIZ, "06_Memoria_Final", "Inventarios_Recortados")
DATOS = os.path.join(AQUI, "datos")
TABLAS = os.path.join(AQUI, "tablas")
P15 = os.path.join(RAIZ, "03_DEM_MicMac", "scripts", "P15_cadena_propia.py")
M05C = os.path.join(RAIZ, "02_Balance_Masa_RM", "Cadena_RM", "M05c_Medicion_Primera_Fecha.py")
os.makedirs(TABLAS, exist_ok=True)


def cambiar(src, viejo, nuevo):
    if src.count(viejo) != 1:
        raise SystemExit("El texto aparece %d veces en vez de una: %s" % (src.count(viejo), viejo))
    return src.replace(viejo, nuevo)


F_REC = os.path.join(DATOS, "Inventarios_recortados.gpkg")
m = open(M05C, encoding="utf-8").read()
m = cambiar(m, 'r7 = gpd.read_file(C.F_INV, layer="rgi70")', 'r7 = gpd.read_file(%r, layer="rgi70")' % F_REC)
m = cambiar(m, 'r6 = gpd.read_file(C.F_INV, layer="rgi60")', 'r6 = gpd.read_file(%r, layer="rgi60")' % F_REC)
m = cambiar(m, 'f = os.path.join(C.DIR_BASE, "01_Hipsometria_%s.csv" % capa)',
            'f = os.path.join(%r if capa in ("rgi70", "rgi60") else C.DIR_BASE, "01_Hipsometria_%%s.csv" %% capa)' % DATOS)
f_m05c = os.path.join(DATOS, "_M05c_generado.py")
open(f_m05c, "w", encoding="utf-8").write(m)

p = open(P15, encoding="utf-8").read()
p = cambiar(p, 'C.DIR_TAB = os.path.join(DIR_RAMA, "tablas")', "C.DIR_TAB = %r" % TABLAS)
p = cambiar(p, 'sys.argv = [os.path.join(CADENA, "M05c_Medicion_Primera_Fecha.py")]', "sys.argv = [%r]" % f_m05c)

sys.argv = [P15, "gdem_cpd", "M05c"]
sys.path.insert(0, os.path.dirname(P15))
exec(compile(p, P15, "exec"), {"__name__": "__main__", "__file__": P15})
