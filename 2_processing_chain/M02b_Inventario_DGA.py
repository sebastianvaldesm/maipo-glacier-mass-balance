# -*- coding: utf-8 -*-
"""
M02b. El inventario de la DGA (IPG 2014) de la cuenca alta del Maipo, sin glaciares rocosos, como
capa dga. Es el inventario principal de los resultados. Cae dentro de la union de M02, asi que se
mide sobre el mismo dh por pixel sin volver a alinear.
Sale: 01_Datos/Procesados/Inventarios_UTM19S/Inventario_DGA_2014_cuenca.gpkg
"""

import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import geopandas as gpd
import numpy as np
import pandas as pd
from shapely.ops import unary_union

import comun_rm as C

CRS = "EPSG:32719"
F_IPG = os.path.join(C.RAIZ, "01_Datos/Brutos", "Inventario_DGA", "IPG_2014", "Inventario Publico de Glaciares 2014",
                     "IPG_2014", "IPG_2014.shp")
CUENCA = ["0570", "0572"]

d = gpd.read_file(F_IPG)
d = d[d["COD_SUBC"].isin(CUENCA) & (d["CLASIFICA"] != "GLACIAR ROCOSO")].to_crs(CRS).reset_index(drop=True)
if d["COD_GLA"].duplicated().any():
    raise SystemExit("Hay COD_GLA repetidos entre los glaciares no rocosos de la cuenca: revisar el identificador")
d["geometry"] = d.geometry.make_valid()


def limpio(n):
    if not isinstance(n, str) or not n.strip() or n.strip().upper() in ("S/N", "SN", "NONE", "SIN NOMBRE"):
        return None
    return n.strip().title()


g = gpd.GeoDataFrame(dict(gid=d["COD_GLA"].astype(str), nombre=[limpio(n) for n in d["NOM_GLA"]],
                          CLASIFICA=d["CLASIFICA"], COD_SCUEN=d["COD_SUBC"].astype(str),
                          COD_SSCUEN=d["COD_SSUBC"].astype(str), FUENTE_DIG=d["FUENTE_DIG"],
                          FUENTE_FEC=d["FUENTE_FEC"], INVENT_FEC=d["INVENT_FEC"], AREA_Km2=d["AREA_Km2"],
                          AREA_EXP=d["AREA_EXP"], AREA_CUB=d["AREA_CUB"], HMEDIANA=d["HMEDIANA"]),
                     geometry=d.geometry.values, crs=CRS)
g["area_utm_km2"] = g.area / 1e6
g["anio_fuente"] = g["FUENTE_FEC"].astype(str).str.extract(r"(\d{2,4})$")[0].map(
    lambda a: None if not isinstance(a, str) else int(a) if len(a) == 4 else 2000 + int(a))

# limites, como en M02
gpkg_rm = os.path.join(C.RAIZ, "01_Datos/Procesados", "Inventarios_UTM19S", "RGI_RM_UTM19S.gpkg")
g_rm = unary_union(gpd.read_file(gpkg_rm, layer="limite_rm").to_crs(CRS).geometry.values)
g["frac_en_RM"] = g.geometry.intersection(g_rm).area / g.area
g["frac_en_Chile"] = 1.0
g["nivel"] = "nucleo_RM"

# relacion con la union: fraccion dentro y unidad (y sistema) que mas se superpone
U = gpd.read_file(C.F_INV, layer="union")[["uid", "sistema", "geometry"]]
uu = unary_union(U.geometry.values)
g["frac_en_union"] = g.geometry.intersection(uu).area / g.area
x = gpd.overlay(g[["gid", "geometry"]], U, how="intersection", keep_geom_type=True)
x["a"] = x.area
m = x.sort_values("a", ascending=False).drop_duplicates("gid").set_index("gid")
g["uid_union"] = g["gid"].map(m["uid"])
g["sistema"] = g["gid"].map(m["sistema"])
if g["sistema"].isna().any():
    raise SystemExit("Hay poligonos de la DGA que no tocan la union: %s" % list(g.loc[g.sistema.isna(), "gid"]))

out = C.F_INV_DGA
if os.path.exists(out):
    os.remove(out)
g.to_file(out, layer="dga", driver="GPKG")
g.drop(columns="geometry").to_csv(os.path.join(C.DIR_TAB, "M02b_Inventario_DGA.csv"), index=False)

fuera = unary_union(g.geometry.values).difference(uu).area / 1e6
print("IPG 2014, cuenca alta del Maipo sin glaciares rocosos: %d poligonos, %.2f km2 (AREA_Km2 %.2f)"
      % (len(g), g.area_utm_km2.sum(), g.AREA_Km2.sum()))
print("Fuera de la union: %.3f km2 | poligonos con menos del 99 %% dentro: %d"
      % (fuera, (g.frac_en_union < 0.99).sum()))
print("Por subsubcuenca:")
print(g.groupby("COD_SSCUEN")["area_utm_km2"].agg(["count", "sum"]).round(2).to_string())
print("Anio de la imagen fuente (area, km2):")
print(g.groupby("anio_fuente")["area_utm_km2"].sum().round(2).to_string())
