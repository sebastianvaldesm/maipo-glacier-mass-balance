# -*- coding: utf-8 -*-
"""
M02. Inventarios del dominio y sistemas glaciares.
  1. Baja el RGI 6.0 de la region 17 si no esta (NSIDC, Earthdata Login en ~/.netrc).
  2. Selecciona en el RGI 6.0 y el RGI 7.0 los glaciares que tocan la Region Metropolitana.
  3. Arma la union de los dos inventarios en unidades: cada glaciar del RGI 7.0 con lo que solo el
     RGI 6.0 incluye a su lado, y los trozos del RGI 6.0 lejos de todo glaciar del RGI 7.0. Son
     las unidades que alinea M05.
  4. Sistemas glaciares (poligonos a menos de 1 km entre si) y subcuenca de cada glaciar.
Sale: 01_Datos/Procesados/Inventarios_UTM19S/Inventarios_RM_ASTER.gpkg y tablas/M02_*.csv
"""

import os
import subprocess
import zipfile

import geopandas as gpd
import numpy as np
import pandas as pd
from shapely.ops import unary_union

RAIZ = r"C:\Users\sebas\OneDrive\Desktop\Memoria de Titulo"
DIR_RGI = os.path.join(RAIZ, "01_Datos/Brutos", "Inventario_RGI")
DIR_INV = os.path.join(RAIZ, "01_Datos/Procesados", "Inventarios_UTM19S")
DIR_TAB = os.path.join(RAIZ, "02_Balance_Masa_RM", "Cadena_RM", "tablas")
os.makedirs(DIR_TAB, exist_ok=True)
CRS = "EPSG:32719"

# Dos glaciares cuyos contornos quedan a menos de esta distancia pertenecen al
# mismo sistema. Un kilometro junta los glaciares de un mismo macizo o circo
# sin encadenar macizos vecinos a lo largo de la divisoria.
DIST_SISTEMA = 1000.0

ZIP_RGI6 = os.path.join(DIR_RGI, "nsidc0770_17.rgi60.SouthernAndes.zip")
URL_RGI6 = ("https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0770_rgi_v6/"
            "nsidc0770_17.rgi60.SouthernAndes.zip")

# ---------------------------------------------------------------------------
# 1. RGI 6.0
# ---------------------------------------------------------------------------
if not os.path.exists(ZIP_RGI6):
    cookies = os.path.join(os.environ.get("TEMP", "."), "cookies_nsidc.txt")
    tmp = ZIP_RGI6 + ".parcial"
    cod = subprocess.run(["curl", "-s", "-n", "-L", "-b", cookies, "-c", cookies, "-m", "900",
                          "-w", "%{http_code}", "-o", tmp, URL_RGI6],
                         capture_output=True, text=True).stdout
    if cod != "200" or os.path.getsize(tmp) < 1e6:
        raise SystemExit("No se pudo bajar el RGI 6.0 (HTTP %s)" % cod)
    os.replace(tmp, ZIP_RGI6)
    print("RGI 6.0 bajado:", os.path.getsize(ZIP_RGI6) // 1024, "kB")

with zipfile.ZipFile(ZIP_RGI6) as z:
    shp = [n for n in z.namelist() if n.lower().endswith(".shp")]
print("Shapefile del RGI 6.0 en el zip:", shp)
rgi6_all = gpd.read_file("zip://" + ZIP_RGI6.replace("\\", "/") + "!" + shp[0])
print("RGI 6.0 region 17:", len(rgi6_all), "glaciares")

# ---------------------------------------------------------------------------
# 2. Dominio: los que tocan la Region Metropolitana
# ---------------------------------------------------------------------------
gpkg_rm = os.path.join(DIR_INV, "RGI_RM_UTM19S.gpkg")
lim_rm = gpd.read_file(gpkg_rm, layer="limite_rm").to_crs(CRS)
lim_cl = gpd.read_file(gpkg_rm, layer="limite_chile").to_crs(CRS)
rgi7 = gpd.read_file(gpkg_rm, layer="glaciares_rm").to_crs(CRS)
ipg = gpd.read_file(gpkg_rm, layer="ipg2022_rm").to_crs(CRS)
g_rm = unary_union(lim_rm.geometry.values)
g_cl = unary_union(lim_cl.geometry.values)

rgi6 = rgi6_all.to_crs(CRS)
rgi6 = rgi6[rgi6.intersects(g_rm.buffer(1))].copy()
rgi6["geometry"] = rgi6.geometry.make_valid()
rgi6["area_utm_km2"] = rgi6.area / 1e6
rgi6["frac_en_RM"] = rgi6.geometry.intersection(g_rm).area / rgi6.area
rgi6["frac_en_Chile"] = rgi6.geometry.intersection(g_cl).area / rgi6.area
rgi6["nivel"] = np.where(rgi6["frac_en_Chile"] >= 0.5, "nucleo_RM", "franja_argentina")
rgi6["anio_fuente"] = rgi6["BgnDate"].astype(str).str[:4]
print("RGI 6.0 que tocan la RM:", len(rgi6), "| %.1f km2" % rgi6.area_utm_km2.sum())

# ---------------------------------------------------------------------------
# 3. Subcuenca DGA de cada poligono del RGI 6.0
# ---------------------------------------------------------------------------
rgi7 = rgi7.reset_index(drop=True)
inter = gpd.overlay(rgi6[["RGIId", "geometry"]], rgi7[["rgi_id", "COD_SCUEN", "geometry"]],
                    how="intersection", keep_geom_type=True)
inter["a"] = inter.area
mejor = inter.sort_values("a", ascending=False).drop_duplicates("RGIId")
rgi6 = rgi6.merge(mejor[["RGIId", "COD_SCUEN"]], on="RGIId", how="left")
# Los que no tocan ningun RGI 7.0 toman la subcuenca del poligono IPG 2022 mas
# superpuesto; si tampoco, quedan sin asignar (como en la capa regional).
falta = rgi6["COD_SCUEN"].isna()
if falta.any() and "COD_SCUEN" in ipg.columns:
    i2 = gpd.overlay(rgi6.loc[falta, ["RGIId", "geometry"]], ipg[["COD_SCUEN", "geometry"]],
                     how="intersection", keep_geom_type=True)
    i2["a"] = i2.area
    m2 = i2.sort_values("a", ascending=False).drop_duplicates("RGIId").set_index("RGIId")["COD_SCUEN"]
    rgi6.loc[falta, "COD_SCUEN"] = rgi6.loc[falta, "RGIId"].map(m2)

# ---------------------------------------------------------------------------
# 4. Sistemas glaciares
# ---------------------------------------------------------------------------
todos = pd.concat([rgi7[["geometry"]].assign(fuente="rgi70"),
                   rgi6[["geometry"]].assign(fuente="rgi60")], ignore_index=True)
blob = unary_union(todos.geometry.buffer(DIST_SISTEMA / 2).values)
partes = list(blob.geoms) if blob.geom_type == "MultiPolygon" else [blob]
sis = gpd.GeoDataFrame({"sistema": ["S%03d" % (k + 1) for k in range(len(partes))]},
                       geometry=partes, crs=CRS)


def asignar(gdf, idcol):
    c = gdf[[idcol, "geometry"]].copy()
    c["geometry"] = c.representative_point()
    j = gpd.sjoin(c, sis, how="left", predicate="within")
    return j.drop_duplicates(idcol).set_index(idcol)["sistema"]


rgi7["sistema"] = rgi7["rgi_id"].map(asignar(rgi7, "rgi_id"))
rgi6["sistema"] = rgi6["RGIId"].map(asignar(rgi6, "RGIId"))

# Contorno del sistema = union de sus glaciares (de los dos inventarios), sin el
# buffer de agrupacion; nombre = el del glaciar con nombre mas grande.


def limpio(n):
    if not isinstance(n, str) or not n.strip() or n.strip().upper() in ("S/N", "SN", "NONE"):
        return None
    n = n.strip()
    if len(n) > 2 and n[-2] == " " and n[-1].isalpha():
        n = n[:-2]          # fragmentos del IPG 2022: "OLIVARES BETA A" -> "OLIVARES BETA"
    return n.title()


rgi7["nombre"] = [limpio(a) or limpio(b) for a, b in zip(rgi7.get("glac_name"), rgi7.get("nombre_IPG"))]
filas = []
geoms = []
for s, g in sis.groupby("sistema"):
    m7 = rgi7[rgi7.sistema == s]
    m6 = rgi6[rgi6.sistema == s]
    u = unary_union(list(m7.geometry.values) + list(m6.geometry.values))
    con_nombre = m7.dropna(subset=["nombre"]).sort_values("area_utm_km2", ascending=False)
    nombres = list(dict.fromkeys(con_nombre["nombre"].tolist()))
    filas.append(dict(sistema=s, n_rgi70=len(m7), area_rgi70_km2=m7.area_utm_km2.sum(),
                      n_rgi60=len(m6), area_rgi60_km2=m6.area_utm_km2.sum(),
                      area_nucleo_rgi70_km2=m7.loc[m7.nivel == "nucleo_RM", "area_utm_km2"].sum(),
                      nombre_mayor=nombres[0] if nombres else "",
                      otros_nombres="; ".join(nombres[1:6]),
                      subcuenca=(m7.COD_SCUEN.mode().iloc[0] if m7.COD_SCUEN.notna().any() else ""),
                      xmin=u.bounds[0], ymin=u.bounds[1], xmax=u.bounds[2], ymax=u.bounds[3]))
    geoms.append(u)
tab = pd.DataFrame(filas)
sis_out = gpd.GeoDataFrame(tab, geometry=geoms, crs=CRS)
sis_out = sis_out.sort_values("area_rgi70_km2", ascending=False).reset_index(drop=True)

# ---------------------------------------------------------------------------
# 4b. Donde los dos inventarios no coinciden
# ---------------------------------------------------------------------------
# Superficie que un inventario llama glaciar y el otro no. M05 la mide como si
# fuera un glaciar mas: si adelgaza como hielo, el inventario que la omite se
# queda corto; si no cambia, el que la incluye cuenta roca o nieve estacional.
# Se compara contra TODOS los poligonos del otro inventario dentro del
# rectangulo regional, no solo los del dominio, y se eliminan las astillas de
# borde (apertura de 30 m) y los trozos de menos de 0,02 km2.
aoi = gpd.read_file(gpkg_rm, layer="aoi_regional").to_crs(CRS)
g_aoi = unary_union(aoi.geometry.values)
r7_shp = [f for f in os.listdir(DIR_RGI) if f.endswith("southern_andes.shp")][0]
rgi7_all = gpd.read_file(os.path.join(DIR_RGI, r7_shp), bbox=tuple(aoi.to_crs(rgi6_all.crs).total_bounds))
rgi7_all = rgi7_all.to_crs(CRS)
u7 = unary_union(rgi7_all[rgi7_all.intersects(g_aoi)].geometry.make_valid().values)
u6 = unary_union(rgi6_all.to_crs(CRS).loc[lambda d: d.intersects(g_aoi)].geometry.make_valid().values)


def diferencia(gdf, idcol, otro, pref):
    filas = []
    for _, g in gdf.iterrows():
        d = g.geometry.difference(otro).buffer(-30).buffer(30)
        if d.is_empty:
            continue
        for p in (list(d.geoms) if hasattr(d, "geoms") else [d]):
            if p.area >= 0.02e6:
                filas.append(dict(origen=g[idcol], nivel=g["nivel"], COD_SCUEN=g.get("COD_SCUEN"),
                                  sistema=g["sistema"], geometry=p))
    out = gpd.GeoDataFrame(filas, geometry="geometry", crs=CRS)
    out.insert(0, "gid", ["%s_%04d" % (pref, k + 1) for k in range(len(out))])
    out["area_utm_km2"] = out.area / 1e6
    return out


solo6 = diferencia(rgi6, "RGIId", u7, "D60")
solo7 = diferencia(rgi7, "rgi_id", u6, "D70")
print("Solo en el RGI 6.0: %d trozos, %.1f km2 | solo en el RGI 7.0: %d trozos, %.1f km2"
      % (len(solo6), solo6.area_utm_km2.sum(), len(solo7), solo7.area_utm_km2.sum()))

# ---------------------------------------------------------------------------
# 4c. La union de los dos inventarios
# ---------------------------------------------------------------------------
# "Como medimos cambio de elevacion, el lugar donde uno dice que no hay
# glaciar deberia ser estable y no generar un dh": el dominio de medicion pasa
# a ser la union de los contornos del RGI 6.0 y del RGI 7.0 que tocan la RM, la
# mayor extension que cualquiera de los dos asigna a los glaciares a comienzos
# de siglo. Se reparte en UNIDADES para alinear y medir glaciar por glaciar:
#   - cada glaciar del RGI 7.0, mas las superficies que solo el RGI 6.0 incluye
#     y que estan a su lado: un trozo que toca uno o mas glaciares va al que
#     comparte mas borde con el; uno que no toca ninguno va al mas cercano si
#     esta a menos de DIST_ASIGNA (queda dentro de la ventana de 2 km);
#   - los trozos que quedan mas lejos son unidades propias ("U60-nnnn").
# Lo que se agrega es lo que ningun poligono del RGI 7.0 cubre (u7: todos los
# del rectangulo, no solo los del dominio). Un poligono grande del RGI 6.0 que
# cruza una divisoria tambien cubre hielo que el RGI 7.0 asigna a un glaciar
# vecino que no toca la RM; ese hielo es de otro glaciar y no se agrega.
DIST_ASIGNA = 2000.0
rgi7 = rgi7.reset_index(drop=True)
extra = unary_union(rgi6.geometry.values).difference(u7)
partes = [p for p in (list(extra.geoms) if hasattr(extra, "geoms") else [extra])
          if p.geom_type == "Polygon" and p.area >= 1.0]
sidx = rgi7.sindex
asignado = []
for p in partes:
    cand = list(sidx.query(p.buffer(DIST_ASIGNA), predicate="intersects"))
    if not cand:
        asignado.append(None)
        continue
    dist = [p.distance(rgi7.geometry.iloc[k]) for k in cand]
    tocan = [k for k, d in zip(cand, dist) if d < 1.0]
    if tocan:
        borde = [p.boundary.intersection(rgi7.geometry.iloc[k].buffer(1.0)).length for k in tocan]
        asignado.append(tocan[int(np.argmax(borde))])
    elif min(dist) <= DIST_ASIGNA:
        asignado.append(cand[int(np.argmin(dist))])
    else:
        asignado.append(None)

filas_u = []
for k, g in rgi7.iterrows():
    mias = [p for p, a in zip(partes, asignado) if a == k]
    geom = unary_union([g.geometry] + mias) if mias else g.geometry
    filas_u.append(dict(uid=g["rgi_id"], tipo="rgi70", rgi_id=g["rgi_id"], nombre=g.get("nombre"),
                        COD_SCUEN=g.get("COD_SCUEN"), COD_SSCUEN=g.get("COD_SSCUEN"), sistema=g["sistema"],
                        area_rgi70_km2=g.geometry.area / 1e6, area_solo_rgi60_km2=sum(m.area for m in mias) / 1e6,
                        n_trozos_rgi60=len(mias), geometry=geom))
sueltas = [p for p, a in zip(partes, asignado) if a is None]
r6_idx = rgi6.reset_index(drop=True)
for j, p in enumerate(sueltas):
    inter = r6_idx.geometry.intersection(p).area
    k6 = int(inter.idxmax())
    filas_u.append(dict(uid="U60-%04d" % (j + 1), tipo="solo_rgi60", rgi_id=None, nombre=None,
                        COD_SCUEN=r6_idx.loc[k6, "COD_SCUEN"], COD_SSCUEN=None, sistema=r6_idx.loc[k6, "sistema"],
                        origen_rgi60=r6_idx.loc[k6, "RGIId"], area_rgi70_km2=0.0,
                        area_solo_rgi60_km2=p.area / 1e6, n_trozos_rgi60=1, geometry=p))
union = gpd.GeoDataFrame(filas_u, geometry="geometry", crs=CRS)
union["geometry"] = union.geometry.make_valid()
union["area_utm_km2"] = union.area / 1e6
union["frac_en_Chile"] = union.geometry.intersection(g_cl).area / union.area
union["frac_en_RM"] = union.geometry.intersection(g_rm).area / union.area
print("Union: %d unidades, %.1f km2 (RGI 7.0 %.1f + solo RGI 6.0 %.1f, de ellos %.1f km2 en %d unidades sueltas)"
      % (len(union), union.area_utm_km2.sum(), union.area_rgi70_km2.sum(), union.area_solo_rgi60_km2.sum(),
         union.loc[union.tipo == "solo_rgi60", "area_utm_km2"].sum(), (union.tipo == "solo_rgi60").sum()))

# ---------------------------------------------------------------------------
# 5. Salidas
# ---------------------------------------------------------------------------
out = os.path.join(DIR_INV, "Inventarios_RM_ASTER.gpkg")
if os.path.exists(out):
    os.remove(out)
keep7 = ["rgi_id", "glac_name", "nombre", "nombre_IPG", "src_date", "area_km2", "area_utm_km2",
         "zmed_m", "nivel", "frac_en_RM", "frac_en_Chile", "COD_SCUEN", "COD_SSCUEN", "unidad_estudio",
         "sistema", "geometry"]
rgi7[[c for c in keep7 if c in rgi7.columns]].to_file(out, layer="rgi70", driver="GPKG")
keep6 = ["RGIId", "GLIMSId", "BgnDate", "EndDate", "Name", "Area", "area_utm_km2", "Zmed", "nivel",
         "frac_en_RM", "frac_en_Chile", "anio_fuente", "COD_SCUEN", "sistema", "geometry"]
rgi6[[c for c in keep6 if c in rgi6.columns]].to_file(out, layer="rgi60", driver="GPKG")
sis_out.to_file(out, layer="sistemas", driver="GPKG")
solo6.to_file(out, layer="solo_rgi60", driver="GPKG")
solo7.to_file(out, layer="solo_rgi70", driver="GPKG")
union.to_file(out, layer="union", driver="GPKG")
gpd.GeoDataFrame({"area_km2": [union.area.sum() / 1e6]}, geometry=[unary_union(union.geometry.values)],
                 crs=CRS).to_file(out, layer="union_disuelta", driver="GPKG")
union.drop(columns="geometry").to_csv(os.path.join(DIR_TAB, "M02_Union.csv"), index=False)
sis_out.drop(columns="geometry").to_csv(os.path.join(DIR_TAB, "M02_Sistemas.csv"), index=False)


def resumen(g, nombre, area_col):
    # Desde la segunda tanda de notas no se distingue nucleo de
    # franja: el dominio son todos los glaciares que tocan la RM. La columna
    # nivel queda solo como referencia.
    r = []
    for niv in ["todos", "nucleo_RM", "franja_argentina"]:
        d = g if niv == "todos" else g[g.nivel == niv]
        r.append(dict(inventario=nombre, nivel=niv, n=len(d), area_km2=round(d[area_col].sum(), 2),
                      mediana_km2=round(d[area_col].median(), 3) if len(d) else np.nan))
    return r


res = pd.DataFrame(resumen(rgi7, "RGI 7.0", "area_utm_km2") + resumen(rgi6, "RGI 6.0", "area_utm_km2"))
# Fechas de la fuente
f7 = rgi7["src_date"].astype(str).str[:4].value_counts().to_dict()
f6 = rgi6["anio_fuente"].value_counts().to_dict()
res["fechas_fuente"] = [str(f7)] * 3 + [str(f6) + " a 2003-05-31 (EndDate)"] * 3
res = pd.concat([res, pd.DataFrame([dict(inventario="Union", nivel="todos", n=len(union),
                                         area_km2=round(union.area_utm_km2.sum(), 2),
                                         mediana_km2=round(union.area_utm_km2.median(), 3),
                                         fechas_fuente="RGI 7.0 mas lo que solo incluye el RGI 6.0")])])
res.to_csv(os.path.join(DIR_TAB, "M02_Inventarios_Resumen.csv"), index=False)
print(res.to_string(index=False))
print("\nSistemas:", len(sis_out))
print(sis_out.drop(columns="geometry").head(25).to_string(index=False))
