# -*- coding: utf-8 -*-
"""
M01b. Capas de exclusion de la mascara de roca estable que no vienen de los inventarios:
glaciares rocosos de GLIMS, mineria (Maus et al., 2022), cobertura de suelo ESA WorldCover 2021
y, desde OpenStreetMap, obras, pistas y andariveles de esqui, represas, glaciares y humedales.
Sale: 01_Datos/Capas_Auxiliares/exclusiones_rm_aster.gpkg y el WorldCover recortado.
"""

import json
import os
import time

import geopandas as gpd
import numpy as np
import rasterio
import requests
import shapely.ops
from rasterio.merge import merge
from rasterio.warp import transform_bounds
from rasterio.windows import from_bounds
from shapely.geometry import LineString, Polygon, box

RAIZ = r"C:\Users\sebas\OneDrive\Desktop\Memoria de Titulo"
BRUTOS = os.path.join(RAIZ, "01_Datos/Brutos")
AUX = os.path.join(RAIZ, "01_Datos/Capas_Auxiliares")
F_GDEM = os.path.join(RAIZ, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER", "01_GDEM.tif")
SALIDA = os.path.join(AUX, "exclusiones_rm_aster.gpkg")
CRS = "EPSG:32719"
UA = {"User-Agent": "memoria-glaciares-UTFSM"}

with rasterio.open(F_GDEM) as r:
    rect_utm = box(*r.bounds)
    lon0, lat0, lon1, lat1 = transform_bounds(r.crs, "EPSG:4326", *r.bounds)
lon0, lat0, lon1, lat1 = lon0 - 0.02, lat0 - 0.02, lon1 + 0.02, lat1 + 0.02
print("Rectangulo: lon %.3f a %.3f, lat %.3f a %.3f" % (lon0, lon1, lat0, lat1))


def bajar(url, destino, minimo=1000):
    if os.path.exists(destino) and os.path.getsize(destino) >= minimo:
        return
    os.makedirs(os.path.dirname(destino), exist_ok=True)
    tmp = destino + ".parcial"
    with requests.get(url, headers=UA, stream=True, timeout=600) as r:
        r.raise_for_status()
        with open(tmp, "wb") as f:
            for b in r.iter_content(1 << 20):
                f.write(b)
    if os.path.getsize(tmp) < minimo:
        raise SystemExit("Descarga incompleta: " + url)
    os.replace(tmp, destino)
    print("  bajado", os.path.basename(destino), os.path.getsize(destino) // 1024, "kB")


def al_rectangulo(g):
    g = g.to_crs(CRS)
    g = g[g.intersects(rect_utm)].copy()
    g["geometry"] = g.geometry.make_valid()
    return g


capas = {}

# --- 1. Mineria: Maus et al. (2022), version 2 ------------------------------------
f_maus = os.path.join(BRUTOS, "Mineria_Maus2022", "global_mining_polygons_v2.gpkg")
bajar("https://download.pangaea.de/dataset/942325/files/global_mining_polygons_v2.gpkg", f_maus, 1e6)
m = gpd.read_file(f_maus, bbox=(lon0, lat0, lon1, lat1))
m = al_rectangulo(m)
capas["mineria_maus2022"] = m
print("Mineria (Maus 2022): %d poligonos, %.2f km2 en el rectangulo" % (len(m), m.area.sum() / 1e6))

# --- 2. GLIMS: todos los contornos del rectangulo -----------------------------------
f_glims = os.path.join(BRUTOS, "GLIMS", "GLIMS_Glacier_Outlines_RM.json")
# Este GeoServer espera la caja en orden lon/lat (con lat/lon devuelve 0 entidades)
url_glims = ("https://www.glims.org/geoserver/wfs?service=WFS&version=1.1.0&request=GetFeature"
             "&typeName=GLIMS:GLIMS_Glacier_Outlines&outputFormat=application/json"
             "&srsName=EPSG:4326&bbox=%.4f,%.4f,%.4f,%.4f,EPSG:4326" % (lon0, lat0, lon1, lat1))
bajar(url_glims, f_glims, 1e5)
g = gpd.read_file(f_glims)
if g.crs is None:
    g = g.set_crs("EPSG:4326")
# WFS 1.1.0 con EPSG:4326 puede devolver lat/lon invertidos: se corrige si hace falta
cx = g.geometry.representative_point().x.median()
if not (-80 < cx < -60):
    g["geometry"] = g.geometry.map(lambda s: shapely.ops.transform(lambda x, y: (y, x), s))
g = al_rectangulo(g)
keep = [c for c in ("glac_id", "anlys_id", "subm_id", "chief_affl", "src_date", "line_type", "db_area") if c in g.columns]
g = g[keep + ["geometry"]]
capas["glims"] = g
print("GLIMS: %d contornos en el rectangulo" % len(g), g.groupby("chief_affl").size().to_dict()
      if "chief_affl" in g.columns else "")

# --- 3. ESA WorldCover 2021 v200 ------------------------------------------------------
f_wc = os.path.join(BRUTOS, "ESA_WorldCover", "ESA_WorldCover_2021_v200_RM.tif")
if not os.path.exists(f_wc):
    os.makedirs(os.path.dirname(f_wc), exist_ok=True)
    base_url = "/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map/"
    teselas = ["ESA_WorldCover_10m_2021_v200_S36W072_Map.tif", "ESA_WorldCover_10m_2021_v200_S33W072_Map.tif"]
    with rasterio.Env(GDAL_DISABLE_READDIR_ON_OPEN="EMPTY_DIR", CPL_VSIL_CURL_ALLOWED_EXTENSIONS=".tif"):
        src = [rasterio.open(base_url + t) for t in teselas]
        mosaico, tr = merge(src, bounds=(lon0, lat0, lon1, lat1), nodata=0)
        perfil = src[0].profile
        for s in src:
            s.close()
    perfil.update(driver="GTiff", height=mosaico.shape[1], width=mosaico.shape[2], transform=tr,
                  compress="deflate", tiled=True, blockxsize=512, blockysize=512, nodata=0)
    perfil.pop("photometric", None)
    with rasterio.open(f_wc, "w", **perfil) as dst:
        dst.write(mosaico)
    print("  WorldCover recortado:", mosaico.shape, os.path.getsize(f_wc) // 1024, "kB")
with rasterio.open(f_wc) as r:
    a = r.read(1)
    v, n = np.unique(a, return_counts=True)
    print("WorldCover: clases en el rectangulo (%):", {int(k): round(100 * c / a.size, 2) for k, c in zip(v, n)})

# --- 4. OpenStreetMap: otras intervenciones ---------------------------------------------
SERV = ["https://overpass-api.de/api/interpreter", "https://lz4.overpass-api.de/api/interpreter",
        "https://overpass.kumi.systems/api/interpreter"]


def overpass(filtro, tipo):
    bb = "(%.4f,%.4f,%.4f,%.4f)" % (lat0, lon0, lat1, lon1)
    q = "[out:json][timeout:300];(way%s%s;relation%s%s;);out geom;" % (filtro, bb, filtro, bb)
    for k in range(6):
        try:
            r = requests.post(SERV[k % 3], data={"data": q}, headers=UA, timeout=600)
            if r.status_code == 200:
                break
            print("  Overpass %s -> %s, reintento" % (filtro, r.status_code))
        except requests.RequestException as err:
            print("  Overpass %s -> %s, reintento" % (filtro, type(err).__name__))
        time.sleep(20 * (k + 1))
    else:
        raise SystemExit("Overpass no respondio para " + filtro)
    geoms = []
    for el in r.json()["elements"]:
        partes = []
        if el["type"] == "way" and len(el.get("geometry", [])) >= 2:
            partes = [el["geometry"]]
        elif el["type"] == "relation":
            partes = [mb["geometry"] for mb in el.get("members", [])
                      if mb.get("geometry") and len(mb["geometry"]) >= 2 and mb.get("role", "") in ("outer", "")]
        for p in partes:
            xy = [(c["lon"], c["lat"]) for c in p]
            if tipo == "lineas":
                geoms.append(LineString(xy))
            elif len(xy) >= 4:
                if xy[0] != xy[-1]:
                    xy.append(xy[0])
                geoms.append(Polygon(xy))
    if not geoms:
        return None
    out = gpd.GeoDataFrame({"filtro": [filtro] * len(geoms)}, geometry=geoms, crs="EPSG:4326")
    out = al_rectangulo(out)
    return out if len(out) else None


# Los centros de esqui de la region (Valle Nevado, La Parva, El Colorado) no
# estan dibujados como landuse=winter_sports sino como pistas (piste:type).
OSM = {"osm_esqui": ('["landuse"="winter_sports"]', "poligonos"),
       "osm_pista": ('["piste:type"]', "lineas"),
       "osm_andarivel": ('["aerialway"]', "lineas"),
       "osm_construccion": ('["landuse"="construction"]', "poligonos"),
       "osm_glaciar": ('["natural"="glacier"]', "poligonos"),
       "osm_humedal": ('["natural"="wetland"]', "poligonos"),
       "osm_embalse": ('["landuse"="reservoir"]', "poligonos"),
       "osm_represa": ('["waterway"="dam"]', "lineas")}
ya = set(gpd.list_layers(SALIDA)["name"]) if os.path.exists(SALIDA) else set()
for nombre, (filtro, tipo) in OSM.items():
    if nombre in ya:
        capas[nombre] = None
        print("  %s: en cache" % nombre)
        continue
    v = overpass(filtro, tipo)
    capas[nombre] = v
    print("  OSM %-18s %s" % (nombre, "sin entidades" if v is None else "%d entidades" % len(v)))
    time.sleep(10)

for nombre, v in capas.items():
    if v is None or len(v) == 0:
        continue
    v.to_file(SALIDA, layer=nombre, driver="GPKG")
print("Capas en", SALIDA, ":", list(gpd.list_layers(SALIDA)["name"]))
