# -*- coding: utf-8 -*-
"""
Funciones y parametros comunes de M05 y M05c: rutas, lista de escenas, lectura de cada escena
en la grilla regional de 30 m, unidades de medicion (union, RGI 6.0, RGI 7.0 y DGA) y la lista
de variantes de alineacion de xDEM. M05 y M05c lo importan; P15 lo ajusta para los modelos de
MicMac sin tocar este archivo.
"""

import os
import warnings

import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.enums import Resampling
from rasterio.features import rasterize
from rasterio.warp import reproject
from rasterio.windows import Window
from scipy import ndimage
import xdem

warnings.filterwarnings("ignore")

RAIZ = r"C:\maipo\proyecto"
DIR_BASE = os.path.join(RAIZ, "01_Datos/Mascara_Estable", "MascaraEstable_RM_ASTER")
DIR_REG = os.path.join(RAIZ, "01_Datos/Brutos", "ASTER_Regional")
F_INV = os.path.join(RAIZ, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventarios_RM_ASTER.gpkg")
# Inventario de la DGA (IPG 2014) de la cuenca alta del Maipo, capa dga (M02b). Es el
# inventario principal; cae entero dentro de la union, asi que se
# mide sobre el mismo dh por pixel sin agrandar el dominio de M05.
F_INV_DGA = os.path.join(RAIZ, "01_Datos/Procesados", "Inventarios_UTM19S", "Inventario_DGA_2014_cuenca.gpkg")
DIR_CADENA = os.path.join(RAIZ, "02_Balance_Masa_RM", "Cadena_RM")
DIR_TAB = os.path.join(DIR_CADENA, "tablas")

LIM_PIXEL = 150.0      # pixel imposible: |dh| > 150 m (seccion 6.1)
LIM_SHIFT = 150.0      # desplazamiento horizontal maximo creible
RES = 30.0


def nmad(x):
    x = np.asarray(x, float)
    x = x[np.isfinite(x)]
    if x.size == 0:
        return np.nan
    m = np.median(x)
    return float(1.4826 * np.median(np.abs(x - m)))


class Base:
    """GDEM, mascara estable y metadatos de la grilla regional."""

    def __init__(self):
        with rasterio.open(os.path.join(DIR_BASE, "01_GDEM.tif")) as r:
            self.gdem = r.read(1, masked=True).filled(np.nan).astype("float32")
            self.transform = r.transform
            self.crs = r.crs
            self.shape = r.shape
        with rasterio.open(os.path.join(DIR_BASE, "03_MascaraEstable.tif")) as r:
            m = r.read(1, masked=True)
            self.estable = (m.filled(0) == 1)
        self.x0 = self.transform.c
        self.y0 = self.transform.f

    def ventana_px(self, xmin, ymin, xmax, ymax):
        """Ventana de la grilla (fila, columna) que cubre un rectangulo, recortada a la grilla."""
        c0 = int(np.floor((xmin - self.x0) / RES))
        c1 = int(np.ceil((xmax - self.x0) / RES))
        f0 = int(np.floor((self.y0 - ymax) / RES))
        f1 = int(np.ceil((self.y0 - ymin) / RES))
        c0, f0 = max(c0, 0), max(f0, 0)
        c1, f1 = min(c1, self.shape[1]), min(f1, self.shape[0])
        if c1 <= c0 or f1 <= f0:
            return None
        return f0, f1, c0, c1

    def transform_de(self, f0, c0):
        return rasterio.Affine(RES, 0, self.x0 + c0 * RES, 0, -RES, self.y0 - f0 * RES)


def leer_escena(ruta, base):
    """Lee una escena AST14DEM y la lleva a la grilla regional, solo en su
    ventana. Devuelve (arreglo, (f0, f1, c0, c1)) o None si no toca la grilla."""
    with rasterio.open(ruta) as src:
        b = src.bounds
        v = base.ventana_px(b.left, b.bottom, b.right, b.top)
        if v is None:
            return None
        f0, f1, c0, c1 = v
        dst = np.full((f1 - f0, c1 - c0), np.nan, dtype="float32")
        src_arr = src.read(1).astype("float32")
        src_arr[(src_arr <= -9998) | (src_arr == 0)] = np.nan
        reproject(source=src_arr, destination=dst, src_transform=src.transform, src_crs=src.crs,
                  src_nodata=np.nan, dst_transform=base.transform_de(f0, c0), dst_crs=base.crs,
                  dst_nodata=np.nan, resampling=Resampling.bilinear)
    if not np.isfinite(dst).any():
        return None
    return dst, v


def lista_escenas():
    """Escenas de verano (enero a abril) del inventario regional, sin corte de
    nubosidad. Una fila por granulo, con fecha, hora y nubosidad declarada."""
    inv = pd.read_csv(os.path.join(DIR_REG, "Inventario_Regional.csv"))
    inv = inv[inv["estado"] == "ok"][["granulo", "archivo"]]
    # el inventario guarda la ruta completa de cada escena tal como estaba al escribirlo, antes de que
    # 01_Datos_Brutos/ pasara a 01_Datos/Brutos/ y de que la carpeta del proyecto cambiara de nombre. Su carpeta es de solo lectura, asi que la ruta se
    # rearma aqui desde RAIZ con lo que va despues de la carpeta de datos brutos.
    brutos = os.path.join(RAIZ, "01_Datos", "Brutos").replace("\\", "/") + "/"
    inv["archivo"] = inv["archivo"].str.replace(r"^.*?/01_Datos(?:_Brutos|/Brutos)/", lambda m: brutos, regex=True)
    cat = pd.read_csv(os.path.join(DIR_REG, "Catalogo_Regional_AST14DEM.csv"))
    inv = inv.merge(cat[["granulo", "fecha", "hora_utc", "mes", "dia_del_anio", "nube_media_pct",
                         "nube_min_cuadrante_pct"]], on="granulo")
    inv = inv[inv["mes"].isin([1, 2, 3, 4]) & inv["archivo"].map(os.path.exists)]
    inv["escena"] = inv["fecha"] + "_" + inv["hora_utc"].str.replace(":", "")
    inv["anio_dec"] = pd.to_datetime(inv["fecha"]).map(lambda d: d.year + (d.dayofyear - 0.5) / 365.25)
    return inv.sort_values("escena").reset_index(drop=True)


ID_COL = {"rgi70": "rgi_id", "rgi60": "RGIId", "solo_rgi60": "gid", "solo_rgi70": "gid"}


def inventario(nombre):
    """Capa de Inventarios_RM_ASTER.gpkg con el identificador en `gid`."""
    g = gpd.read_file(F_INV, layer=nombre)
    g = g.rename(columns={ID_COL[nombre]: "gid"})
    return g.reset_index(drop=True)


def dem(arr, base, f0, c0):
    a = np.where(np.isfinite(arr), arr, -9999).astype("float32")
    return xdem.DEM.from_array(a, base.transform_de(f0, c0), base.crs, nodata=-9999)


def metodos():
    """Los cinco metodos del protocolo de comparacion (VerticalShift,
    NuthKaab, TerrainBias y dos combinaciones) mas los dos con rampa."""
    return {
        "VerticalShift": lambda: xdem.coreg.VerticalShift(),
        "NuthKaab": lambda: xdem.coreg.NuthKaab(),
        "TerrainBias": lambda: xdem.coreg.TerrainBias(),
        "VerticalShift+NuthKaab": lambda: xdem.coreg.VerticalShift() + xdem.coreg.NuthKaab(),
        "NuthKaab+TerrainBias": lambda: xdem.coreg.NuthKaab() + xdem.coreg.TerrainBias(),
        "Deramp": lambda: xdem.coreg.Deramp(poly_order=1),
        "NuthKaab+Deramp": lambda: xdem.coreg.NuthKaab() + xdem.coreg.Deramp(poly_order=1),
    }


def metodos_todos():
    """Todos los metodos de coregistracion de xDEM 0.2.3: cada clase sola, mas las combinaciones del protocolo de comparacion y las
    que agregan a Nuth y Kaab una correccion de sesgo:
      afines:   VerticalShift, NuthKaab, DhMinimize, LZD, ICP
      de sesgo: TerrainBias (curvatura maxima), Deramp (plano),
                DirectionalBias (a lo largo de la traza de Terra)
      cadenas:  VerticalShift+NuthKaab, NuthKaab+TerrainBias, NuthKaab+Deramp,
                NuthKaab+DirectionalBias, ICP+NuthKaab
    El angulo de DirectionalBias se mide desde el eje X (este) y crece en
    sentido horario: la coordenada que corrige es x cos(a) - y sin(a)
    (geoutils.raster.get_xy_rotated). La traza de las escenas va a 12,5 grados
    al este del norte (lado largo del rectangulo de datos, medido en cuatro
    escenas de 2019), es decir a = -77,5. Con -8 se corrige casi el eje este,
    a lo ancho de la traza y no a lo largo.
    CPD no esta en esta lista: P15 lo agrega en una segunda pasada porque
    compara cada uno de los 5.000 puntos de una nube con todos los de la otra
    (matrices de 5.000 x 5.000 x 3 en cada iteracion, cientos de MB), tarda
    8 s por ajuste en mediana y hasta tres minutos y, con varios procesos a la
    vez, agota la memoria del equipo (Windows mata los procesos sin aviso)."""
    ang = -77.5
    m = dict(metodos())
    m.update({
        "DhMinimize": lambda: xdem.coreg.DhMinimize(),
        "LZD": lambda: xdem.coreg.LZD(),
        "ICP": lambda: xdem.coreg.ICP(),
        "DirectionalBias": lambda: xdem.coreg.DirectionalBias(angle=ang, fit_or_bin="bin"),
        "NuthKaab+DirectionalBias": lambda: xdem.coreg.NuthKaab() + xdem.coreg.DirectionalBias(angle=ang, fit_or_bin="bin"),
        "ICP+NuthKaab": lambda: xdem.coreg.ICP() + xdem.coreg.NuthKaab(),
    })
    return m


def asignacion_union(base, dist_max=2000.0):
    """Raster de la grilla regional con el indice (1..n) de la unidad de la
    union que mide cada pixel, y la lista de unidades.
      - Pixeles de la union (centro dentro del contorno de una unidad): su
        unidad.
      - Pixeles de un poligono del RGI 6.0 del dominio que la union no cubre
        (hielo que el RGI 7.0 asigna a un glaciar vecino fuera de la RM): la
        unidad mas cercana, si esta a menos de dist_max; sirven solo para
        medir esos poligonos completos al comparar con Dussaillant.
    Devuelve (asig, en_union, uids): asig int32 con 0 fuera; en_union bool."""
    u = inventario_union()
    shp = [(g, k + 1) for k, g in enumerate(u.geometry.values)]
    asig = rasterize(shp, out_shape=base.shape, transform=base.transform, fill=0, dtype="int32",
                     all_touched=False)
    en_union = asig > 0
    r6 = gpd.read_file(F_INV, layer="rgi60")
    p6 = rasterize([(g, 1) for g in r6.geometry.values], out_shape=base.shape, transform=base.transform,
                   fill=0, dtype="uint8", all_touched=False).astype(bool)
    falta = p6 & ~en_union
    if falta.any():
        d, (ii, jj) = ndimage.distance_transform_edt(~en_union, return_indices=True)
        cerca = falta & (d * RES <= dist_max)
        asig[cerca] = asig[ii[cerca], jj[cerca]]
    return asig, en_union, list(u["uid"])


def inventario_union():
    """Capa union de M02 (unidades de alineacion y medicion)."""
    return gpd.read_file(F_INV, layer="union").reset_index(drop=True)


def desplazamiento(cg):
    """Suma los desplazamientos de los pasos afines de un metodo (o pipeline)."""
    pasos = cg.pipeline if hasattr(cg, "pipeline") else [cg]
    sx = sy = sz = 0.0
    hubo = False
    for p in pasos:
        try:
            M = p.to_matrix()
            sx += float(M[0, 3]); sy += float(M[1, 3]); sz += float(M[2, 3])
            hubo = True
        except Exception:
            pass
    return (sx, sy, sz) if hubo else (np.nan, np.nan, np.nan)


def alinear(ref_arr, esc_arr, inlier, base, f0, c0, constructor, seed=42):
    """Ajusta un metodo sobre los pixeles `inlier` y devuelve el arreglo
    alineado y los desplazamientos. Lanza excepcion si el ajuste falla."""
    ref = dem(ref_arr, base, f0, c0)
    tba = dem(esc_arr, base, f0, c0)
    cg = constructor()
    cg.fit(reference_elev=ref, to_be_aligned_elev=tba, inlier_mask=inlier, random_state=seed)
    out = cg.apply(tba)
    a = np.asarray(out.data, dtype="float32").copy()
    a[np.ma.getmaskarray(out.data)] = np.nan
    return a, desplazamiento(cg)


def mascara_poligono(geom, base, f0, f1, c0, c1):
    return rasterize([(geom, 1)], out_shape=(f1 - f0, c1 - c0), transform=base.transform_de(f0, c0),
                     fill=0, dtype="uint8", all_touched=False).astype(bool)


def distancia_m(mask_bool):
    """Distancia (m) de cada pixel al pixel True mas cercano."""
    if not mask_bool.any():
        return np.full(mask_bool.shape, np.inf, dtype="float32")
    return (ndimage.distance_transform_edt(~mask_bool) * RES).astype("float32")
