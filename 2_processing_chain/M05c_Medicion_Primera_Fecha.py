# -*- coding: utf-8 -*-
"""
M05c. Cambio de elevacion contra la primera fecha.
  Para cada glaciar, la superficie de referencia es la mediana pixel a pixel de las escenas
  aceptadas de su primer anio (2001 en casi todos). Cada escena posterior se mide contra ella en
  bandas de 100 m, ponderadas por el area de cada banda en el glaciar completo; las bandas sin dato
  toman una recta dh-altura si hay al menos tres medidas. Una escena entra solo si, junto con la
  referencia, ve el 90 % de los pixeles del glaciar. Regla de temporada: por glaciar y anio se usan
  las escenas de marzo-abril, si no las de febrero y si no las de enero.
Sale: tablas/M05c_*.csv (serie de cada glaciar y control)
"""

import glob
import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import geopandas as gpd
import numpy as np
import pandas as pd
from rasterio.features import rasterize

import comun_rm as C

SALIDA = os.environ.get("CADENA_RM_SALIDA", "M05_salida")
DIR_IN = os.path.join(C.DIR_CADENA, SALIDA)
SUF = os.environ.get("CADENA_RM_SUFIJO", "")
NODATA = -32768
MIN_PX = 10
MIN_PX_BANDA = 10
MIN_BANDAS_RELLENO = 3
LIM_NMAD_ROCA = 15.0
LIM_IMP = 10.0
LIM_NMAD_HIELO = 25.0
LIM_BRECHA = 4.0
LIM_COB = 50.0
# Un par que ve solo una parte del glaciar (por ejemplo la lengua, con la cabecera
# bajo nube) sobrestima la perdida. Medido antes de fijar el umbral: el 95 % de los pares aceptados ya veia el 94 % o mas de sus
# pixeles, y exigir el 90 % deja 12.934 de 13.484 pares sin quitarle la recta a
# ninguna unidad. Se exige el 90 % de los pixeles del poligono con dato valido
# (sin los imposibles), en cada par y, en la serie, en la escena y la
# referencia a la vez.
LIM_PX_VALIDOS = float(os.environ.get("CADENA_RM_PX_VALIDOS", "90"))

t00 = time.time()
base = C.Base()
U = C.inventario_union()
asig, en_union, uids = C.asignacion_union(base)
dom = asig > 0
N_DOM = int(dom.sum())
pos = np.full(base.shape, -1, dtype=np.int64)
pos[dom] = np.arange(N_DOM)
z_dom = base.gdem[dom]
uni_dom = asig[dom]

# --- 1. Escenas: tabla por unidad y vector de dh ------------------------------------
fs = sorted(glob.glob(os.path.join(DIR_IN, "*_unidades.csv")))
esc_ids = [os.path.basename(f)[:-len("_unidades.csv")] for f in fs]
R = pd.concat([pd.read_csv(f) for f in fs], ignore_index=True)
R = R[R["uid"].notna()].copy() if "uid" in R.columns else R.iloc[0:0]
R["k"] = R["uid"].map({u: k + 1 for k, u in enumerate(uids)})
R["roca_ok"] = (R["decision_roca"] == "alineada") & (R["NMAD_roca"] <= LIM_NMAD_ROCA)
print("Escenas: %d | filas por unidad: %d | alineadas %d | roca ok %d"
      % (len(esc_ids), len(R), (R["decision_roca"] == "alineada").sum(), R["roca_ok"].sum()), flush=True)

V = np.full((len(esc_ids), N_DOM), NODATA, dtype=np.int16)
for s, ide in enumerate(esc_ids):
    v = np.load(os.path.join(DIR_IN, ide + "_dh.npz"))["dh_dm"]
    if v.size != N_DOM:
        raise SystemExit("El vector de %s no tiene el largo del dominio (%d != %d)" % (ide, v.size, N_DOM))
    # pixeles de unidades con roca que no paso el control -> sin dato
    malas = R.loc[(R["escena"] == ide) & ~R["roca_ok"], "k"].dropna().astype(int).values
    if malas.size:
        v[np.isin(uni_dom, malas)] = NODATA
    V[s] = v
print("Vector de dh cargado: %d escenas x %d pixeles (%.0f s)" % (V.shape[0], V.shape[1], time.time() - t00),
      flush=True)
info = C.lista_escenas().set_index("escena")
anio_dec = np.array([info.loc[e, "anio_dec"] for e in esc_ids])
anio = np.array([int(e[:4]) for e in esc_ids])
mes = np.array([int(e[5:7]) for e in esc_ids])
# Prioridad de meses dentro de cada anio (menor = mejor). "jerarquia": marzo-abril,
# febrero, enero; "marzo": marzo, abril, febrero, enero
# (variante por la nieve de abril); "todas": sin prioridad.
TEMPORADA = os.environ.get("CADENA_RM_TEMPORADA", "jerarquia")
# Variantes de un solo periodo (solo enero, solo febrero o solo marzo-abril): los meses que no figuran en la
# clase quedan fuera de la serie.
CLASE = {"jerarquia": {3: 1, 4: 1, 2: 2, 1: 3}, "marzo": {3: 1, 4: 2, 2: 3, 1: 4},
         "todas": {1: 1, 2: 1, 3: 1, 4: 1}, "enero": {1: 1}, "febrero": {2: 1},
         "marzoabril": {3: 1, 4: 1}}[TEMPORADA]
NOMBRE_MES = {1: "enero", 2: "febrero", 3: "marzo", 4: "abril"}
print("Temporada:", TEMPORADA, flush=True)
fila_roca = {(r.escena, r.k): r for r in R.itertuples()}

# --- 2. Conjuntos de poligonos ----------------------------------------------------------
CAPAS = [("union", "uid"), ("rgi70", "rgi_id"), ("rgi60", "RGIId")]
# Inventario de la DGA (IPG 2014, M02b), el principal
if os.path.exists(C.F_INV_DGA):
    CAPAS.append(("dga", "gid"))
H = {}
for capa, idc in CAPAS:
    f = os.path.join(C.DIR_BASE, "01_Hipsometria_%s.csv" % capa)
    if not os.path.exists(f):
        raise SystemExit("Falta la hipsometria %s (correr M03b con esa base)" % f)
    h = pd.read_csv(f, dtype={idc: str})
    H[capa] = {k: d.set_index("banda")["area_m2"] for k, d in h.groupby(idc)}


def pixeles(geoms):
    """Posiciones en el vector del dominio de los pixeles cuyo centro cae en
    cada poligono (la misma regla con que M05 rasteriza)."""
    out = []
    for g in geoms:
        x0, y0, x1, y1 = g.bounds
        v = base.ventana_px(x0, y0, x1, y1)
        if v is None:
            out.append(np.array([], dtype=np.int64)); continue
        f0, f1, c0, c1 = v
        m = rasterize([(g, 1)], out_shape=(f1 - f0, c1 - c0), transform=base.transform_de(f0, c0), fill=0,
                      dtype="uint8", all_touched=False).astype(bool)
        p = pos[f0:f1, c0:c1][m]
        out.append(p[p >= 0])
    return out


r7 = gpd.read_file(C.F_INV, layer="rgi70")
r6 = gpd.read_file(C.F_INV, layer="rgi60")
CONJ = {"union": (list(U["uid"]), None),
        "rgi70": (list(r7["rgi_id"]), pixeles(r7.geometry.values)),
        "rgi60": (list(r6["RGIId"]), pixeles(r6.geometry.values))}
pos_union = [np.flatnonzero((uni_dom == k + 1) & en_union[dom]) for k in range(len(uids))]
CONJ["union"] = (CONJ["union"][0], pos_union)
if ("dga", "gid") in CAPAS:
    rd = gpd.read_file(C.F_INV_DGA, layer="dga")
    CONJ["dga"] = (list(rd["gid"].astype(str)), pixeles(rd.geometry.values))


def perfil(vals, z, area_banda):
    """Mediana y media por banda de 100 m reponderadas al area de la banda;
    bandas sin dato por la recta dh-cota si hay al menos tres observadas.
    Devuelve (dh_mediana, dh_media, cobertura %, tabla por banda)."""
    banda = np.floor(z / 100.0) * 100.0
    bs = area_banda.index.values.astype(float)
    med = np.full(bs.size, np.nan); me = np.full(bs.size, np.nan); n = np.zeros(bs.size, int)
    for j, b in enumerate(bs):
        x = vals[banda == b]
        if x.size >= MIN_PX_BANDA:
            med[j] = np.median(x); me[j] = np.mean(x); n[j] = x.size
    obs = np.isfinite(med)
    a = area_banda.values.astype(float)
    cob = float(100 * a[obs].sum() / a.sum()) if a.sum() > 0 else 0.0
    if not obs.any():
        return np.nan, np.nan, cob, None
    tab = pd.DataFrame(dict(banda=bs[obs], n=n[obs], mediana=med[obs], media=me[obs]))
    if obs.sum() >= MIN_BANDAS_RELLENO and (~obs).any():
        for s_ in (med, me):
            p = np.polyfit(bs[obs], s_[obs], 1)
            s_[~obs] = np.polyval(p, bs[~obs])
    u = np.isfinite(med)
    return (float((med[u] * a[u]).sum() / a[u].sum()), float((me[u] * a[u]).sum() / a[u].sum()), cob, tab)


PARES, SERIE, BANDAS, HIELO, MESES = [], [], [], [], []
for conj, (ids, posiciones) in CONJ.items():
    t1 = time.time()
    for gid, pp in zip(ids, posiciones):
        if pp.size < MIN_PX or gid not in H[conj]:
            continue
        hip = H[conj][gid]
        z = z_dom[pp]
        un = uni_dom[pp]
        X = V[:, pp]
        ks, nks = np.unique(un, return_counts=True)
        peso_k = dict(zip(ks, nks))
        # escenas en que alguna unidad del poligono tiene fila (la escena lo vio)
        cand = []
        for s, ide in enumerate(esc_ids):
            filas = [fila_roca.get((ide, k)) for k in ks]
            filas = [f for f in filas if f is not None]
            if filas:
                cand.append((s, filas))
        acept = []
        for s, filas in cand:
            # unidad dominante: la que mide mas pixeles del poligono
            dom_f = max(filas, key=lambda f: peso_k.get(f.k, 0))
            fila = dict(conjunto=conj, gid=gid, escena=esc_ids[s], anio_dec=anio_dec[s],
                        uid_alinea=dom_f.uid, metodo=getattr(dom_f, "metodo", None),
                        NMAD_roca=getattr(dom_f, "NMAD_roca", np.nan), NMAD_roca_antes=getattr(dom_f, "NMAD_roca_antes", np.nan),
                        desplaz_m=getattr(dom_f, "desplaz_m", np.nan), buffer_km=getattr(dom_f, "buffer_km", np.nan),
                        nube_media_pct=getattr(dom_f, "nube_media_pct", np.nan), px=int(pp.size))
            x = X[s].astype(np.float32)
            ok = X[s] != NODATA
            x = x / 10.0
            fila["px_con_dato"] = int(ok.sum())
            if not any(f.roca_ok for f in filas):
                dec = dom_f.decision_roca
                fila["motivo"] = ("roca: sin terreno estable suficiente" if dec == "sin roca suficiente" else
                                  "roca: ningun metodo converge" if dec == "ningun metodo converge" else
                                  "roca: NMAD > 15 m")
                PARES.append(fila); continue
            if ok.sum() < MIN_PX:
                fila["motivo"] = "hielo: sin pixeles validos"
                PARES.append(fila); continue
            v = x[ok]; zz = z[ok]
            imp = np.abs(v) > C.LIM_PIXEL
            fila["imposibles_pct"] = float(100 * imp.mean())
            if conj == "union":
                bz = np.floor(zz / 100) * 100
                for b in np.unique(bz):
                    q = bz == b
                    HIELO.append(dict(gid=gid, escena=esc_ids[s], banda=b, n=int(q.sum()), n_imp=int(imp[q].sum())))
            vb, zb = v[~imp], zz[~imp]
            if vb.size < MIN_PX:
                fila["motivo"] = "hielo: pixeles imposibles > 10 %"
                PARES.append(fila); continue
            dmed, dme, cob, _ = perfil(vb, zb, hip)
            fila.update(dh_gdem_m=dmed, brecha_m=float(dme - np.median(vb)) if np.isfinite(dme) else np.nan,
                        cobertura_hipso_pct=cob, NMAD_hielo_m=C.nmad(vb), n_px_hielo=int(vb.size),
                        px_validos_pct=float(100 * vb.size / pp.size))
            if fila["imposibles_pct"] > LIM_IMP:
                fila["motivo"] = "hielo: pixeles imposibles > 10 %"
            elif fila["px_validos_pct"] < LIM_PX_VALIDOS:
                fila["motivo"] = "hielo: ve menos del 90 % del glaciar"
            elif fila["NMAD_hielo_m"] > LIM_NMAD_HIELO:
                fila["motivo"] = "hielo: NMAD > 25 m"
            elif not abs(fila["brecha_m"]) <= LIM_BRECHA:
                fila["motivo"] = "hielo: brecha media-mediana > 4 m"
            elif not cob >= LIM_COB:
                fila["motivo"] = "hielo: cobertura hipsometrica < 50 %"
            else:
                fila["motivo"] = "aceptado"
                acept.append(s)
            PARES.append(fila)
        if not acept:
            continue
        # --- temporada: en cada anio, solo las escenas del mejor mes disponible -------
        # Las escenas de comienzo del verano traen nieve estacional; lo ideal es marzo-abril, si no febrero y como maximo
        # enero. Medido antes de aplicarlo: las de enero quedan +0,8 m sobre la
        # recta de su glaciar (mediana) y las de abril +0,2 a +0,4 m.
        apartadas = []
        excluidas = [s for s in acept if mes[s] not in CLASE]
        acept = [s for s in acept if mes[s] in CLASE]
        for s in excluidas:
            SERIE.append(dict(conjunto=conj, gid=gid, escena=esc_ids[s], anio_dec=anio_dec[s], es_referencia=False,
                              motivo_serie="variante: mes fuera del periodo"))
        if not acept:
            continue
        # registro de que meses habia y cual se uso, por poligono y anio
        disp = {}
        for s in acept:
            disp.setdefault(anio[s], set()).add(mes[s])
        if TEMPORADA != "todas":
            mejor = {}
            for s in acept:
                mejor[anio[s]] = min(mejor.get(anio[s], 9), CLASE[mes[s]])
            apartadas = [s for s in acept if CLASE[mes[s]] > mejor[anio[s]]]
            acept = [s for s in acept if CLASE[mes[s]] == mejor[anio[s]]]
        usados = {}
        for s in acept:
            usados.setdefault(anio[s], set()).add(mes[s])
        for a in sorted(disp):
            MESES.append(dict(conjunto=conj, gid=gid, anio=int(a),
                              meses_aceptados="-".join(NOMBRE_MES[m] for m in sorted(disp[a])),
                              meses_usados="-".join(NOMBRE_MES[m] for m in sorted(usados.get(a, ()))),
                              escenas_usadas=sum(1 for s in acept if anio[s] == a),
                              escenas_apartadas=sum(1 for s in apartadas if anio[s] == a)))
        for s in apartadas:
            SERIE.append(dict(conjunto=conj, gid=gid, escena=esc_ids[s], anio_dec=anio_dec[s], es_referencia=False,
                              motivo_serie="temporada: ese anio hay escenas de un mes mejor"))
        # --- referencia: primer anio con escenas aceptadas ---------------------------
        a0 = min(anio[s] for s in acept)
        ref_s = [s for s in acept if anio[s] == a0]
        Xr = X[ref_s].astype(np.float32) / 10.0
        Xr[X[ref_s] == NODATA] = np.nan
        Xr[np.abs(Xr) > C.LIM_PIXEL] = np.nan
        Rf = np.nanmedian(Xr, axis=0) if len(ref_s) > 1 else Xr[0]
        t_ref = float(np.mean(anio_dec[ref_s]))
        okr = np.isfinite(Rf)
        _, _, cob_ref, _ = perfil(Rf[okr], z[okr], hip) if okr.sum() >= MIN_PX else (0, 0, 0.0, None)
        for s in acept:
            fila = dict(conjunto=conj, gid=gid, escena=esc_ids[s], anio_dec=anio_dec[s], anio_ref=a0,
                        t_ref=t_ref, n_esc_ref=len(ref_s), cob_ref_pct=cob_ref, es_referencia=bool(s in ref_s))
            if s in ref_s:
                SERIE.append(fila); continue
            x = X[s].astype(np.float32) / 10.0
            x[X[s] == NODATA] = np.nan
            x[np.abs(x) > C.LIM_PIXEL] = np.nan
            d = x - Rf
            okd = np.isfinite(d) & (np.abs(d) <= C.LIM_PIXEL)
            if okd.sum() < MIN_PX:
                fila.update(motivo_serie="sin pixeles comunes con la referencia"); SERIE.append(fila); continue
            dmed, dme, cob, tab = perfil(d[okd], z[okd], hip)
            pxc = float(100 * okd.sum() / pp.size)
            fila.update(dh_m=dmed, dh_media_m=dme, cob_rel_pct=cob, n_px=int(okd.sum()), px_comunes_pct=pxc,
                        motivo_serie="ok" if (cob >= LIM_COB and pxc >= LIM_PX_VALIDOS)
                        else "ve menos del 90 % del glaciar junto con la referencia")
            SERIE.append(fila)
            if conj == "union" and tab is not None:
                tab.insert(0, "escena", esc_ids[s]); tab.insert(0, "gid", gid)
                BANDAS.append(tab)
    print("  %s: %d poligonos (%.0f s)" % (conj, len(ids), time.time() - t1), flush=True)

P = pd.DataFrame(PARES)
S = pd.DataFrame(SERIE)
P.to_csv(os.path.join(C.DIR_TAB, "M05c_Pares%s.csv" % SUF), index=False)
S.to_csv(os.path.join(C.DIR_TAB, "M05c_Serie%s.csv" % SUF), index=False)
if BANDAS:
    pd.concat(BANDAS, ignore_index=True).to_csv(os.path.join(C.DIR_TAB, "M05c_Bandas_Union%s.csv" % SUF), index=False)
pd.DataFrame(HIELO).to_csv(os.path.join(C.DIR_TAB, "M05c_Hielo_Cota%s.csv" % SUF), index=False)
pd.DataFrame(MESES).to_csv(os.path.join(C.DIR_TAB, "M05c_Meses_por_Glaciar%s.csv" % SUF), index=False)
print(P.groupby(["conjunto", "motivo"]).size().to_string())
if len(S):
    print(S.groupby("conjunto")["anio_ref"].value_counts().unstack(fill_value=0).to_string())
    if "motivo_serie" in S.columns:
        print(S.groupby(["conjunto", "motivo_serie"]).size().to_string())
print("Listo (%.0f s)" % (time.time() - t00))
