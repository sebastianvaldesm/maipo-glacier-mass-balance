# -*- coding: utf-8 -*-
"""
M05b. CPD (coherent point drift) de xDEM sobre una muestra de pares, para ver si gana a las otras
variantes y cuanto tarda. P15 usa esta misma configuracion (xdem.coreg.CPD() por omision).
Sale: M05b_salida/
"""

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import numpy as np
import pandas as pd
import xdem

import comun_rm as C

parte = int(sys.argv[1]) if len(sys.argv) > 2 else 0
de = int(sys.argv[2]) if len(sys.argv) > 2 else 1
B = float(os.environ.get("CADENA_RM_B_KM", "2")) * 1000.0
DIR_OUT = os.path.join(C.DIR_CADENA, "M05b_salida")
os.makedirs(DIR_OUT, exist_ok=True)

base = C.Base()
esc = C.lista_escenas()
U = C.inventario_union()
asig, en_union, uids = C.asignacion_union(base)
gl = U[(U["area_utm_km2"] > 0.5) & (U["area_utm_km2"] <= 5.0)]
MET = dict(C.metodos_todos())
MET["CPD"] = lambda: xdem.coreg.CPD()

muestra = list(range(0, len(esc), 40))
idx = [m for j, m in enumerate(muestra) if j % de == parte]
for i in idx:
    e = esc.iloc[i]
    sal = os.path.join(DIR_OUT, e["escena"] + ".csv")
    if os.path.exists(sal):
        continue
    filas = []
    r = C.leer_escena(e["archivo"], base)
    if r is None:
        pd.DataFrame([dict(escena=e["escena"], nota="fuera de la grilla")]).to_csv(sal, index=False)
        continue
    arr, (f0, f1, c0, c1) = r
    x0, y0 = base.transform_de(f0, c0).c, base.transform_de(f0, c0).f
    cand = gl.cx[x0:x0 + (c1 - c0) * C.RES, y0 - (f1 - f0) * C.RES:y0]
    t0 = time.time()
    for _, g in cand.iterrows():
        k = uids.index(g["uid"]) + 1
        bx = g.geometry.bounds
        v = base.ventana_px(bx[0] - B, bx[1] - B, bx[2] + B, bx[3] + B)
        g0, g1, h0, h1 = max(v[0], f0), min(v[1], f1), max(v[2], c0), min(v[3], c1)
        if g1 - g0 < 10 or h1 - h0 < 10:
            continue
        se = (slice(g0 - f0, g1 - f0), slice(h0 - c0, h1 - c0))
        ref = base.gdem[g0:g1, h0:h1]; a0 = arr[se]
        mg = (asig[g0:g1, h0:h1] == k) & en_union[g0:g1, h0:h1]
        if (mg & np.isfinite(a0)).sum() < 0.5 * mg.sum():
            continue
        d = C.distancia_m(mg)
        dh0 = a0 - ref
        cerca = base.estable[g0:g1, h0:h1] & np.isfinite(dh0) & (d <= B)
        if cerca.sum() < 3000:
            continue
        inl = cerca & (np.abs(dh0 - np.median(dh0[cerca])) <= C.LIM_PIXEL)
        # misma calificacion que M05: ajuste con media roca, NMAD en la otra media
        ff, cc = np.indices(inl.shape)
        damero = ((ff // 16) + (cc // 16)) % 2 == 0
        inl_A = inl & damero; inl_V = inl & ~damero
        for nm, cons in MET.items():
            t = time.time()
            try:
                a, (sx, sy, sz) = C.alinear(ref, a0, inl_A, base, g0, h0, cons)
                dd = a - ref
                vv = dd[inl_V & np.isfinite(dd)]
                vv = vv[np.abs(vv) <= C.LIM_PIXEL]
                sh = float(np.hypot(sx, sy)) if np.isfinite(sx) else 0.0
                filas.append(dict(escena=e["escena"], uid=g["uid"], area_km2=g["area_utm_km2"],
                                  roca_px=int(inl.sum()), metodo=nm, NMAD=C.nmad(vv),
                                  mediana=float(np.median(vv)), desplaz_m=sh, diverge=sh > C.LIM_SHIFT,
                                  segundos=time.time() - t))
            except Exception as err:
                filas.append(dict(escena=e["escena"], uid=g["uid"], area_km2=g["area_utm_km2"],
                                  roca_px=int(inl.sum()), metodo=nm, falla=repr(err)[:80],
                                  segundos=time.time() - t))
    pd.DataFrame(filas if filas else [dict(escena=e["escena"], nota="sin unidades")]).to_csv(sal, index=False)
    print("[%d] %s  %d unidades  %.0f s" % (i, e["escena"], len(cand), time.time() - t0), flush=True)
print("Parte %d de %d terminada." % (parte, de), flush=True)
