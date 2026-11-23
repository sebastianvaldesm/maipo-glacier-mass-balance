# -*- coding: utf-8 -*-
"""
M05. Coregistracion por escena y por glaciar.
  Para cada escena y cada unidad de la union que la escena ve:
    - roca estable a menos de 2 km de la unidad (3, 4,5, 6,75 y hasta 10 km si faltan 3.000 px),
      sin los pixeles con |dh - mediana| > 150 m, que son nubes o correlacion fallida;
    - prueba las variantes de alineacion de xDEM y elige la de menor NMAD en la roca que no
      entro al ajuste (damero de bloques de 480 m); descarta corrimientos de mas de 150 m;
    - resta la mediana final de la roca y guarda el dh por pixel de la unidad (npz, en dm);
    - controla la roca (NMAD <= 15 m) y el hielo (pixeles imposibles, NMAD, media contra mediana).
  Es incremental: salta las escenas ya hechas.
Uso: python M05_Coregistracion_Glaciar.py <parte> <de>     (varias partes en paralelo)
Sale: M05_salida/
"""

import os
import sys
import time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import numpy as np
import pandas as pd

import comun_rm as C

parte = int(sys.argv[1]) if len(sys.argv) > 2 else 0
de = int(sys.argv[2]) if len(sys.argv) > 2 else 1

B_KM = float(os.environ.get("CADENA_RM_B_KM", "2"))
SALIDA = os.environ.get("CADENA_RM_SALIDA", "M05_salida")
METODO_FIJO = os.environ.get("CADENA_RM_METODO", "")
MIN_ROCA = 3000          # ~2,7 km2 de roca para alinear
MIN_ROCA_ABS = 1000      # por debajo de esto no se intenta
B_MAX = 10000.0
MIN_PX_GLACIAR = 10
NODATA = -32768

DIR_OUT = os.path.join(C.DIR_CADENA, SALIDA)
os.makedirs(DIR_OUT, exist_ok=True)

base = C.Base()
esc = C.lista_escenas()
U = C.inventario_union()
asig, en_union, uids = C.asignacion_union(base)
dom = asig > 0
pos = np.full(base.shape, -1, dtype=np.int64)
pos[dom] = np.arange(int(dom.sum()))
N_DOM = int(dom.sum())
MET = C.metodos_todos()
if METODO_FIJO:
    MET = {METODO_FIJO: MET[METODO_FIJO]}

idx = [i for i in range(len(esc)) if i % de == parte]
print("Escenas: %d | en esta parte: %d | buffer %.1f km | unidades %d | pixeles del dominio %d | metodos %d"
      % (len(esc), len(idx), B_KM, len(U), N_DOM, len(MET)), flush=True)

for i in idx:
    e = esc.iloc[i]
    pref = os.path.join(DIR_OUT, e["escena"])
    if os.path.exists(pref + "_unidades.csv"):
        continue
    t0 = time.time()
    r = C.leer_escena(e["archivo"], base)
    G, M, ERR = [], [], []
    mosaico = np.full(N_DOM, NODATA, dtype=np.int16)
    if r is not None:
        arr, (f0, f1, c0, c1) = r
        ref_s = base.gdem[f0:f1, c0:c1]
        est_s = base.estable[f0:f1, c0:c1]
        dh0_s = arr - ref_s
        ok_s = np.isfinite(dh0_s)
        asig_s = asig[f0:f1, c0:c1]
        presentes = np.unique(asig_s[ok_s & (asig_s > 0)])
        for k in presentes:
            u = U.iloc[int(k) - 1]
            fila = dict(escena=e["escena"], fecha=e["fecha"], anio_dec=e["anio_dec"],
                        nube_media_pct=e["nube_media_pct"], uid=u["uid"])
            bx = u.geometry.bounds
            v = base.ventana_px(bx[0] - B_MAX, bx[1] - B_MAX, bx[2] + B_MAX, bx[3] + B_MAX)
            if v is None:
                continue
            g0, g1 = max(v[0], f0), min(v[1], f1)
            h0, h1 = max(v[2], c0), min(v[3], c1)
            if g1 - g0 < 10 or h1 - h0 < 10:
                continue
            sl = (slice(g0 - f0, g1 - f0), slice(h0 - c0, h1 - c0))
            a_w = asig[g0:g1, h0:h1] == k
            mg = a_w & en_union[g0:g1, h0:h1]          # la unidad misma
            n_val = int((a_w & ok_s[sl]).sum())
            if n_val < MIN_PX_GLACIAR:
                continue
            fila.update(px_unidad=int(mg.sum()), px_con_dato=int((mg & ok_s[sl]).sum()), px_medidos=n_val)
            d = C.distancia_m(mg if mg.any() else a_w)
            est = est_s[sl] & ok_s[sl]
            B = B_KM * 1000.0
            while True:
                cerca = est & (d <= B)
                if cerca.sum() > 0:
                    med = float(np.median(dh0_s[sl][cerca]))
                    inl = cerca & (np.abs(dh0_s[sl] - med) <= C.LIM_PIXEL)
                else:
                    inl = cerca
                if inl.sum() >= MIN_ROCA or B >= B_MAX:
                    break
                B = min(B * 1.5, B_MAX)
            fila.update(buffer_km=B / 1000.0, roca_con_dato_px=int(cerca.sum()),
                        roca_utilizable_px=int(inl.sum()),
                        roca_imposible_pct=float(100 * (1 - inl.sum() / max(cerca.sum(), 1))))
            if inl.sum() < MIN_ROCA_ABS:
                fila["decision_roca"] = "sin roca suficiente"
                G.append(fila)
                continue
            vb = base.ventana_px(bx[0] - B, bx[1] - B, bx[2] + B, bx[3] + B)
            b0, b1 = max(vb[0], g0), min(vb[1], g1)
            k0, k1 = max(vb[2], h0), min(vb[3], h1)
            sb = (slice(b0 - g0, b1 - g0), slice(k0 - h0, k1 - h0))
            se = (slice(b0 - f0, b1 - f0), slice(k0 - c0, k1 - c0))
            ref = ref_s[se]; esc_a = arr[se]; inl_b = inl[sb]; med_b = a_w[sb]
            dh_antes = (esc_a - ref)[inl_b]
            fila.update(NMAD_roca_antes=C.nmad(dh_antes), mediana_roca_antes=float(np.median(dh_antes)))
            # Eleccion del metodo con roca INDEPENDIENTE: la roca se parte en
            # bloques de 480 m alternados; cada metodo se ajusta con una mitad y
            # se califica con la NMAD de la otra. Calificar con la misma roca del
            # ajuste premia al metodo con mas parametros.
            ff, cc = np.indices(inl_b.shape)
            damero = ((ff // 16) + (cc // 16)) % 2 == 0
            inl_A = inl_b & damero; inl_V = inl_b & ~damero
            cand_m = []
            for nm, cons in MET.items():
                rm = dict(escena=e["escena"], uid=u["uid"], metodo=nm)
                t1 = time.time()
                if inl_A.sum() < MIN_ROCA_ABS // 2 or inl_V.sum() < MIN_ROCA_ABS // 2:
                    rm.update(falla="roca insuficiente para validar"); M.append(rm); continue
                try:
                    a, (sx, sy, sz) = C.alinear(ref, esc_a, inl_A, base, b0, k0, cons)
                except Exception as err:
                    rm.update(falla=repr(err)[:60], segundos=round(time.time() - t1, 3)); M.append(rm); continue
                dd = a - ref
                vv = dd[inl_V & np.isfinite(dd)]
                vv = vv[np.abs(vv) <= C.LIM_PIXEL]
                va = dd[inl_A & np.isfinite(dd)]
                va = va[np.abs(va) <= C.LIM_PIXEL]
                if vv.size < MIN_ROCA_ABS // 2:
                    rm.update(falla="roca insuficiente tras alinear", segundos=round(time.time() - t1, 3))
                    M.append(rm); continue
                sh = float(np.hypot(sx, sy)) if np.isfinite(sx) else 0.0
                rm.update(NMAD=C.nmad(vv), NMAD_ajuste=C.nmad(va), mediana=float(np.median(vv)),
                          n=int(vv.size), sx=sx, sy=sy, sz=sz, desplaz_m=sh, diverge=bool(sh > C.LIM_SHIFT),
                          segundos=round(time.time() - t1, 3))
                M.append(rm)
                if not rm["diverge"]:
                    cand_m.append(rm)
            # El elegido se vuelve a ajustar con toda la roca; si ese ajuste
            # diverge se pasa al siguiente.
            mejor = None
            for rm in sorted(cand_m, key=lambda x: x["NMAD"]):
                try:
                    a, (sx, sy, sz) = C.alinear(ref, esc_a, inl_b, base, b0, k0, MET[rm["metodo"]])
                except Exception:
                    continue
                sh = float(np.hypot(sx, sy)) if np.isfinite(sx) else 0.0
                if sh > C.LIM_SHIFT:
                    continue
                dd = a - ref
                vv = dd[inl_b & np.isfinite(dd)]
                vv = vv[np.abs(vv) <= C.LIM_PIXEL]
                if vv.size < MIN_ROCA_ABS:
                    continue
                mejor = (dict(metodo=rm["metodo"], NMAD=C.nmad(vv), NMAD_validacion=rm["NMAD"],
                              mediana=float(np.median(vv)), sd=float(np.std(vv)), n=int(vv.size),
                              desplaz_m=sh, sx=sx, sy=sy, sz=sz), dd)
                break
            if mejor is None:
                fila["decision_roca"] = "ningun metodo converge"
                G.append(fila)
                continue
            rm, dd = mejor
            # Los metodos que ajustan un plano o una curva por minimos cuadrados
            # dejan en cero la MEDIA de la roca, no su mediana; como el glaciar
            # se mide con medianas, se termina restando la mediana que queda
            # sobre la roca (no cambia la NMAD ni la eleccion del metodo).
            s_ok = inl_b & np.isfinite(dd) & (np.abs(dd) <= C.LIM_PIXEL)
            med_fin = float(np.median(dd[s_ok]))
            dd = dd - med_fin
            fila.update(metodo=rm["metodo"], NMAD_roca=rm["NMAD"], NMAD_validacion=rm["NMAD_validacion"],
                        mediana_roca_metodo=rm["mediana"],
                        media_roca=float(np.mean(dd[s_ok])), sd_roca=rm["sd"],
                        RMSE_roca=float(np.sqrt(np.mean(dd[s_ok] ** 2))), correccion_mediana_final=med_fin,
                        roca_final_px=rm["n"], desplaz_m=rm["desplaz_m"], sx=rm["sx"], sy=rm["sy"],
                        sz=rm["sz"], decision_roca="alineada")
            # dh por pixel de lo que mide esta unidad, en decimetros
            w = med_b & np.isfinite(dd)
            pw = pos[b0:b1, k0:k1][w]
            mosaico[pw] = np.clip(np.round(dd[w] * 10.0), -32767, 32767).astype(np.int16)
            fila["px_guardados"] = int(w.sum())
            # error de la roca por banda de cota
            zr = ref[inl_b]; vr = dd[inl_b]
            s = np.isfinite(vr) & (np.abs(vr) <= C.LIM_PIXEL)
            br = np.floor(zr[s] / 100) * 100; vr = vr[s]
            for bnd in np.unique(br):
                q = br == bnd
                if q.sum() >= 30:
                    ERR.append(dict(escena=e["escena"], uid=u["uid"], banda=bnd, n=int(q.sum()),
                                    mediana=float(np.median(vr[q])), NMAD=C.nmad(vr[q])))
            G.append(fila)
    np.savez_compressed(pref + "_dh.npz", dh_dm=mosaico)
    pd.DataFrame(M).to_csv(pref + "_metodos.csv", index=False)
    pd.DataFrame(ERR).to_csv(pref + "_error.csv", index=False)
    pd.DataFrame(G if G else [dict(escena=e["escena"], nota="no ve ninguna unidad")]).to_csv(
        pref + "_unidades.csv", index=False)
    print("[%d] %s  unidades %d  (%.0f s)" % (i, e["escena"], len(G), time.time() - t0), flush=True)
print("Parte %d de %d terminada." % (parte, de), flush=True)
