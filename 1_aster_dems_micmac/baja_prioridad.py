# Corre un script de Python con prioridad baja de Windows (BELOW_NORMAL), para que use la CPU que deja libre MicMac.
# Baja su propia prioridad y lanza el script como proceso hijo, que la hereda (y sus procesos de multiprocessing
# tambien). En Git Bash `nice` se cuelga con los .exe de Windows, y correr el script con runpy aqui mismo cuelga
# a multiprocessing.
# Uso: python baja_prioridad.py script.py [argumentos...]
import ctypes
import subprocess
import sys

k = ctypes.windll.kernel32
k.GetCurrentProcess.restype = ctypes.c_void_p            # HANDLE de 64 bits; sin esto ctypes lo trunca
k.SetPriorityClass.argtypes = [ctypes.c_void_p, ctypes.c_uint32]
if not k.SetPriorityClass(k.GetCurrentProcess(), 0x4000):
    sys.exit("no se pudo bajar la prioridad")
sys.exit(subprocess.call([sys.executable] + sys.argv[1:]))
