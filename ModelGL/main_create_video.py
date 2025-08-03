import sys
import json

import utils.conexao as c
from utils.ca4 import CA4
from utils.paths import Paths

from PIL import Image



def main_old():
    print("testa imagem")
    Image.open(Paths.IMG_CA4_START).show()    

def main():
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    c.assegura_offline()
    print("Iniciando vídeo...")
    CA4.open_ca4()
    

if __name__ == "__main__":
    main()