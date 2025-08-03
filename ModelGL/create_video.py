import sys
import json

import utils.conexao as c
from utils.ca4 import CA4

def main():
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    c.assegura_offline()
    print("Iniciando vídeo...")
    CA4.open_ca4()
    

if __name__ == "__main__":
    main()
