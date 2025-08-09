
from PIL import Image

from utils.utils_ca4 import open_ca4
from utils.utils_conexao import assegura_offline
from utils.utils_paths_config import Paths



def main_old():
    print("testa imagem")
    Image.open(Paths.IMG_CA4_START).show()    

def main():
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    assegura_offline()
    print("Iniciando vídeo...")
    open_ca4()
    

if __name__ == "__main__":
    main()