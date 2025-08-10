
from PIL import Image
import pyautogui

from utils.utils_autogui import focar_janela_ca4
from utils.utils_print import print_alt
from utils.utils_autogui_ca4 import open_ca4
from utils.utils_conexao import assegura_offline
from utils.utils_paths_config import Paths



def run_sequence():
    print_alt("Rodando a sequência completa...")
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    assegura_offline()
    print_alt("Iniciando vídeo...")
    open_ca4()

def step():
    pyautogui.sleep(2)  # Sleep for a while before checking again
    focar_janela_ca4()
    # au.click_img_s(Paths.IMG_CA4_BTN_THANK_YOU)

def main():
    # run_sequence()
    step()
    # pass 



if __name__ == "__main__":
    main()