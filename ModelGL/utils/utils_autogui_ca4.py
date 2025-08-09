import subprocess

import utils.utils_autogui as au
from utils.utils_paths_config import Paths

def open_ca4():
    _caminho_ca4: str = "C:\\Program Files\\Reallusion\\Cartoon Animator 4\\bin64\\CartoonAnimator.exe"
    print("Vai abrir o CA4 do caminho:")
    print(_caminho_ca4)
    subprocess.Popen([_caminho_ca4])
    
    print("Aguardando CA4 carregar...")
    
    au.wait_for_img(Paths.IMG_CA4_START)
    au.click_img_s(Paths.IMG_CA4_BTN_THANK_YOU)