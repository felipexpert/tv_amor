
from PIL import Image
import pyautogui

from utils.utils_load_ani_auto_task_json import load_ani_auto_task
from utils.classes.ani_auto_task import AniAutoTask
from utils.utils_autogui import click_img_s, focar_janela_ca4
from utils.utils_print import print_alt
from utils.utils_autogui_ca4 import open_ca4
from utils.utils_conexao import assegura_offline
from utils.utils_paths_config import Paths

def start_ca4():
    assegura_offline()
    print_alt("Iniciando vídeo...")
    open_ca4()

def run_sequence(aat: AniAutoTask):
    print_alt("Rodando a sequência completa...")
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    start_ca4()

    # Vai clicar no botão para adicionar personagem
    click_img_s(Paths.IMG_CA4_CREATE_G3_FREE_BONE_ACTOR)

def step(aat: AniAutoTask):
    pyautogui.sleep(2)  # Sleep for a while before checking again
    janela_encontrada = focar_janela_ca4()
    print_alt("janela_encontrada", janela_encontrada)
    if not janela_encontrada:
        print_alt("O Cartoon Animator 4 não está disponível, vamos carregar o programa...")
        start_ca4()

    # Vai clicar no botão para adicionar personagem
    click_img_s(Paths.IMG_CA4_CREATE_G3_FREE_BONE_ACTOR)
    # caminho = 

def main():
    aat: AniAutoTask = load_ani_auto_task()
    # run_sequence()
    step(aat)
    # pass 



if __name__ == "__main__":
    main()