
from pathlib import Path
from PIL import Image
import pyautogui

from utils.utils_load_ani_auto_task_json import load_ani_auto_task
from utils.classes.ani_auto_task import AniAutoTask
from utils.utils_print import print_alt
from utils.utils_autogui_ca4 import add_background, add_personas, focus_or_open_ca4, click_img_s, start_ca4, working_dir_file
from utils.utils_paths_config import Paths


def run_sequence(aat: AniAutoTask):
    print_alt("Rodando a sequência completa...")
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    start_ca4()

    # Vai clicar no botão para adicionar personagem
    click_img_s(Paths.IMG_CA4_CREATE_G3_FREE_BONE_ACTOR)

    add_personas(aat)

def step(aat: AniAutoTask):
    focus_or_open_ca4()

    add_personas(aat)

    add_background(aat)

def main():
    aat: AniAutoTask = load_ani_auto_task()
    # run_sequence()
    step(aat)
    # pass 



if __name__ == "__main__":
    main()