from pathlib import Path
import subprocess
from typing import Union

import pyautogui

from utils.utils_conexao import assegura_offline
from utils.utils_print import print_alt
from utils.classes.ani_auto_task import AniAutoTask
from utils.utils_autogui import click_img_s, focus_window_ca4, wait_for_img
from utils.utils_paths_config import Paths

def open_ca4():
    _caminho_ca4: str = "C:\\Program Files\\Reallusion\\Cartoon Animator 4\\bin64\\CartoonAnimator.exe"
    print("Vai abrir o CA4 do caminho:")
    print(_caminho_ca4)
    subprocess.Popen([_caminho_ca4])
    
    print("Aguardando CA4 carregar...")
    
    wait_for_img(Paths.IMG_CA4_START)
    click_img_s(Paths.IMG_CA4_BTN_THANK_YOU)

def start_ca4():
    assegura_offline()
    print_alt("Iniciando vídeo...")
    open_ca4()

def working_dir_file(file: Union[str,Path]) -> str:
   return str(Path(Paths.AAT_WORKING_DIR) / (Path(str(file))))


def add_personas(aat: AniAutoTask):
    # Vai clicar no botão para adicionar personagem
    # de acordo com o número de personagens, faz esta ação
    personasQtd = len(aat.aatPersonas)
    for pe_number in range(1, (personasQtd + 1)):
        pyautogui.sleep(2)
        click_img_s(Paths.IMG_CA4_CREATE_G3_FREE_BONE_ACTOR)
        # psd_path:str = str(Path(Paths.AAT_WORKING_DIR) / Path(f"{pe_number}.psd"))
        psd_path:str = working_dir_file(f"persona_{pe_number}.psd")
        pyautogui.sleep(1)
        pyautogui.write(psd_path)
        pyautogui.sleep(0.5)
        pyautogui.press("enter")
        pyautogui.sleep(7)
        # Depois que ele adiciona, tem que voltar para a tela anterior
        click_img_s(Paths.IMG_CA4_BACK_STAGE)

def flip_persona_1_if_needed(aat: AniAutoTask):
    personasQtd = len(aat.aatPersonas)
    if personasQtd > 1:
        pyautogui.sleep(1)
        click_img_s(Paths.IMG_CA4_FIND_PERSONA_1)
        pyautogui.sleep(0.5)
        pyautogui.keyDown('alt')
        pyautogui.sleep(0.5)
        pyautogui.press('e')
        pyautogui.sleep(0.5)
        pyautogui.press('f')
        pyautogui.sleep(0.5)
        pyautogui.press('h')
        pyautogui.sleep(0.5)
        pyautogui.keyUp('alt')
        pyautogui.sleep(0.5)


def add_background(aat: AniAutoTask):
    pyautogui.sleep(2)
    click_img_s(Paths.IMG_CA4_CREATE_MEDIA)
    print_alt("funcionou click em IMG_CA4_CREATE_MEDIA")
    pyautogui.sleep(1)
    click_img_s(Paths.IMG_CA4_BACKGROUND)
    pyautogui.sleep(1)
    bg_path = working_dir_file(aat.aatBackgroundImage)
    pyautogui.write(bg_path)
    pyautogui.sleep(0.5)
    pyautogui.press("enter")

def focus_or_open_ca4():
    # pyautogui.sleep(2)  # Sleep for a while before checking again
    janela_encontrada = focus_window_ca4()
    print_alt("janela_encontrada", janela_encontrada)
    if not janela_encontrada:
        print_alt("O Cartoon Animator 4 não está disponível, vamos carregar o programa...")
        start_ca4()