
import pyautogui
from ModelGL.utils.utils_autogui import click_img_s
from utils.utils_paths_config import Paths

def persona_number_ca4_selector(personaNumber: str):
        file_path:str = ""
        match personaNumber:
                case "EPeNum1":
                        file_path = Paths.IMG_CA4_FIND_PERSONA_1
                case "EPeNum2":
                        file_path = Paths.IMG_CA4_FIND_PERSONA_2
        pyautogui.sleep(1)
        click_img_s(file_path)