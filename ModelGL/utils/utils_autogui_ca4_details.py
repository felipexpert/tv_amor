
import pyautogui
from utils.classes.ani_auto_task import EPeNumber
from utils.utils_autogui import click_img_s
from utils.utils_paths_config import Paths

def persona_number_ca4_selector(personaNumber: EPeNumber):
        file_path:str = ""
        match personaNumber:
                case EPeNumber.EPeNum1:
                        file_path = Paths.IMG_CA4_FIND_PERSONA_1
                case EPeNumber.EPeNum1:
                        file_path = Paths.IMG_CA4_FIND_PERSONA_2
        pyautogui.sleep(1)
        click_img_s(file_path)