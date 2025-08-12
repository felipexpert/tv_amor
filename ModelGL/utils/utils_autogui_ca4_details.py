
import pyautogui
from utils.classes.ani_auto_task import EPeNumber
from utils.utils_autogui import click_img_s, click_to_deselect
from utils.utils_paths_config import Paths

def persona_number_ca4_selector(personaNumber: EPeNumber):
        click_to_deselect()
        file_path:str = ""
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_G3_ACTOR)
        match personaNumber:
                case EPeNumber.EPeNum1:
                        pyautogui.press('down')
                case EPeNumber.EPeNum2:
                        pyautogui.press('down')
                        pyautogui.sleep(0.5)
                        pyautogui.press('down')
        pyautogui.sleep(1)

def persona_number_ca4_selector_bkp(personaNumber: EPeNumber):
        click_to_deselect()
        file_path:str = ""
        match personaNumber:
                case EPeNumber.EPeNum1:
                        file_path = Paths.IMG_CA4_FIND_PERSONA_1
                case EPeNumber.EPeNum2:
                        file_path = Paths.IMG_CA4_FIND_PERSONA_2
        pyautogui.sleep(1)
        click_img_s(file_path)

def milliseconds_to_frames(milliseconds):
    fps = 30
    return (milliseconds * fps) // 1000 + 1