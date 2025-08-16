
from pathlib import Path
import subprocess

import pyautogui
from utils.utils_autogui import WaitResult, click_img_from_imgs_s, click_img_s, contains_img, wait_for_images, wait_for_img, wait_for_img_from_imgs
from utils.utils_autogui_ca4 import hot_key_n_times, press_key_n_times
from utils_smh.classes.manual import Manual, ManualAction, ManualWork
from utils_smh.classes.manual_savior import ManualGL
from utils_smh.classes.social_network import SocialNetwork
from utils_smh.util_load_save_manual_savior_json import save_manual_savior_work
from utils_smh.utils_paths_config import Paths

def chrome_open_browser(profile:str):
    subprocess.Popen([
        "chrome",
        f"--profile-directory={profile}"
    ])
    pyautogui.sleep(0.5)

def chrome_open_website(web_site:str):
    pyautogui.hotkey("ctrl", "l")
    pyautogui.write(web_site)
    pyautogui.press("enter")

def chrome_share_tiktok(smh_id:int, file_path: str, message: str, manual_savior:ManualGL):
    pass

def chrome_share_instagram(smh_id:int, file_path: str, message: str, manual_savior:ManualGL):
    wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_LOGO, Paths.IMG_SMH_INSTAGRAM_LOGO_EXTENSO])
    pyautogui.sleep(0.5)
    wait_for_img(Paths.IMG_SMH_INSTAGRAM_NEW_POST)
    pyautogui.sleep(0.5)
    click_img_s(Paths.IMG_SMH_INSTAGRAM_NEW_POST)
    pyautogui.press("tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(0.5)
    pyautogui.press("tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(1)
    pyautogui.write(file_path)
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    wait_for_img(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CUT_CONTENT)
    press_key_n_times("tab", 2)
    pyautogui.press("enter")
    press_key_n_times("tab", 2)
    pyautogui.press("enter")
    wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL])
    click_img_from_imgs_s([Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL], offset_x=137, offset_y=177)
    pyautogui.sleep(0.5)
    pyautogui.write(message)
    pyautogui.sleep(0.5)
    click_img_s(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_SHARE)
    
    # verificação do resultado
    waiting = True
    while waiting:
        wait_result:WaitResult = wait_for_images(Paths.IMG_SMH_INSTAGRAM_POST_SUCCEED, Paths.IMG_SMH_INSTAGRAM_POST_FAILED)
        match wait_result:
            case WaitResult.SUCCESS:
                save_manual_savior_work(manual_savior, smh_id, SocialNetwork.SNInstagram)
                pyautogui.press("esc")
                waiting = False
            case WaitResult.FAILURE:
                # Vai tentar novamente
                pyautogui.sleep(2)
                pyautogui.press("tab")
                pyautogui.sleep(0.5)
                pyautogui.press("enter")
            case WaitResult.TIMEOUT:
                print("Timeout! Erro desconhecido...")
                waiting = False
    