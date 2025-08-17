
from pathlib import Path
import subprocess

import pyautogui
import pyperclip
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
    pyautogui.sleep(1)
    if contains_img(Paths.IMG_CHROME_RESTAURAR):
        # click_img_from_imgs_s(Paths.IMG_CHROME_RESTAURAR_RESTAURAR)
        pyautogui.press("tab")
        pyautogui.sleep(0.5)
        pyautogui.press("enter")

    pyperclip.copy(web_site)
    pyautogui.sleep(0.5)
    pyautogui.hotkey("ctrl", "l")
    pyautogui.sleep(0.5)
    pyautogui.hotkey("ctrl", "v")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    # pyautogui.sleep(3)
    # hot_key_n_times(["ctrl", "shift", "r"], 3)
    pyautogui.sleep(0.5)

def chrome_share_tiktok(smh_id:int, file_path: str, message: str, manual_savior:ManualGL):
    pyautogui.sleep(0.5)
    pyautogui.press("pagedown")
    btn_select_video:str = wait_for_img_from_imgs([Paths.IMG_SMH_TIKTOK_SELECT_VIDEO, Paths.IMG_SMH_TIKTOK_SELECT_VIDEO_2])
    click_img_s(btn_select_video)
    pyautogui.sleep(1)
    pyperclip.copy(file_path)
    pyautogui.hotkey("ctrl", "v")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(2)
    press_key_n_times("pageup", 2)
    wait_for_img(Paths.IMG_SMH_TIKTOK_ENVIADO)
    press_key_n_times("tab", 2)
    pyautogui.hotkey("ctrl", "a")
    pyautogui.sleep(1)
    pyperclip.copy(message)
    pyautogui.hotkey("ctrl", "v")
    pyautogui.sleep(3)
    press_key_n_times("pagedown", 3)
    pyautogui.sleep(0.5)
    wait_for_img(Paths.IMG_SMH_TIKTOK_PUBLICAR)
    click_img_s(Paths.IMG_SMH_TIKTOK_PUBLICAR)
    pyautogui.sleep(2)
    has_publicar_agora_btn = contains_img(Paths.IMG_SMH_TIKTOK_PUBLICAR_AGORA)
    if has_publicar_agora_btn: click_img_s(Paths.IMG_SMH_TIKTOK_PUBLICAR_AGORA)
    # wait_for_images([Paths.IMG_SMH_TIKTOK_VIDEO_PUBLICADO, Paths.IMG_SMH_TIKTOK_VIDEO_PUBLICADO_2])
    pyautogui.sleep(2)
    save_manual_savior_work(manual_savior, smh_id, SocialNetwork.SNTiktok)
    pyautogui.sleep(1)

def chrome_share_instagram(smh_id:int, file_path: str, message: str, manual_savior:ManualGL):
    pyautogui.sleep(1)
    press_key_n_times("pagedown", 2)
    wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_LOGO, Paths.IMG_SMH_INSTAGRAM_LOGO_EXTENSO])
    pyautogui.sleep(0.5)
    found:str = wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_2])
    pyautogui.sleep(0.5)
    click_img_s(found)
    pyautogui.press("tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(0.5)
    pyautogui.press("tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(1)
    pyperclip.copy(file_path)
    pyautogui.hotkey("ctrl", "v")
    # pyautogui.write(file_path)
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    wait_for_img(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CUT_CONTENT)

    #primeiro arruma o posicionamento da mídia no Instagram
    pyautogui.sleep(0.5)
    press_key_n_times("tab", 3)
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(0.5)
    hot_key_n_times(["shift", "tab"], 4)
    # press_key_n_times("tab", 5)
    pyautogui.sleep(0.5)
    pyautogui.press("space")
    pyautogui.sleep(0.5)

    # press_key_n_times("tab", 2)
    pyautogui.hotkey("shift", "tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    press_key_n_times("tab", 2)
    pyautogui.press("enter")
    
    # encontrar a textarea
    # wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL])
    # click_img_from_imgs_s([Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL], offset_x=137, offset_y=177)
    pyautogui.sleep(1)

    # inserindo texto
    pyperclip.copy(message)   # copia para a área de transferência
    pyautogui.click(857, 416) 
    pyautogui.sleep(0.5)
    pyautogui.hotkey("ctrl", "v")  # cola no campo ativo
    pyautogui.sleep(0.5)
    click_img_s(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_SHARE)
    
    # verificação do resultado
    waiting = True
    while waiting:
        wait_result:WaitResult = wait_for_images(Paths.IMG_SMH_INSTAGRAM_POST_SUCCEED, Paths.IMG_SMH_INSTAGRAM_POST_FAILED)
        match wait_result:
            case WaitResult.SUCCESS:
                save_manual_savior_work(manual_savior, smh_id, SocialNetwork.SNInstagram)
                # pyautogui.press("esc")
                pyautogui.press("f5")
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
    