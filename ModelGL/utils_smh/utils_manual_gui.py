
from enum import Enum
import os
from pathlib import Path
import subprocess

import pyautogui
import pyperclip
from utils.utils_autogui import WaitResult, click_img_from_imgs_s, click_img_s, contains_img, wait_for_images, wait_for_img, wait_for_img_from_imgs
from utils.utils_autogui_ca4 import get_image_size, get_video_size, hot_key_n_times, press_key_n_times
from utils_smh.classes.manual import Manual, ManualAction, ManualWork
from utils_smh.classes.manual_savior import ManualGL
from utils_smh.classes.social_network import SocialNetwork
from utils_smh.util_load_save_manual_savior_json import save_manual_savior_work
from utils_smh.utils_paths_config import Paths
import utils.utils_paths_config as p

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
    pyperclip.copy(file_path)
    pyautogui.sleep(2)
    pyautogui.hotkey("ctrl", "v")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(2.5)
    if contains_img(Paths.IMG_SMH_TIKTOK_QUESTAO_ATIVAR_VERIFICACOES):
        click_img_s(Paths.IMG_SMH_TIKTOK_QUESTAO_ATIVAR_VERIFICACOES_ATIVAR)
        pyautogui.sleep(0.5)
    press_key_n_times("pageup", 2)
    wait_for_img(Paths.IMG_SMH_TIKTOK_ENVIADO)
    click_img_s(Paths.IMG_SMH_TIKTOK_ENVIADO, offset_x=385, offset_y=259)
    # press_key_n_times("tab", 2)
    pyautogui.hotkey("ctrl", "a")
    pyautogui.sleep(1)
    pyperclip.copy(message)
    pyautogui.hotkey("ctrl", "v")
    pyautogui.sleep(2)
    def acao_de_publicar():
        press_key_n_times("pagedown", 3)
        pyautogui.sleep(0.5)
        wait_for_img(Paths.IMG_SMH_TIKTOK_PUBLICAR)
        click_img_s(Paths.IMG_SMH_TIKTOK_PUBLICAR)
        pyautogui.sleep(2)
        has_publicar_agora_btn = contains_img(Paths.IMG_SMH_TIKTOK_PUBLICAR_AGORA)
        if has_publicar_agora_btn: click_img_s(Paths.IMG_SMH_TIKTOK_PUBLICAR_AGORA)
    acao_de_publicar()
    # press_key_n_times("pageup", 2)
    # wait_for_images([Paths.IMG_SMH_TIKTOK_VIDEO_PUBLICADO, Paths.IMG_SMH_TIKTOK_VIDEO_PUBLICADO_2])
    pyautogui.sleep(3)
    if contains_img(Paths.IMG_SMH_TIKTOK_ERRO_MSG_SAIR_DO_SITE):
        # Deu erro, precisa clicar novamente para tentar novamente
        click_img_from_imgs_s(Paths.IMG_SMH_TIKTOK_ERRO_MSG_SAIR_DO_SITE_BTN_CANCELAR)
        pyautogui.sleep(1)
        acao_de_publicar()
        pyautogui.sleep(3) # espera 3 segundos para a próxima ação
    url_atual = obtem_url_atual()
    if "content" in url_atual:
        save_manual_savior_work(manual_savior, smh_id, SocialNetwork.SNTiktok)
        pyautogui.sleep(1)
    else:
        raise Exception("falha ao compartilhar no Tiktok")

def obtem_url_atual() -> str:
    # Combinação de teclas para selecionar a barra de endereço e copiar
    pyautogui.hotkey('ctrl', 'l') 
    pyautogui.hotkey('ctrl', 'c')

    # Obtém o conteúdo do clipboard (área de transferência)
    url = pyperclip.paste()
    return url

def chrome_share_instagram(smh_id:int, file_path: str, message: str, manual_savior:ManualGL):
    pyautogui.sleep(1)
    press_key_n_times("pagedown", 2)
    wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_LOGO, Paths.IMG_SMH_INSTAGRAM_LOGO_EXTENSO])
    pyautogui.sleep(0.5)
    found:str = wait_for_img_from_imgs([Paths.IMG_SMH_INSTAGRAM_NEW_POST, Paths.IMG_SMH_INSTAGRAM_NEW_POST_2])
    click_img_s(found)
    pyautogui.sleep(1.5)
    pyautogui.press("tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(1)
    pyautogui.press("tab")
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyperclip.copy(file_path)
    pyautogui.sleep(2)
    pyautogui.hotkey("ctrl", "v")
    # pyautogui.write(file_path)
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    wait_for_img(Paths.IMG_SMH_INSTAGRAM_NEW_POST_CUT_CONTENT)

    is_square = is_for_sure_square_media(file_path)

    if not is_square:
        # primeiro arruma o posicionamento da mídia no Instagram, porque não é ou pode
        # não ser mídia quadrada
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
        pyautogui.sleep(0.5)
    else:
        # é mídia quadrada, vai para a próxima tela
        pyautogui.sleep(0.5)
        press_key_n_times("tab", 2)
        pyautogui.sleep(0.5)
        pyautogui.press("enter")
        pyautogui.sleep(0.5)

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
                # pyautogui.press("f5")
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

def is_for_sure_square_media(file_path: str) -> bool:
    debug = False
    width:int = None
    height:int = None
    ft:FileType = get_file_type(file_path)
    if debug: print(f"File is: {p.basename(file_path)}")
    if debug: print(f"file_type is {ft}")
    match(ft):
        case FileType.VIDEO:
            w,h = get_video_size(file_path)
            width:int = w
            height:int = h
        case FileType.IMAGE:
            w,h = get_image_size(file_path)
            width:int = w
            height:int = h
        case FileType.UNKNOWN:
            return False
    return width == height

class FileType(Enum):
    VIDEO = "VIDEO"
    IMAGE = "IMAGE"
    UNKNOWN = "UNKNOWN"            

def get_file_type(file_path: str) -> FileType:
    debug = False
    ext:str = get_file_extension(file_path)
    if debug: print(f"extensão: {ext}")
    match ext:
        case "mp4" | "avi": return FileType.VIDEO
        case "jpg" | "jpeg" | "png" | "webp" | "jfif" | "jif" | "gif" | "svg": return FileType.IMAGE
        case _: return FileType.UNKNOWN
      
def get_file_extension(file_path: str) -> str:
    ext:str = os.path.splitext(file_path)[1]  # retorna ".mp4"
    ext_lower:str = ext.lower()[1:] # apenas letras
    return ext_lower