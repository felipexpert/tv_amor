import subprocess
from typing import List, Tuple

import pyautogui

from utils_smh.classes.config_smh import ConfigSmh
from utils_smh.classes.manual_for_gui import ChromeProfile, ManualForGUI, SocialNetworkWorks, Work
from utils_smh.classes.manual_savior import ManualGL
from utils_smh.classes.social_network import SocialNetwork
from utils_smh.classes.util_filter_manual_done import filter_manual_done
from utils_smh.util_load_save_manual_savior_json import load_manual_savior, save_manual_savior_work
from utils_smh.utils_load_manual_json import load_manual
from utils_smh.utils_manual_gui import chrome_open_browser, chrome_open_website, chrome_share_instagram, chrome_share_tiktok
from utils_smh.utils_manual_to_manual_gl import manual_to_manual_for_gui
from utils_smh.utils_paths_config import load_config

def load_manual_for_gui_filtered() -> Tuple[ManualForGUI, ManualGL]:
    debug = False
    
    config_smh:ConfigSmh = load_config()
    if debug: print("config_smh", config_smh)
    manual = load_manual()
    manual_for_gui:ManualForGUI = manual_to_manual_for_gui(manual, config_smh)
    (ini_total_profiles, ini_total_works) = count_work(manual_for_gui)
    print(f"Total de contas iniciais p/ fazer: {ini_total_profiles}. Total de trabalhos iniciais p/ fazer: {ini_total_works}")
    manual_savior:ManualGL = load_manual_savior()
    filter_manual_done(manual_for_gui, manual_savior)
    (total_profiles, total_works) = count_work(manual_for_gui)
    print(f"Total de contas p/ fazer: {total_profiles}. Total de trabalhos p/ fazer: {total_works}")
    return (manual_for_gui, manual_savior)

def count_work(manual_for_gui: ManualForGUI) -> Tuple[int, int]:
    total_profiles = len(manual_for_gui.chromeProfiles)
    total_works = 0
    for p in manual_for_gui.chromeProfiles:
        p:ChromeProfile = p 
        snworks:List[SocialNetworkWorks] = p.socialNetworks
        for snwork in snworks:
            snwork:SocialNetworkWorks = snwork
            works_len = len(snwork.works)
            total_works += works_len
    return (total_profiles, total_works)
        

def share_all(manual_for_gui:ManualForGUI, manual_savior:ManualGL):
    for cprof in manual_for_gui.chromeProfiles:
        cprof:ChromeProfile
        chrome_profile_name_str:str = cprof.chromeProfile
        chrome_open_browser(chrome_profile_name_str)
        pyautogui.sleep(1)
        pyautogui.hotkey('win', 'up') #maximiza
        for snworks in cprof.socialNetworks:
            snworks:SocialNetworkWorks = snworks
            match snworks.socialNetwork:
                case SocialNetwork.SNInstagram:
                    for w in snworks.works:
                        share_instagram(w, manual_savior)
                case SocialNetwork.SNTiktok:
                    for w in snworks.works:
                        share_tiktok(w, manual_savior)

        # Após os trabalhos, fecha o Navegador Chrome
        pyautogui.hotkey("alt", "f4")

def share_tiktok(work:Work, manual_savior:ManualGL):
    print("Compartilhará no Instagram...")
    file_path:str = work.mediaPath
    message:str = work.messageOpt if work.messageOpt else "" 
    # The line `pyautogui.hotkey("ctrl", "v"file_path)` seems to have a syntax error. It looks like
    # there is a missing comma between `"v"` and `file_path`. To fix this error, you should separate
    # the arguments with a comma like this:
    chrome_open_website("https://www.tiktok.com/tiktokstudio/upload")
    pyautogui.sleep(1)
    pyautogui.hotkey("ctrl", "0") # restaura o zoom
    chrome_share_tiktok(work.smhId, file_path, message, manual_savior)


def share_instagram(work:Work, manual_savior:ManualGL):
    print("Compartilhará no Instagram...")
    file_path:str = work.mediaPath
    message:str = work.messageOpt if work.messageOpt else "" 
    chrome_open_website("https://www.instagram.com/")
    # chrome_open_website("https://www.instagram.com/ad_tools") 
    pyautogui.sleep(1)
    pyautogui.hotkey("ctrl", "0") # restaura o zoom
    chrome_share_instagram(work.smhId, file_path, message, manual_savior)

def main():
    debug = False
    (manual_for_gui, manual_savior) = load_manual_for_gui_filtered()
    share_all(manual_for_gui, manual_savior)
    
if __name__ == "__main__":
    main()
