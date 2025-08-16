import subprocess
from typing import Tuple

import pyautogui

from utils_smh.classes.config_smh import ConfigSmh
from utils_smh.classes.manual_for_gui import ChromeProfile, ManualForGUI, SocialNetworkWorks, Work
from utils_smh.classes.manual_savior import ManualGL
from utils_smh.classes.social_network import SocialNetwork
from utils_smh.classes.util_filter_manual_done import filter_manual_done
from utils_smh.util_load_save_manual_savior_json import load_manual_savior, save_manual_savior_work
from utils_smh.utils_load_manual_json import load_manual
from utils_smh.utils_manual_gui import chrome_open_browser, chrome_open_website, chrome_share_instagram
from utils_smh.utils_manual_to_manual_gl import manual_to_manual_for_gui
from utils_smh.utils_paths_config import load_config

def load_manual_for_gui_filtered() -> Tuple[ManualForGUI, ManualGL]:
    debug = False
    
    config_smh:ConfigSmh = load_config()
    if debug: print("config_smh", config_smh)
    manual = load_manual()
    manual_for_gui:ManualForGUI = manual_to_manual_for_gui(manual, config_smh)
    manual_savior:ManualGL = load_manual_savior()
    filter_manual_done(manual_for_gui, manual_savior)
    return (manual_for_gui, manual_savior)

def share_all(manual_for_gui:ManualForGUI, manual_savior:ManualGL):
    for cprof in manual_for_gui.chromeProfiles:
        cprof:ChromeProfile
        chrome_profile_name_str:str = cprof.chromeProfile
        chrome_open_browser(chrome_profile_name_str)
        for snworks in cprof.socialNetworks:
            snworks:SocialNetworkWorks = snworks
            match snworks.socialNetwork:
                case SocialNetwork.SNInstagram:
                    for w in snworks.works:
                        share_instagram(w, manual_savior)

        # Após os trabalhos, fecha o Navegador Chrome
        pyautogui.hotkey("alt", "f4")


def share_instagram(work:Work, manual_savior:ManualGL):
    print("Compartilhará no Instagram...")
    file_path:str = work.mediaPath
    message:str = work.messageOpt if work.messageOpt else "" 
    chrome_open_website("https://www.instagram.com/")
    chrome_share_instagram(work.smhId, file_path, message, manual_savior)

def share_tiktok(work:Work):
    print("Compartilhará no Tiktok...")
    pass

def main():
    debug = False
    (manual_for_gui, manual_savior) = load_manual_for_gui_filtered()
    share_all(manual_for_gui, manual_savior)
    
if __name__ == "__main__":
    main()
