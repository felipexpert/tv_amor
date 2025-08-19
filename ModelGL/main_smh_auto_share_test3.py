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

def main():
    chrome_open_browser("Default")
    
if __name__ == "__main__":
    main()
