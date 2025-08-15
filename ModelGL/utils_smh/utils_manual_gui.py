
from pathlib import Path
import subprocess

import pyautogui
from utils_smh.classes.manual import Manual, ManualAction, ManualWork
from utils_smh.utils_paths_config import Paths


def run_works(manual: Manual):
    print("Inicial os trabalhos...")
    for acc in manual.mAccounts:
        for work in acc.maWorks:
            do_work(work)
        
def do_work(work:ManualWork):
    smh_id = work.mwSmh.id
    file_code = work.mwFileCode
    file_path = Path(Paths.SMH_WORKING_DIR) / Path(file_code)
    print(f"SMH ID {smh_id}")
    print(f"Filecode {file_code}")
    print(f"file_path {file_path}")
    mac_ig:ManualAction = work.mwActionIgOpt
    mac_tk:ManualAction = work.mwActionTkOpt

def chrome_work(profile:str):
    subprocess.Popen([
        "chrome",
        f"--profile-directory={profile}"
    ])
    pyautogui.sleep(0.5)
    chrome_open_website("www.instagram.com")

def chrome_open_website(web_site:str):
    pyautogui.hotkey("ctrl", "l")
    pyautogui.write(web_site)
    pyautogui.press("enter")