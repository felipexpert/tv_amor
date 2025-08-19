from utils_smh.classes.config_smh import ConfigSmh
from utils_smh.classes.manual_savior import ManualGL, ShareMediaHistoryWork
from utils_smh.classes.social_network import SocialNetwork
from utils_smh.classes.util_filter_manual_done import filter_manual_done
from utils_smh.util_load_save_manual_savior_json import load_manual_savior, save_manual_savior
from utils_smh.utils_load_manual_json import load_manual
from utils_smh.utils_manual_to_manual_gl import manual_to_manual_for_gui  
from utils_smh.utils_paths_config import load_config


def main():
    # test_manual_savior()
    test_manual_for_gui()

def test_manual_for_gui():
    debug = False
    
    config_smh:ConfigSmh = load_config()
    if debug: print("config_smh", config_smh)
    manual = load_manual()
    manual_for_gui = manual_to_manual_for_gui(manual, config_smh)
    manual_savior:ManualGL = load_manual_savior()
    filter_manual_done(manual_for_gui, manual_savior)
    print("manual_for_gui", manual_for_gui)

def test_manual_savior():
    # carrega o manual_gl
    mg:ManualGL = load_manual_savior()
    print("mg", mg)
    
    mg.mglWorks.append(ShareMediaHistoryWork(wSmhId=1, wDones=[SocialNetwork.SNInstagram]))
    save_manual_savior(mg)
    
if __name__ == "__main__":
    main()