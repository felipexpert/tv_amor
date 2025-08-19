from utils_smh.classes.config_smh import ConfigSmh
from utils_smh.utils_load_manual_json import load_manual
from utils_smh.utils_paths_config import load_config


def main():
    debug = False
    config_smh:ConfigSmh = load_config()
    if debug: print("config_smh", config_smh)
    manual = load_manual()
    print("manual", manual)
    
if __name__ == "__main__":
    main()