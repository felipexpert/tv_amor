import json
from pathlib import Path
from typing import Union

from utils.utils_print import print_alt
from utils.classes.config import Config

def load_config() -> Config:
    debug = False
    BASE_DIR = Path(__file__).resolve().parents[2] # raiz do projeto
    if debug: print_alt("BASE_DIR", BASE_DIR)
    CONFIG_JSON_PATH: str = str(BASE_DIR / "config.json")
    if debug: print_alt("CONFIG_JSON_PATH", CONFIG_JSON_PATH)


    # root_dir = Path(__file__).resolve().parents[1]  # diretório pai de ModelGL
    # config_path = root_dir / "config.json"
    config_path_str = CONFIG_JSON_PATH
    config_path = Path(config_path_str)
    if debug: print_alt("config_path", config_path)
    with open(config_path, "r", encoding="utf-8") as f:
        if debug: print_alt("f", f)
        config_dict = json.load(f)
        if debug: print_alt("configDict", config_dict)
        return Config(**config_dict)

workingDir: Path = load_config()

def cp(*paths: Union[Path, str]) -> str:
    """
    cp - concat paths
    """
    if not paths:
        raise ValueError("É necessário pelo menos um path para base")

    base = Path(str(paths[0]))
    for p in paths[1:]:
        p2 = Path(str(p))
        base /= p2
    return str(base)
class Paths:
    BASE_DIR = cp(Path(__file__).resolve().parents[2]) # raiz do projeto
    MODEL_GL_DIR:str = cp(BASE_DIR, "ModelGL")
    IMAGES_DIR:str = cp(MODEL_GL_DIR, "images")
    
    # START - PyAutoGUI images - START
    IMG_CA4_START:str = cp(IMAGES_DIR, "01_ca4_start.jpg")
    IMG_CA4_BTN_THANK_YOU:str = cp(IMAGES_DIR, "02_ca4_btn_thank_you.jpg")
    # END   - PyAutoGUI images - END
    
    
    ANI_AUTO_TASK_JSON: str = cp(workingDir, "ani_auto_task.json")
    
    
    

# Teste
# print(Paths.BASE_DIR, type(Paths.BASE_DIR))
# print(Paths.IMG_CA4_START)
def basename(path_str: str) -> str:
    """
    Retorna o nome do arquivo (último trecho) de um caminho.

    Args:
        path_str (str): Caminho completo como string.

    Returns:
        str: Nome do arquivo ou último diretório.
    """
    return Path(path_str).name