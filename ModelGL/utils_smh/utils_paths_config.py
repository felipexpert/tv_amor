import json
from pathlib import Path
from typing import Union

from utils.utils_print import print_alt
from utils_smh.classes.config_smh import ConfigSmh

def load_config() -> ConfigSmh:
    debug = False
    BASE_DIR = Path(__file__).resolve().parents[2] # raiz do projeto
    if debug: print_alt("BASE_DIR", BASE_DIR)
    CONFIG_JSON_PATH: str = str(BASE_DIR / "config_smh.json")
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
        return ConfigSmh(**config_dict)

workingDir: Path = load_config().workingDir

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
    IMAGES_DIR:str = cp(MODEL_GL_DIR, "images_smh")
    
    # START - PyAutoGUI images - START
    IMG_SMH_INSTAGRAM_LOGO:str = cp(IMAGES_DIR, "01_smh_instagram_logo.jpg")
    IMG_SMH_INSTAGRAM_LOGO_EXTENSO:str = cp(IMAGES_DIR, "01_smh_instagram_logo_extenso.jpg")
    IMG_SMH_INSTAGRAM_NEW_POST:str = cp(IMAGES_DIR, "01_smh_instagram_new_post.jpg")
    IMG_SMH_INSTAGRAM_NEW_POST_CUT_CONTENT:str = cp(IMAGES_DIR, "01_smh_instagram_new_post_cut_content.jpg")
    IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST:str = cp(IMAGES_DIR, "01_smh_instagram_new_post_create_new_post.jpg")
    IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_REEL:str = cp(IMAGES_DIR, "01_smh_instagram_new_post_create_new_post_reel.jpg")
    IMG_SMH_INSTAGRAM_NEW_POST_CREATE_NEW_POST_SHARE:str = cp(IMAGES_DIR, "01_smh_instagram_new_post_create_new_post_share.jpg")
    IMG_SMH_INSTAGRAM_POST_SUCCEED:str = cp(IMAGES_DIR, "01_smh_instagram_post_succeed.jpg")
    IMG_SMH_INSTAGRAM_POST_FAILED:str = cp(IMAGES_DIR, "01_smh_instagram_post_failed.jpg")
    # END   - PyAutoGUI images - END  
    
    # AAT é ani_auto_task
    SMH_WORKING_DIR: str = str(workingDir)
    SMH_AUTO_TASK_JSON: str = cp(SMH_WORKING_DIR, "manual.json")
    SMH_AUTO_TASK_JSON_SAVIOR: str = cp(SMH_WORKING_DIR, "manual_savior.json")

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