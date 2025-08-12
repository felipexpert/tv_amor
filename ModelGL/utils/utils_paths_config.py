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
    IMAGES_DIR:str = cp(MODEL_GL_DIR, "images")
    
    # START - PyAutoGUI images - START
    IMG_CA4_START:str = cp(IMAGES_DIR, "01_ca4_start.jpg")
    IMG_CA4_BTN_THANK_YOU:str = cp(IMAGES_DIR, "02_ca4_btn_thank_you.jpg")
    IMG_CA4_CREATE_G3_FREE_BONE_ACTOR:str = cp(IMAGES_DIR, "03_ca4_create_g3_free_bone_actor.jpg")
    IMG_CA4_BACK_STAGE:str = cp(IMAGES_DIR, "04_ca4_back_stage.jpg")
    IMG_CA4_CREATE_MEDIA:str = cp(IMAGES_DIR, "05_ca4_create_media.jpg")
    IMG_CA4_BACKGROUND:str = cp(IMAGES_DIR, "06_ca4_background.jpg")
    IMG_CA4_G3_ACTOR:str = cp(IMAGES_DIR, "07_ca4_find_g3_actor.jpg")
    IMG_CA4_FIND_PERSONA_1:str = cp(IMAGES_DIR, "07_ca4_find_persona_1.jpg")
    IMG_CA4_FIND_PERSONA_2:str = cp(IMAGES_DIR, "07_ca4_find_persona_2.jpg")
    IMG_CA4_POS_X:str = cp(IMAGES_DIR, "08_ca4_pos_x.jpg")
    IMG_CA4_POS_Y:str = cp(IMAGES_DIR, "08_ca4_pos_y.jpg")
    IMG_CA4_TIMELINE:str = cp(IMAGES_DIR, "09_ca4_timeline.jpg")
    IMG_CA4_TIMELINE_PLAYHEAD:str = cp(IMAGES_DIR, "10_ca4_timeline_playhead.jpg")
    IMG_CA4_CREATE_SCRIPT:str = cp(IMAGES_DIR, "11_ca4_create_script.jpg")
    IMG_CA4_WAVE_FILE:str = cp(IMAGES_DIR, "11_ca4_wave_file.jpg")
    # END   - PyAutoGUI images - END
    
    # AAT é ani_auto_task
    AAT_WORKING_DIR: str = str(workingDir)
    ANI_AUTO_TASK_JSON: str = cp(AAT_WORKING_DIR, "ani_auto_task.json")
    
    
    

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