from pathlib import Path
from utils.paths2 import cp

class Paths:
    BASE_DIR = cp(Path(__file__).resolve().parents[2]) # raiz do projeto
    MODEL_GL_DIR:str = cp(BASE_DIR, "ModelGL")
    IMAGES_DIR:str = cp(MODEL_GL_DIR, "images")
    
    # START - PyAutoGUI images - START
    IMG_CA4_START:str = cp(IMAGES_DIR, "01_ca4_start.jpg")
    IMG_CA4_BTN_THANK_YOU:str = cp(IMAGES_DIR, "02_ca4_btn_thank_you.jpg")
    # END   - PyAutoGUI images - END  
    

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