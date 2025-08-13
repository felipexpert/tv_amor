from pathlib import Path
from PIL import Image

from utils.utils_print import print_alt
from utils.classes.ani_auto_task import AniAutoTask
from utils.classes.config import Config
from utils.utils_autogui_ca4 import get_background_size, working_dir_file
from utils.utils_load_ani_auto_task_json import load_ani_auto_task
from utils.utils_paths_config import load_config




def main():
    # Caminho da imagem — altere para o arquivo que você quer testar
    aat: AniAutoTask = load_ani_auto_task()

    w, h = get_background_size(aat)
    print(f"Largura: {w}px, Altura: {h}px")


if __name__ == "__main__":
    main()
