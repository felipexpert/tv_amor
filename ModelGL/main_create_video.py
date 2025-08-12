
from pathlib import Path
from typing import List
from PIL import Image
import pyautogui

from utils.classes.config import Config
from utils.utils_load_ani_auto_task_json import load_ani_auto_task
from utils.classes.ani_auto_task import ASpeech, AniAutoTask, EPeNumber, TPeAction
from utils.utils_print import print_alt
from utils.utils_autogui_ca4 import add_all_speeches, add_background, add_persona_speech, add_personas, display_timeline_if_hidden, flip_persona_1_if_needed, focus_or_open_ca4, click_img_s, place_personas, set_video_total_duration, start_ca4, working_dir_file
from utils.utils_paths_config import Paths, load_config


def run_sequence(aat: AniAutoTask, config: Config):
    print_alt("Rodando a sequência completa...")
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    start_ca4()
    run_sequence_ca4_opened(aat, config)

def run_sequence_ca4_opened(aat: AniAutoTask, config: Config):
    focus_or_open_ca4()
    add_personas(aat)
    add_background(aat)
    place_personas(aat)
    flip_persona_1_if_needed(aat)
    set_video_total_duration(aat)
    display_timeline_if_hidden()
    add_all_speeches(aat, config)

def step(aat: AniAutoTask, config: Config):
    focus_or_open_ca4()
    add_personas(aat)

    



    

def main():
    config:Config = load_config()
    aat: AniAutoTask = load_ani_auto_task()

    # run_sequence(aat, config)

    run_sequence_ca4_opened(aat, config)

    step(aat, config)
    
    # pass 



if __name__ == "__main__":
    main()