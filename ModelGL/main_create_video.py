
from pathlib import Path
from typing import List
from PIL import Image
import pyautogui

from utils.classes.config import Config
from utils.utils_load_ani_auto_task_json import load_ani_auto_task
from utils.classes.ani_auto_task import ASpeech, AniAutoTask, EPeNumber, GestureApplicationType, TPeAction
from utils.utils_print import print_alt
from utils.utils_autogui_ca4 import add_all_speeches, add_background, add_personas, add_personas_gestures_default, add_personas_without_gesture_stay_normal, add_personas_without_gesture_stay_static, compact_video, display_timeline_if_hidden, flip_persona_1_if_needed, focus_or_open_ca4, place_personas, prepare_render_video_configs, render_video, set_video_total_duration, start_ca4
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
    prepare_render_video_configs(aat)
    place_personas(aat)
    flip_persona_1_if_needed(aat)
    set_video_total_duration(aat)
    display_timeline_if_hidden()
    add_all_speeches(aat, config)
    gestures(aat)
    render_video()

def gestures(aat: AniAutoTask):
    match aat.aatGestureApplicationType:
        case GestureApplicationType.GATDefault:
            add_personas_gestures_default(aat)
        case GestureApplicationType.GATWithoutGestureStayStatic:
            add_personas_without_gesture_stay_static(aat)
        case GestureApplicationType.GATWithoutGestureStayNormal:
            add_personas_without_gesture_stay_normal(aat)

def step(aat: AniAutoTask, config: Config):
    # focus_or_open_ca4()
    # render_video()

    # video_file_temp_path:str = str(Path(Paths.AAT_WORKING_DIR) / Path('video_temp.mp4')) # video temp
    # pyautogui.write(video_file_temp_path)
    # compact_video(video_file_temp_path)
    
    focus_or_open_ca4()
    add_personas(aat)
    add_background(aat)
    prepare_render_video_configs(aat)
    place_personas(aat)
    flip_persona_1_if_needed(aat)
    set_video_total_duration(aat)
    display_timeline_if_hidden()
    # add_all_speeches(aat, config)
    # render_video()
    # add_personas_gestures(aat)
    gestures(aat)

    



    

def main():
    config:Config = load_config()
    aat: AniAutoTask = load_ani_auto_task()

    # print(aat.aatGestureApplicationType)
    # run_sequence(aat, config)

    # run_sequence_ca4_opened(aat, config)

    step(aat, config)
    
    # pass 



if __name__ == "__main__":
    main()