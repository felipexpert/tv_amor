
from pathlib import Path
from typing import List
from PIL import Image
import pyautogui

from utils.utils_load_ani_auto_task_json import load_ani_auto_task
from utils.classes.ani_auto_task import ASpeech, AniAutoTask, EPeNumber, TPeAction
from utils.utils_print import print_alt
from utils.utils_autogui_ca4 import add_background, add_persona_speech, add_personas, display_timeline_if_hidden, flip_persona_1_if_needed, focus_or_open_ca4, click_img_s, place_personas, set_video_total_duration, start_ca4, working_dir_file
from utils.utils_paths_config import Paths


def run_sequence(aat: AniAutoTask):
    print_alt("Rodando a sequência completa...")
    # Para habilitar/desabilitar internet:
    # Windows + R
    # ncpa.cpl
    start_ca4()
    run_sequence_ca4_opened(aat)

def run_sequence_ca4_opened(aat: AniAutoTask):
    focus_or_open_ca4()
    add_personas(aat)
    add_background(aat)
    place_personas(aat)
    flip_persona_1_if_needed(aat)
    set_video_total_duration(aat)
    display_timeline_if_hidden()

def step(aat: AniAutoTask):
    focus_or_open_ca4()
    display_timeline_if_hidden()
    action_speechs:List[ASpeech] = get_aat_action_speechs(aat)
    first_action_speech:TPeAction = action_speechs[0]
    add_persona_speech(first_action_speech.tpaNumber, first_action_speech.tpaAction)

    
def get_aat_action_speechs(aat: AniAutoTask) -> List[TPeAction]:
    action_speechs:List[TPeAction] = []
    for action in aat.aatActions:
        action: TPeAction = action
        if action.tpaAction.tag == "ASpeech":
            action_speechs.append(action)
    return action_speechs



    

def main():
    aat: AniAutoTask = load_ani_auto_task()

    # run_sequence(aat)

    step(aat)
    
    # pass 



if __name__ == "__main__":
    main()