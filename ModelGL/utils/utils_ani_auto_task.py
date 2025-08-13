import math
from typing import List
from utils.classes.ani_auto_task import AGesture, AniAutoTask, CGesture, TPeAction


def get_aat_action_speechs(aat: AniAutoTask) -> List[TPeAction]:
    action_speechs:List[TPeAction] = []
    for action in aat.aatActions:
        action: TPeAction = action
        if action.tpaAction.tag == "ASpeech":
            action_speechs.append(action)
    return action_speechs

# Durações associadas (em frames)
gesture_durations = {
    CGesture.GHi: 33, #
    CGesture.GStandShort: 41, #
    CGesture.GStandLong: 81, #
    CGesture.GThinkShort: 33, #
    CGesture.GThinkLong: 50, #
    CGesture.GTalkShort: 25, #
    CGesture.GTalkLong: 62, #
    CGesture.GWorryShort: 42, #
    CGesture.GWorryLong: 70, #
    CGesture.GShakeLegShort: 18, #
    CGesture.GShakeLegLong: 44, #
    CGesture.GExcited: 74, #
    CGesture.GDance: 174, #
    CGesture.GDefault: 71, #
}

# Função para pegar a duração (com buffer opcional)
def get_gesture_duration_in_frames(gesture: CGesture, buffer: int = 3) -> int:
    base_duration = gesture_durations.get(gesture, 70)  # default 70 se não encontrar
    return base_duration + buffer

def get_gesture_duration_in_millis(gesture: CGesture) -> int:
    frames = get_gesture_duration_in_frames(gesture)
    return frames_to_milliseconds(frames)

def frames_to_milliseconds(frames: int) -> int:
    fps:float = 30
    return int((frames / fps) * 1000)

# fazer uma função que gera a sequencia correta, aplicando GDefault nos espaços vazios, e gerar uma lista com os
# espaços preenchidos por GDefault
def make_gestures_complete(gestures: List[AGesture], total_duration_millis: int) -> List[AGesture]:
    gestures_sorted = sorted(gestures, key=lambda g: g.agStartTime)
    complete_list = []
    current_time = 1  # timeline começa no frame 1
    default_duration = get_gesture_duration_in_millis(CGesture.GDefault)

    def fill_gap(start: int, end: int):
        gap_length = end - start + 1
        count = math.ceil(gap_length / default_duration)
        for i in range(count):
            seg_start = start + i * default_duration
            seg_end = min(seg_start + default_duration - 1, end)
            complete_list.append(
                AGesture(tag="AGesture", agGesture=CGesture.GDefault, agStartTime=seg_start)
            )

    for gesture in gestures_sorted:
        gesture_start = gesture.agStartTime
        gesture_duration = get_gesture_duration_in_millis(gesture.agGesture)
        gesture_end = gesture_start + gesture_duration - 1

        # Se houver lacuna entre current_time e o início do gesto, preenche com GDefault múltiplos
        if gesture_start > current_time:
            fill_gap(current_time, gesture_start - 1)
        
        # Adiciona o gesto atual
        complete_list.append(gesture)
        current_time = max(current_time, gesture_end + 1)

    # Preenche o espaço após o último gesto até o final do vídeo
    if current_time <= total_duration_millis:
        fill_gap(current_time, total_duration_millis)

    return complete_list