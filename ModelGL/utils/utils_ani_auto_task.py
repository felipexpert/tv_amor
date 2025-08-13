from typing import List
from utils.classes.ani_auto_task import AniAutoTask, CGesture, TPeAction


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
def get_gesture_duration(gesture: CGesture, buffer: int = 3) -> int:
    base_duration = gesture_durations.get(gesture, 70)  # default 70 se não encontrar
    return base_duration + buffer