from typing import List
from utils.classes.ani_auto_task import AniAutoTask, TPeAction


def get_aat_action_speechs(aat: AniAutoTask) -> List[TPeAction]:
    action_speechs:List[TPeAction] = []
    for action in aat.aatActions:
        action: TPeAction = action
        if action.tpaAction.tag == "ASpeech":
            action_speechs.append(action)
    return action_speechs