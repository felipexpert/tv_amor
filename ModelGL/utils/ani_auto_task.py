from pydantic import BaseModel, Field
from typing import List, Literal, Union

from utils.config import Config

from utils.load_config import load_config

# ===== AAction =====
class ASpeech(BaseModel):
    tag: Literal["ASpeech"]
    asAudioWav: str
    asStartTime: int


class AGesture(BaseModel):
    tag: Literal["AGesture"]
    agGesture: str
    agStartTime: int


# Discriminated union para AAction
AAction = Union[ASpeech, AGesture]


# ===== TPeAction =====
class TPeAction(BaseModel):
    tpaNumber: str  # Ex.: "EPeNum1"
    tpaAction: AAction


# ===== AniAutoTask =====
class AniAutoTask(BaseModel):
    aatActions: List[TPeAction]
    aatTotalDuration: int
    aatBackgroundImage: str


if __name__ == "__main__":
    import json
    from pathlib import Path

    c: Config = load_config()
    workingDir: Path = c.workingDir

    # Exemplo: carregar o JSON gerado pelo Haskell
    # json_path = Path("ani_auto_task.json")
    json_path = workingDir
    ani_auto_task_data = json.loads(json_path.read_text(encoding="utf-8"))

    ani_task = AniAutoTask(**ani_auto_task_data)

    print(ani_task)
    print("Background:", ani_task.aatBackgroundImage)
    print("Total Duration:", ani_task.aatTotalDuration)
    for action in ani_task.aatActions:
        print("Number:", action.tpaNumber, "| Action:", action.tpaAction)
