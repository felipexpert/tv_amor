
# ===== AAction =====
from typing import List, Literal, Union
from pydantic import BaseModel


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