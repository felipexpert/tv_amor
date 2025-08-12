
# ===== AAction =====
from enum import Enum
from typing import List, Literal, Union
from pydantic import BaseModel


class ASpeech(BaseModel):
    tag: Literal["ASpeech"]
    asAudioWav: str
    asStartTime: int

# ===== EPeNumber =====
class CGesture(Enum):
    GHi = "GHi"
    GStandShort = "GStandShort"
    GStandLong = "GStandLong"
    GThinkShort = "GThinkShort"
    GThinkLong = "GThinkLong"
    GTalkShort = "GTalkShort"
    GTalkLong = "GTalkLong"
    GWorry = "GWorry"
    GShakeLeg = "GShakeLeg"
    GExcited = "GExcited"
    GDance = "GDance"

class AGesture(BaseModel):
    tag: Literal["AGesture"]
    agGesture: CGesture
    agStartTime: int


# Discriminated union para AAction
AAction = Union[ASpeech, AGesture]

# ===== EPeNumber =====
class EPeNumber(Enum):
    EPeNum1 = "EPeNum1"
    EPeNum2 = "EPeNum2"

# ===== TPeAction =====
class TPeAction(BaseModel):
    tpaNumber: EPeNumber
    tpaAction: AAction


# ===== AniAutoTask =====
class TPersona(BaseModel):
    pNumber: EPeNumber 
    pX: int
    pY: int

# ===== AniAutoTask =====
class AniAutoTask(BaseModel):
    aatActions: List[TPeAction]
    aatTotalDuration: int
    aatBackgroundImage: str
    aatPersonas: List[TPersona]