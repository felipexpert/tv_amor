from pydantic import BaseModel
from typing import List, Union

class CharLabel(BaseModel):
    value: str

class Wave(BaseModel):
    who: CharLabel

class Pause(BaseModel):
    duration: float

InlineCommand = Union[Wave, Pause]

class PlainText(BaseModel):
    text: str

class Command(BaseModel):
    cmd: InlineCommand

RichText = Union[PlainText, Command]

class DialogueBlock(BaseModel):
    speaker: CharLabel
    contents: List[RichText]

Episode = List[DialogueBlock]