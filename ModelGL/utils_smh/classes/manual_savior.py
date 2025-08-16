
from typing import List
from pydantic import BaseModel

from utils_smh.classes.social_network import SocialNetwork

class ShareMediaHistoryWork(BaseModel):
    wSmhId: int
    wDones: List[SocialNetwork]

class ManualGL(BaseModel):
    mglWorks: List[ShareMediaHistoryWork]