from typing import List
from pydantic import BaseModel

from pathlib import Path

class Profile(BaseModel):
    storeId:str
    storeLabel:str
    chromeProfile:str

class ConfigSmh(BaseModel):
    workingDir: Path 
    profiles:List[Profile]