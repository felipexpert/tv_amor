from pydantic import BaseModel

from pathlib import Path

import json

from utils.paths import Paths

class Config(BaseModel):
    workingDir: Path 
    