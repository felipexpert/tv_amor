from pydantic import BaseModel

from pathlib import Path


class Config(BaseModel):
    workingDir: Path 
    backgroundDir: Path 
    musicsDir: Path 
    spritesDir: Path