from pydantic import BaseModel

from pathlib import Path


class ConfigSmh(BaseModel):
    workingDir: Path 