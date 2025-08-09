from pydantic import BaseModel

from pathlib import Path

class Config(BaseModel):
    workingDir: Path 
    
# def load_config() -> Config:
#     return Config(workingDir="C:\\tv_amor_working_dir")

def load_config() -> Config:
    # root_dir = Path(__file__).resolve().parents[1]  # diretório pai de ModelGL
    # config_path = root_dir / "config.json"
    config_path = Path("C:\\tv_amor_working_dir")
    with open(config_path, "r", encoding="utf-8") as f:
        configDict = json.load(f)
        Config(**configDict)