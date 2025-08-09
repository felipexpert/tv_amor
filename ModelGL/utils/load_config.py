import json
from pathlib import Path
from utils.paths import Paths
from utils.config import Config


def load_config() -> Config:
    # root_dir = Path(__file__).resolve().parents[1]  # diretório pai de ModelGL
    # config_path = root_dir / "config.json"
    config_path_str = Paths.CONFIG_JSON_PATH
    config_path = Path(config_path_str)
    with open(config_path, "r", encoding="utf-8") as f:
        configDict = json.load(f)
        Config(**configDict)