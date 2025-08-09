import json
import sys

from pathlib import Path

from utils.config import load_config, Config

def print_err(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def print_alt(*args, **kwargs):
    """
    print_alt Print Alternative
    Serve para não jogar texto no stdout, que já está com a informação do json
    Por enquanto vamos jogar no stderr, depois podemos criar um log específico
    """
    # invocando print_err
    print_err(*args, **kwargs)

# def load_config() -> Config:
#     return Config(workingDir="C:\\tv_amor_working_dir")

def load_config() -> Config:
    return load_config()
    # root_dir = Path(__file__).resolve().parents[1]  # diretório pai de ModelGL
    # config_path = root_dir / "config.json"
    # config_path = Path("C:\\tv_amor_working_dir")
    # with open(config_path, "r", encoding="utf-8") as f:
    #     configDict = json.load(f)
    #     Config(**configDict)