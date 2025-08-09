from pydantic import BaseModel, Field
from typing import List, Literal, Union

from utils.config import load_config, Config

from utils.ani_auto_task import AniAutoTask

if __name__ == "__main__":
    import json
    from pathlib import Path

    c: Config = load_config()
    workingDir: Path = c.workingDir

    # Exemplo: carregar o JSON gerado pelo Haskell
    # json_path = Path("ani_auto_task.json")
    json_path = workingDir
    ani_auto_task_data = json.loads(json_path.read_text(encoding="utf-8"))

    ani_task = AniAutoTask(**ani_auto_task_data)

    print(ani_task)
    print("Background:", ani_task.aatBackgroundImage)
    print("Total Duration:", ani_task.aatTotalDuration)
    for action in ani_task.aatActions:
        print("Number:", action.tpaNumber, "| Action:", action.tpaAction)
