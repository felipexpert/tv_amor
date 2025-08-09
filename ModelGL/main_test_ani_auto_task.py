from utils.classes.ani_auto_task import AniAutoTask
from utils.utils_paths_config import Paths
from utils.utils_print import print_alt


if __name__ == "__main__":
    debug = True
    
    import json
    from pathlib import Path

    # Exemplo: carregar o JSON gerado pelo Haskell
    
    json_path = Path(Paths.ANI_AUTO_TASK_JSON)
    if debug: print_alt("json_path", json_path)
    ani_auto_task_data = json.loads(json_path.read_text(encoding="utf-8"))

    ani_task = AniAutoTask(**ani_auto_task_data)

    print(ani_task)
    print("Background:", ani_task.aatBackgroundImage)
    print("Total Duration:", ani_task.aatTotalDuration)
    for action in ani_task.aatActions:
        print("Number:", action.tpaNumber, "| Action:", action.tpaAction)