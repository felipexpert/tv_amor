from utils.utils_paths_config import Paths
from utils.classes.ani_auto_task import AniAutoTask
from utils.utils_print import print_alt


def load_ani_auto_task() -> AniAutoTask:
    debug = False
    
    import json
    from pathlib import Path

    # Exemplo: carregar o JSON gerado pelo Haskell
    
    json_path = Path(Paths.ANI_AUTO_TASK_JSON)
    if debug: print_alt("json_path", json_path)
    ani_auto_task_data = json.loads(json_path.read_text(encoding="utf-8"))

    return AniAutoTask(**ani_auto_task_data)
