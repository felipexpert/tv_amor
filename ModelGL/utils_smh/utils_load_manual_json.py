from utils.utils_print import print_alt
from utils_smh.classes.manual import Manual
from utils_smh.utils_paths_config import Paths


def load_manual() -> Manual:
    debug = False
    
    import json
    from pathlib import Path

    # Exemplo: carregar o JSON gerado pelo Haskell
    
    json_path = Path(Paths.SMH_AUTO_TASK_JSON)
    if debug: print_alt("json_path", json_path)
    manual_data = json.loads(json_path.read_text(encoding="utf-8"))

    return Manual(**manual_data["data"])
