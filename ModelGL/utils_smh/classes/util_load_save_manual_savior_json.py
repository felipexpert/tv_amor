
from utils.utils_print import print_alt
from utils_smh.classes.manual_savior import ManualGL
from utils_smh.utils_paths_config import Paths


def load_manual_savior() -> ManualGL:
    debug = False
    
    import json
    from pathlib import Path

    # Exemplo: carregar o JSON gerado pelo Haskell
    
    json_path = Path(Paths.SMH_AUTO_TASK_JSON_SAVIOR)
    if json_path.exists():
        if debug: print_alt("json_path", json_path)
        manual_data = json.loads(json_path.read_text(encoding="utf-8"))
        return ManualGL(**manual_data)
    else:
        m:ManualGL = ManualGL(mglWorks=[])
        save_manual_savior(m)
        return m

def save_manual_savior(m: ManualGL):
    import json
    from pathlib import Path

    json_path = Path(Paths.SMH_AUTO_TASK_JSON_SAVIOR)

    # Monta a estrutura do JSON (mesma usada no load)
    manual_data = m.model_dump()  # se estiver no Pydantic v2
    # manual_data = {"data": m.dict()}      # se estiver no Pydantic v1

    json_text = json.dumps(manual_data, indent=2, ensure_ascii=False)

    json_path.write_text(json_text, encoding="utf-8")
