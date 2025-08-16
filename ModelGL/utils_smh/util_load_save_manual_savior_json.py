
from typing import Optional
from utils.utils_print import print_alt
from utils_smh.classes.manual_savior import ManualGL, ShareMediaHistoryWork
from utils_smh.classes.social_network import SocialNetwork
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
    # esse `mode="json"` é importante! Sem ele a Enum `SocialNetwork` estava 
    # dando problema
    manual_data = m.model_dump(mode="json")  # se estiver no Pydantic v2
    # manual_data = {"data": m.dict()}      # se estiver no Pydantic v1

    json_text = json.dumps(manual_data, indent=2, ensure_ascii=False)

    json_path.write_text(json_text, encoding="utf-8")

def save_manual_savior_work(manual_savior:ManualGL, smh_id:int, sn:SocialNetwork):
    # 1º verifica se já tem o ShareMediaHistoryWork
    smh_work_opt:Optional[ShareMediaHistoryWork] = None
    for smh_work in manual_savior.mglWorks:
        if smh_work.wSmhId == smh_id:
            smh_work_opt = smh_work
            break
    if smh_work_opt:
        # se tem, verficia se tem a rede social, e se não tiver adiciona
        smh_work:ShareMediaHistoryWork = smh_work_opt
        if not smh_work_contains_sn(smh_work, sn):
            smh_work.wDones.append(sn)
            # salva no arquivo json!
            save_manual_savior(manual_savior)
    else:
        # cria e adiciona
        smh_work_new:ShareMediaHistoryWork = ShareMediaHistoryWork(wSmhId=smh_id, wDones=[sn])
        manual_savior.mglWorks.append(smh_work_new)

        # salva no arquivo json!
        save_manual_savior(manual_savior)
    

def smh_work_contains_sn(w:ShareMediaHistoryWork, sn: SocialNetwork) -> bool:
    for sn2 in w.wDones:
        if sn2 == sn: return True
    return False