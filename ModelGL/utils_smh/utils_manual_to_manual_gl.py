from pathlib import Path
from typing import List, Optional, Tuple
from utils.utils_print import print_alt
from utils_smh.classes.config_smh import ConfigSmh, Profile
from utils_smh.classes.manual import Manual, ManualAccount, ManualAction, ManualWork, Store
from utils_smh.classes.manual_for_gui import ChromeProfile, ManualForGUI, SocialNetwork, SocialNetworkWorks, Work

from utils_smh.classes.manual_savior import ManualGL
from utils_smh.utils_config_smh import get_profile
from utils_smh.utils_paths_config import Paths
from returns.maybe import Maybe

def subtract_works_done(m: ManualForGUI, ms: ManualGL):
    for cp in m.chromeProfiles:
        snworks:SocialNetworkWorks = cp.socialNetworks
        snworks_works = snworks.works
        # preciso percorrer a cópia da lista, porque vou tirar itens da lista
        # então utilizo `snworks_works[:]` para percorrer esta cópia
        for work in snworks_works[:]:
            if is_work_already_done(work, ms):
                snworks_works.remove(work)
            

def is_work_already_done(w:Work, ms:ManualGL) -> bool:
    pass

def ensure_official_chrome_profile(cprofs: List[ChromeProfile]) -> ChromeProfile:
    cprof_official:ChromeProfile = None
    for cprof in cprofs:
        cprof:ChromeProfile = cprof
        if cprof.storeId is None:
            cprof_official = cprof
            break
    if cprof_official is None:
        # inicializa sem trabalhos
        cprof_official:ChromeProfile = ChromeProfile(storeId=None,chromeProfile="Default",socialNetworks=[])
        # adiciona à lista que veio
        cprofs.append(cprof_official)
    return cprof_official        
        

def manual_to_manual_for_gui(m: Manual, config:ConfigSmh) -> ManualForGUI:
    debug = False
    chrome_profiles: List[ChromeProfile] = []
    snworks_official_from_others:List[List[SocialNetworkWorks]] = []
    for acc in m.mAccounts:
        store_opt:Optional[Store] = acc.maAutoShareStoreOpt
        # if not store_opt: continue # por enquanto pula oficial, porque nao tem
        # novo profile
        store_id_opt:Optional[int] = store_opt.id if store_opt else None
        profile_maybe:Maybe[Profile] = get_profile(config, store_id_opt)
        # print_alt("profile_maybe", profile_maybe)
        # if profile_maybe.is_some():
        if profile_maybe:
            p:Profile = profile_maybe.unwrap()
            (list_snworks,list_snworks_official) = list_of_social_network_works(acc)
            if debug: print("list_snworks", list_snworks)
            chrome_profile:ChromeProfile = ChromeProfile(storeId=store_id_opt, chromeProfile=p.chromeProfile, socialNetworks=list_snworks)
            chrome_profiles.append(chrome_profile)
            snworks_official_from_others.append(list_snworks_official)
        else:
            print_alt("ATENÇÃO: Não encontramos um profile para store_id_opt={store_id_opt}, em config_smh.json")
    chrome_profile_official = ensure_official_chrome_profile(chrome_profiles)
    add_snworks_to_chrome_profile(chrome_profile_official, snworks_official_from_others)
    manual_gl:ManualForGUI = ManualForGUI(chromeProfiles=chrome_profiles)
    return manual_gl

def add_snworks_to_chrome_profile(cprof: ChromeProfile, snworks_list_list: List[List[SocialNetworkWorks]]):
    # adiciona ou cria caso nao tenha
    for snworks_list in snworks_list_list:
        for snworks in snworks_list:
            snworks2_opt = get_social_network_works_opt(cprof, snworks.socialNetwork)
            if snworks2_opt:
                # insere no começo, sem criar outra lista
                snworks2_opt.works[:0] = snworks.works
            else:
                cprof.socialNetworks.append(snworks)
        

def get_social_network_works_opt(cprof: ChromeProfile, sn:SocialNetwork) -> Optional[SocialNetworkWorks]:
    for snworks in cprof.socialNetworks:
        if snworks.socialNetwork == sn:
            return snworks
    return None
        
# Continua daqui - estou adicionando os oficiais que vem junto com os de cliente, para
# depois concatenar com as atividades do profile do chrome para oficiais
# comita antes de continuar, se ficar confuso, fica fácil de recuperar este ponto
def list_of_social_network_works(acc: ManualAccount) -> Tuple[List[SocialNetworkWorks], List[SocialNetworkWorks]]:
    ig_works_works:List[Work] = []
    tk_works_works:List[Work] = []
    ig_works_works_official:List[Work] = []
    tk_works_works_official:List[Work] = []
    for manual_work in acc.maWorks:
        if manual_work.mwActionIgOpt:
            (w, w_official_opt) = create_work(manual_work, manual_work.mwActionIgOpt)
            ig_works_works.append(w)
            if w_official_opt: ig_works_works_official.append(w_official_opt)
        if manual_work.mwActionTkOpt:
            (w,w_official_opt) = create_work(manual_work, manual_work.mwActionTkOpt)
            tk_works_works.append(w)
            if w_official_opt: tk_works_works_official.append(w_official_opt)
    # Eu precisei criar o `SocialNetworkWorks` depois do `ig_works_works` pronto,
    # com os itens, porque ele não mantém o ponteiro, ele faz uma cópia dos itens
    # da lista
    ig_works:SocialNetworkWorks = SocialNetworkWorks(socialNetwork=SocialNetwork.SNInstagram, works=ig_works_works)
    tk_works:SocialNetworkWorks = SocialNetworkWorks(socialNetwork=SocialNetwork.SNTiktok, works=tk_works_works)

    ig_works_official:SocialNetworkWorks = SocialNetworkWorks(socialNetwork=SocialNetwork.SNInstagram, works=ig_works_works_official)
    tk_works_official:SocialNetworkWorks = SocialNetworkWorks(socialNetwork=SocialNetwork.SNTiktok, works=tk_works_works_official)
    
    return ([ig_works, tk_works], [ig_works_official, tk_works_official])

def create_work(manual_work:ManualWork, manual_action:ManualAction) -> Tuple[Work, Optional[Work]]:
    msg_opt = manual_action.maMessageOpt
    smh_id = manual_action.maAccountOpt.maaSmhId if manual_action.maAccountOpt else manual_work.mwSmh.id
    w:Work = create_work2(smh_id, msg_opt, manual_work.mwFileCode)
    smh_id_official_opt:Optional[int] = manual_action.maAccountOfficialOpt.maaSmhId if manual_action.maAccountOfficialOpt else None
    w_official_opt:Work = create_work2(smh_id_official_opt, msg_opt, manual_work.mwFileCode) if smh_id_official_opt else None
    return (w,w_official_opt)

def create_work2(smh_id:int, msg_opt:Optional[str], file_code) -> Work:
    media_path:str = str(Path(Paths.SMH_WORKING_DIR) / Path(file_code))
    work:Work = Work(smhId=smh_id, messageOpt=msg_opt, mediaPath=media_path)
    return work

def media_path(filecode:str) -> str:
    return str(Path(Paths.SMH_WORKING_DIR) / Path(filecode))
