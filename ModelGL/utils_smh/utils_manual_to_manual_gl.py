from pathlib import Path
from typing import List, Optional
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

def manual_to_manual_for_gui(m: Manual, config:ConfigSmh) -> ManualForGUI:
    debug = False
    chrome_profiles: list[ChromeProfile] = []
    for acc in m.mAccounts:
        store_opt:Optional[Store] = acc.maAutoShareStoreOpt
        if not store_opt: continue # por enquanto pula oficial, porque nao tem
        # novo profile
        store_id_opt:Optional[int] = store_opt.id if store_opt else None
        profile_maybe:Maybe[Profile] = get_profile(config, store_id_opt)
        # print_alt("profile_maybe", profile_maybe)
        # if profile_maybe.is_some():
        if profile_maybe:
            p:Profile = profile_maybe.unwrap()
            list_snworks:List[SocialNetworkWorks] = list_of_social_network_works(acc)
            if debug: print("list_snworks", list_snworks)
            chrome_profile:ChromeProfile = ChromeProfile(storeId=store_id_opt, chromeProfile=p.chromeProfile, socialNetworks=list_snworks)
            chrome_profiles.append(chrome_profile)
        else:
            print_alt("ATENÇÃO: Não encontramos um profile para store_id_opt={store_id_opt}, em config_smh.json")
    manual_gl:ManualForGUI = ManualForGUI(chromeProfiles=chrome_profiles)
    return manual_gl
        

def list_of_social_network_works(acc: ManualAccount) -> List[SocialNetworkWorks]:
    ig_works_works:List[Work] = []
    tk_works_works:List[Work] = []
    for manual_work in acc.maWorks:
        if manual_work.mwActionIgOpt:
            w = create_work(manual_work, manual_work.mwActionIgOpt)
            ig_works_works.append(w)
        if manual_work.mwActionTkOpt:
            w = create_work(manual_work, manual_work.mwActionTkOpt)
            tk_works_works.append(w)
    # Eu precisei criar o `SocialNetworkWorks` depois do `ig_works_works` pronto,
    # com os itens, porque ele não mantém o ponteiro, ele faz uma cópia dos itens
    # da lista
    ig_works:SocialNetworkWorks = SocialNetworkWorks(socialNetwork=SocialNetwork.SNInstagram, works=ig_works_works)
    tk_works:SocialNetworkWorks = SocialNetworkWorks(socialNetwork=SocialNetwork.SNTiktok, works=tk_works_works)
    return [ig_works, tk_works]

def create_work(manual_work:ManualWork, manual_action:ManualAction):
    msg_opt = manual_action.maMessageOpt
    smh_id = manual_action.maAccountOpt.maaSmhId if manual_action.maAccountOpt else manual_work.mwSmh.id
    w = create_work2(smh_id, msg_opt, manual_work.mwFileCode)
    return w

def create_work2(smh_id:int, msg_opt:Optional[str], file_code) -> Work:
    media_path:str = str(Path(Paths.BASE_DIR) / Path(file_code))
    work:Work = Work(smhId=smh_id, messageOpt=msg_opt, mediaPath=media_path)
    return work

def media_path(filecode:str) -> str:
    return str(Path(Paths.SMH_WORKING_DIR) / Path(filecode))
