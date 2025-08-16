from typing import List, Optional
from utils_smh.classes import manual_savior
from utils_smh.classes.manual_for_gui import ChromeProfile, ManualForGUI, SocialNetworkWorks, Work
from utils_smh.classes.manual_savior import ManualGL, ShareMediaHistoryWork
from utils_smh.classes.social_network import SocialNetwork


def filter_manual_done(mgl:ManualForGUI, ms:ManualGL):
    for cprof in mgl.chromeProfiles[:]:
        cprof:List[ChromeProfile] = cprof
        for snwork in cprof.socialNetworks[:]:
            snwork:SocialNetworkWorks = snwork
            sn:SocialNetwork = snwork.socialNetwork
            for w in snwork.works[:]:
                w:Work = w
                smh_id:int = w.smhId
                smh_work_opt:Optional[ShareMediaHistoryWork] = find_manual_savior_opt_by_smh_id(ms, smh_id)
                if smh_work_opt:
                    smh_work:ShareMediaHistoryWork = smh_work_opt
                    if check_social_network_in_smh_work(smh_work, sn):
                        # já fez
                        snwork.works.remove(w)
            if len(snwork.works) == 0:
                cprof.socialNetworks.remove(snwork)
        if len(cprof.socialNetworks) == 0:
            mgl.chromeProfiles.remove(cprof)
                

def check_social_network_in_smh_work(smh_work:ShareMediaHistoryWork, sn: SocialNetwork) -> bool:
    for sn2 in smh_work.wDones:
        if sn == sn2: return True
    return False

def find_manual_savior_opt_by_smh_id(ms:ManualGL, smh_id:int) -> Optional[ShareMediaHistoryWork]:
    for smh_work in ms.mglWorks:
        smh_work: ShareMediaHistoryWork
        if smh_work.wSmhId == smh_id:
            return smh_work
    return None