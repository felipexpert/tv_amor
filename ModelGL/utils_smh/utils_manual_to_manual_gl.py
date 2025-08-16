from pathlib import Path
from typing import List, Optional
from utils.utils_print import print_alt
from utils_smh.classes.config_smh import ConfigSmh, Profile
from utils_smh.classes.manual import Manual, ManualAction, Store
from utils_smh.classes.manual_gl import ChromeProfile, ManualGL, SocialNetwork, SocialNetworkWorks, Work
from collections import defaultdict

from utils_smh.utils_config_smh import get_profile
from utils_smh.utils_paths_config import Paths
from returns.maybe import Maybe

def manual_to_manual_gl(m: Manual, config:ConfigSmh) -> ManualGL:
    chrome_profiles: list[ChromeProfile] = []
    for acc in m.mAccounts:
        store_opt:Optional[Store] = acc.maAutoShareStoreOpt
        if not store_opt: continue # por enquanto pula oficial, porque nao tem
        # novo profile
        store_id_opt:Optional[int] = store_opt.id if store_opt else None
        profile_maybe:Maybe[Profile] = get_profile(config, store_id_opt)
        if profile_maybe.is_some():
            p:Profile = profile_maybe.unwrap()
            list_snworks:List[SocialNetworkWorks] = list_of_social_network_works()
            chrome_profile:ChromeProfile = ChromeProfile(store_id_opt, p.chromeProfile, list_snworks)
            chrome_profiles.append(chrome_profile)
        else:
            print_alt("ATENÇÃO: Não encontramos um profile para store_id_opt={store_id_opt}, em config_smh.json")
    manual_gl:ManualGL = ManualGL(chrome_profiles)
    return manual_gl
        

def list_of_social_network_works() -> List[SocialNetworkWorks]:
    pass

def manual_to_manual_gl_bkp(m: Manual) -> ManualGL:
    chrome_profiles: list[ChromeProfile] = []

    for account in m.mAccounts:
        redes_dict: dict[SocialNetwork, list[Work]] = defaultdict(list)
        pendentes_dict: dict[SocialNetwork, list[Work]] = defaultdict(list)

        for work in account.maWorks:
            def processar_acao(
                action: Optional[ManualAction],
                rede: SocialNetwork
            ):
                if not action:
                    return

                # Caso: ação do cliente (normal)
                if action.maAccountOpt:
                    redes_dict[rede].append(
                        Work(
                            smhId=action.maAccountOpt.maaSmhId,
                            messageOpt=action.maMessageOpt,
                            mediaPath=media_path(work.mwFileCode)
                        )
                    )

                # Caso: ação oficial
                if action.maAccountOfficialOpt:
                    # É cópia de outro SMH → vai para pendentes
                    pendentes_dict[rede].append(
                        Work(
                            smhId=action.maAccountOfficialOpt.maaSmhId,
                            messageOpt=action.maMessageOpt,
                            mediaPath=media_path(work.mwFileCode)
                        )
                    )

            # Processa Instagram e TikTok (nessa ordem)
            processar_acao(work.mwActionIgOpt, SocialNetwork.SNInstagram)
            processar_acao(work.mwActionTkOpt, SocialNetwork.SNTiktok)

        # No final da conta, insere os pendentes nas redes vazias ou concatena
        for rede, pendentes in pendentes_dict.items():
            if rede in redes_dict and redes_dict[rede]:
                # Já existe algo → concatena
                redes_dict[rede].extend(pendentes)
            else:
                # Rede estava vazia → usa apenas pendentes
                redes_dict[rede] = pendentes

        # Monta o ChromeProfile desta conta
        social_networks = [
            SocialNetworkWorks(socialNetwork=rede, works=works)
            for rede, works in redes_dict.items()
        ]
        chrome_profiles.append(ChromeProfile(socialNetworks=social_networks))

    return ManualGL(chromeProfiles=chrome_profiles)

def media_path(filecode:str) -> str:
    return str(Path(Paths.SMH_WORKING_DIR) / Path(filecode))
