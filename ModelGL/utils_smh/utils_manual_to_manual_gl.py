from pathlib import Path
from typing import Optional
from utils_smh.classes.manual import Manual, ManualAction
from utils_smh.classes.manual_gl import ChromeProfile, ManualGL, SocialNetwork, SocialNetworkWorks, Work
from collections import defaultdict

from utils_smh.utils_paths_config import Paths

def manual_to_manual_gl(m: Manual) -> ManualGL:
    chrome_profiles: list[ChromeProfile] = []

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
