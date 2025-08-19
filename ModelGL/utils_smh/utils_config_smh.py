from typing import List, Optional
from utils_smh.classes.config_smh import ConfigSmh, Profile
from returns.maybe import Maybe


def get_profile(config:ConfigSmh, store_id_opt:Optional[int]) -> Maybe[Profile]:
    profiles:List[Profile] = config.profiles
    profile_maybe:Maybe[Profile] = Maybe.empty
    for p in profiles:
        # se for número ele converte para texto e verifica, se for None
        # é o Profile "official", que é o da MO
        if p.storeId == (str(store_id_opt) if  store_id_opt else "official"):
            # profile_maybe = Maybe.from_optional(p)
            profile_maybe = Maybe.from_value(p)
            break
    return profile_maybe