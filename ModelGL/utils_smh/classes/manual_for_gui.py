from enum import Enum
from typing import List, Optional
from pydantic import BaseModel

from utils_smh.classes.social_network import SocialNetwork

class Work(BaseModel):
    smhId: int
    messageOpt: Optional[str]
    mediaPath:str

class SocialNetworkWorks(BaseModel):
    socialNetwork:SocialNetwork
    works:List[Work]

class ChromeProfile(BaseModel):
    storeId:Optional[int]
    chromeProfile:str
    socialNetworks:List[SocialNetworkWorks]

class ManualForGUI(BaseModel):
    chromeProfiles:List[ChromeProfile]
