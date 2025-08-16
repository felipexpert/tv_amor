from enum import Enum
from typing import List, Optional
from anthropic import BaseModel

class Work(BaseModel):
    smhId: int
    messageOpt: Optional[str]
    mediaPath:str

class SocialNetwork(Enum):
    SNInstagram = "SNInstagram"
    SNTiktok = "SNTiktok"

class SocialNetworkWorks(BaseModel):
    socialNetwork:SocialNetwork
    works:List[Work]

class ChromeProfile(BaseModel):
    storeId:Optional[int]
    chromeProfile:str
    socialNetworks:List[SocialNetworkWorks]

class ManualGL(BaseModel):
    chromeProfiles:List[ChromeProfile]
