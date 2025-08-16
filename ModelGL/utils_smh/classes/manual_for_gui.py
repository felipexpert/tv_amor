from enum import Enum
from typing import List, Optional
from pydantic import BaseModel

class Work(BaseModel):
    smhId: int
    messageOpt: Optional[str]
    mediaPath:str

class SocialNetwork(Enum):
    SNWhatsAppGroup = "SNWhatsAppGroup"
    SNFacebook = "SNFacebook"
    SNInstagram = "SNInstagram"
    SNTiktok = "SNTiktok"
    SNYouTube = "SNYouTube"

class SocialNetworkWorks(BaseModel):
    socialNetwork:SocialNetwork
    works:List[Work]

class ChromeProfile(BaseModel):
    storeId:Optional[int]
    chromeProfile:str
    socialNetworks:List[SocialNetworkWorks]

class ManualForGUI(BaseModel):
    chromeProfiles:List[ChromeProfile]
