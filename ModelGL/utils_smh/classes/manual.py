from typing import List, Optional
from pydantic import BaseModel

class Store(BaseModel): 
    id:int

class ShareMediaHistory(BaseModel):
    id:int

class ManualActionAccount(BaseModel):
    # este campo é importante, porque a smh que origina a action, pode ser de outra
    # conta, portanto outra Smh, porque é gerado cópia nas contas oficiais, que 
    # vêm junto, portanto o correto é vincular à id da SMH correspondente aqui
    maaSmhId:int

class ManualAction(BaseModel):
    maMessageOpt:Optional[str]
    maAccountOpt:Optional[ManualActionAccount]
    maAccountOfficialOpt:Optional[ManualActionAccount]

class ManualWork(BaseModel):
    mwSmh: ShareMediaHistory
    mwFileCode:str

    mwActionWaOpt:Optional[ManualAction]
    mwActionFbOpt:Optional[ManualAction]
    mwActionIgOpt:Optional[ManualAction]
    mwActionTkOpt:Optional[ManualAction]

class ManualAccount(BaseModel):
    maAutoShareStoreOpt:Optional[Store]
    maWorks:List[ManualWork]

class Manual(BaseModel):
    mAccounts: List[ManualAccount]