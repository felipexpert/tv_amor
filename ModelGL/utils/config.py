from pydantic import BaseModel

class Config(BaseModel):
    workingDir: str 
    
