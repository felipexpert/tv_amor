from pydantic import BaseModel

class AudioInfo(BaseModel):
    aiFilePath: str
    aiDuration: int
    aiText: str

class AudioRequestConfig(BaseModel):
    arcVoice: str

class AudioRequest(BaseModel):
    arText: str
    arConfig: AudioRequestConfig