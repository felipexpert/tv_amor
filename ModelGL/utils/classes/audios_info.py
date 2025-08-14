from pydantic import BaseModel, Field

class AudioInfo(BaseModel):
    aiFilePath: str
    aiDuration: int
    aiText: str

class AudioRequestConfig(BaseModel):
    """
    Configuração para a geração de áudio com edge_tts.
    """
    arcVoice: str = "pt-BR-AntonioNeural"
    arcPitch: str = "+0Hz"  # Exemplo: "+5Hz" para aumentar o tom
    arcRate: str = "+0%"    # Exemplo: "+10%" para acelerar
    arcTimbreScale: float = Field(default=1.0, ge=0.5, le=1.5) # Fator de variação do timbre (0.5 a 1.5)

class AudioRequest(BaseModel):
    arText: str
    arConfig: AudioRequestConfig