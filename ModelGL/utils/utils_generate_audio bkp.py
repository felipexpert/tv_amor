from pathlib import Path
import edge_tts
import librosa
import soundfile as sf

from utils.classes.audios_info import AudioInfo, AudioRequest, AudioRequestConfig

# Função para gerar áudio
async def generate_audio(audio_request: AudioRequest, file_path: Path) -> AudioInfo:
    """
    Gera um arquivo de áudio a partir de um texto, com variações de voz,
    e ajusta o timbre utilizando librosa.
    """
    text:str = audio_request.arText
    voice_config:AudioRequestConfig = audio_request.arConfig
    # 1. Gerar o áudio base com edge_tts
    communicate = edge_tts.Communicate(
        text,
        voice_config.arcVoice,
        pitch=voice_config.arcPitch,
        rate=voice_config.arcRate
    )
    
    temp_path = file_path.with_suffix(".temp.wav")
    await communicate.save(temp_path)

    # 2. Variação do timbre com librosa
    y, sr = librosa.load(temp_path, sr=None)
    y_timbre_varied = librosa.effects.preemphasis(y, coef=voice_config.arcTimbreScale)
    
    # 3. Salvar o áudio final com as variações
    sf.write(file_path, y_timbre_varied, sr, format='WAV')

    # 4. Calcular a duração do áudio final
    duration_seconds = librosa.get_duration(y=y_timbre_varied, sr=sr)
    duration_millis = int(duration_seconds * 1000)

    # 5. Remover o arquivo temporário
    temp_path.unlink()

    return AudioInfo(aiFilePath=str(file_path), aiDuration=duration_millis, aiText=text)