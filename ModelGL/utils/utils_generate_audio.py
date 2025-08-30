from pathlib import Path
import edge_tts
import librosa
import soundfile as sf

from utils.classes.audios_info import AudioInfo, AudioRequest, AudioRequestConfig

from pydub import AudioSegment
from pydub.silence import detect_nonsilent

from utils.utils_print import print_alt

def cortar_silencio_final(audio: AudioSegment, margem_ms: int = 140) -> AudioSegment:
    """
    Remove silêncio no final do áudio, mantendo uma margem mínima.
    """
    non_silents = detect_nonsilent(
        audio,
        min_silence_len=100,          # mínimo de 100ms de silêncio
        silence_thresh=audio.dBFS-40  # considera silêncio bem abaixo da média
    )
    if not non_silents:
        return audio
    _, fim = non_silents[-1]
    return audio[:fim + margem_ms]

# Função para gerar áudio
async def generate_audio(audio_request: AudioRequest, file_path: Path) -> AudioInfo:
    """
    Gera um arquivo de áudio a partir de um texto, com variações de voz,
    corta silêncio extra no final e ajusta o timbre utilizando librosa.
    """
    text: str = audio_request.arText
    voice_config: AudioRequestConfig = audio_request.arConfig

    # Se tiver texto bugado, ele substitui por algo vazio
    
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

    # 3. Salvar temporário pós-timbre
    temp_processed = file_path.with_suffix(".processed.wav")
    sf.write(temp_processed, y_timbre_varied, sr, format='WAV')

    # 4. Carregar com pydub e cortar silêncio
    audio = AudioSegment.from_wav(temp_processed)
    audio = cortar_silencio_final(audio, margem_ms=140)
    audio.export(file_path, format="wav")

    # 5. Calcular a duração do áudio final
    duration_seconds = len(audio) / 1000.0
    duration_millis = int(duration_seconds * 1000)

    # 6. Remover arquivos temporários
    temp_path.unlink(missing_ok=True)
    temp_processed.unlink(missing_ok=True)

    return AudioInfo(aiFilePath=str(file_path), aiDuration=duration_millis, aiText=text)

# Função para gerar áudio
async def generate_audio_bkp(audio_request: AudioRequest, file_path: Path) -> AudioInfo:
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