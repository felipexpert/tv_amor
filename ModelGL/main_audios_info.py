import sys
import io
import json
import asyncio
from typing import List
from pathlib import Path

from pydub import AudioSegment
import edge_tts

from utils.audios_info import AudioRequest, AudioInfo
from utils.utils import print_alt
from pydantic import BaseModel
from utils.load_config import load_config
from utils.config import Config


async def gerar_audio(texto: str, caminho_wav: Path, voz: str) -> int:
    """
    Gera o áudio em .wav com a voz fornecida.
    Salva no caminho especificado e retorna a duração em milissegundos.
    """
    caminho_temp_mp3 = caminho_wav.with_suffix(".temp.mp3")

    # Gera MP3 temporário com edge-tts
    communicate = edge_tts.Communicate(texto, voice=voz)
    with open(caminho_temp_mp3, "wb") as f:
        async for chunk in communicate.stream():
            if chunk["type"] == "audio":
                f.write(chunk["data"])

    # Converte MP3 para WAV
    audio = AudioSegment.from_file(caminho_temp_mp3, format="mp3")
    audio.export(caminho_wav, format="wav")

    # Remove arquivo temporário
    caminho_temp_mp3.unlink(missing_ok=True)

    return int(audio.duration_seconds * 1000)


async def processar_audio_requests(requests: List[AudioRequest], working_dir: Path) -> List[AudioInfo]:
    working_dir.mkdir(parents=True, exist_ok=True)
    audio_infos: List[AudioInfo] = []

    for i, req in enumerate(requests):
        texto: str = req.arText
        voz: str = req.arConfig.arcVoice
        # voz = req.arConfig.get("arcVoice", "pt-BR-AntonioNeural")

        nome_arquivo = f"{i+1:05}.wav"
        caminho_arquivo = working_dir / nome_arquivo

        print_alt(f"🎤 Gerando {nome_arquivo} com voz {voz}...")

        duracao_ms = await gerar_audio(texto, caminho_arquivo, voz)

        audio_info = AudioInfo(
            aiFilePath=str(caminho_arquivo),
            aiDuration=duracao_ms,
            aiText=texto
        )
        audio_infos.append(audio_info)

    return audio_infos


async def main_async():

    try:
        entrada_json = json.load(sys.stdin)
        audio_requests = [AudioRequest(**item) for item in entrada_json]

        print_alt("🔧 Carregando configuração...")
        config = load_config()
        working_dir = Path(config.workingDir)

        print_alt(f"📂 Diretório de trabalho: {working_dir.resolve()}")

        print_alt("audio_requests", audio_requests)

        audio_infos = await processar_audio_requests(audio_requests, working_dir)

        # json.dump([info.model_dump() for info in audio_infos], sys.stdout, ensure_ascii=False)
        
        # Serializar a saída como JSON e imprimir no stdout
        json.dump([info.model_dump() for info in audio_infos], sys.stdout)

    except Exception as e:
        print_alt(f"❌ Erro: {e}")
        sys.exit(1)


def main():
    sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding="utf-8")
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding="utf-8")
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding="utf-8")
    
    asyncio.run(main_async())

async def main_async_bkp():
    audio_request_list = [
        AudioRequest(arText="Olá, humanos!", arConfig={"arcVoice": "pt-BR-AntonioNeural"}),
        AudioRequest(arText="Tudo bem?!", arConfig={"arcVoice": "pt-BR-AntonioNeural"})
    ]
    config = load_config()
    audio_infos = await processar_audio_requests(audio_request_list, config.workingDir)
    print_alt("🎧 Áudios gerados com sucesso!")
    print_alt(audio_infos)

if __name__ == "__main__":
    main()
