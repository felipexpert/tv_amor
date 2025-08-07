# ModelGL/main_audios_info.py

import asyncio
import sys
import json
import uuid
import os
from pathlib import Path
from edge_tts import Communicate
from pydub import AudioSegment

from utils.utils import load_config

EXTENSAO = "wav"



async def gerar_audio(texto, nome_arquivo, voz="pt-BR-AntonioNeural"):
    communicate = Communicate(texto, voice=voz)
    with open(nome_arquivo, "wb") as f:
        async for chunk in communicate.stream():
            if chunk["type"] == "audio":
                f.write(chunk["data"])

async def processar_requisicoes(audio_requests, working_dir: Path):
    resultados = []

    working_dir.mkdir(parents=True, exist_ok=True)

    for i, req in enumerate(audio_requests):
        texto = req["arText"]
        voz = req["arConfig"]["arcVoice"]
        nome_arquivo = f"{i+1:05d}.{EXTENSAO}"
        caminho_completo = working_dir / nome_arquivo

        await gerar_audio(texto, caminho_completo, voz)

        audio = AudioSegment.from_file(caminho_completo)
        dur_ms = int(audio.duration_seconds * 1000)

        resultados.append({
            "aiFilePath": str(caminho_completo),
            "aiDuration": dur_ms
        })

    return resultados

async def main():
    input_json = sys.stdin.read()
    try:
        audio_requests = json.loads(input_json)

        config = load_config()
        working_dir = Path(config["workingDir"])

        resultado = await processar_requisicoes(audio_requests, working_dir)
        print(json.dumps(resultado))
    except Exception as e:
        print(json.dumps({"error": str(e)}))
        sys.exit(1)

if __name__ == "__main__":
    asyncio.run(main())
