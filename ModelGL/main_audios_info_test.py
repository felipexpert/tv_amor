import sys
import json
from typing import List

from utils.audios_info import AudioInfo, AudioRequest

def main_2():
    # print("Iniciando processamento de áudio...")
    # entrada = json.load(sys.stdin)
    # print("Entrada recebida:", entrada)
    # print('{"status": "ok", "message": "Teste bem-sucedido"}')
    jsonTest = json.loads('[{ "aiFilePath": "/caminho/novo", "aiDuration": 300, "aiText": "Testando funcionalidade..." }]')
    json.dump(jsonTest, sys.stdout)

def audio_request_to_audio_info(audio_request: AudioRequest) -> AudioInfo:
    audioInfo = AudioInfo(
        aiFilePath='/path/to/audio/file',
        aiDuration=1000,
        aiText=audio_request.arText
    )
    return audioInfo

def main():
    entrada = json.load(sys.stdin) 
    lista: List[AudioRequest] = [AudioRequest(**item) for item in entrada]
    audio_infos_test = list(map(audio_request_to_audio_info, lista))
    
    # print(audio_infos_test)  # Print the JSON representation of the audio_infos_test
    
    # Serializar a saída como JSON e imprimir no stdout
    json.dump([info.model_dump() for info in audio_infos_test], sys.stdout)



if __name__ == "__main__":
    main()