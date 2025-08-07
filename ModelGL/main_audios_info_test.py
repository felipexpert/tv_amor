import io
import sys
import json
from typing import List

from utils.audios_info import AudioInfo, AudioRequest

from utils.utils import print_err

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
    
    # Reconfigura sys.stderr para usar UTF-8
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')
    
    # Redefine sys.stdin para forçar UTF-8 (isso é seguro e eficaz)
    sys.stdin = io.TextIOWrapper(sys.stdin.buffer, encoding='utf-8')
    
    entrada = json.load(sys.stdin) 
    lista: List[AudioRequest] = [AudioRequest(**item) for item in entrada]
    audio_infos_test = list(map(audio_request_to_audio_info, lista))
    
    print_err("Parâmetros recebidos:", entrada)
    
    # Serializar a saída como JSON e imprimir no stdout
    json.dump([info.model_dump() for info in audio_infos_test], sys.stdout)



if __name__ == "__main__":
    main()