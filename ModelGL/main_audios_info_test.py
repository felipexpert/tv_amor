import sys
import json

def main():
    # print("Iniciando processamento de áudio...")
    # entrada = json.load(sys.stdin)
    # print("Entrada recebida:", entrada)
    # print('{"status": "ok", "message": "Teste bem-sucedido"}')
    jsonTest = json.loads('[{ "aiFilePath": "/caminho/novo", "aiDuration": 300, "aiText": "Testando funcionalidade..." }]')
    json.dump(jsonTest, sys.stdout)

if __name__ == "__main__":
    main()