import asyncio
from pathlib import Path
import edge_tts
import os

# Define o nome da pasta onde os arquivos de áudio serão salvos
BASE_DIR = Path(__file__).resolve().parents[1] # raiz do projeto
PASTA_DE_SAIDA = str(BASE_DIR / Path("teste_vozes"))

async def gerar_audio(texto: str, nome_voz: str):
    """
    Gera um arquivo de áudio no formato .wav a partir de um texto, usando uma voz específica,
    e salva na pasta 'teste_vozes'.

    Args:
        texto (str): O texto que será narrado.
        nome_voz (str): O nome da voz a ser utilizada (ex: 'pt-BR-AntonioNeural').
    """
    print(f"Gerando áudio com a voz '{nome_voz}'...")

    try:
        # Verifica se a pasta de saída existe. Se não, cria.
        if not os.path.exists(PASTA_DE_SAIDA):
            os.makedirs(PASTA_DE_SAIDA)
            print(f"Pasta '{PASTA_DE_SAIDA}' criada com sucesso.")
        
        # Cria a instância para a conversão de texto em fala
        communicate = edge_tts.Communicate(texto, nome_voz)
        
        # Define o caminho completo do arquivo de saída dentro da pasta
        nome_arquivo = f"{nome_voz}.wav"
        caminho_completo = os.path.join(PASTA_DE_SAIDA, nome_arquivo)
        
        # Gera e salva o arquivo de áudio no formato .wav
        await communicate.save(caminho_completo)
        
        print(f"Áudio gerado com sucesso! Arquivo salvo em '{caminho_completo}'.")
    
    except Exception as e:
        print(f"Ocorreu um erro ao gerar o áudio: {e}")

async def main():
    # Exemplo de uso da função:
    
    # Texto que você quer narrar
    texto_para_narrar = "Olá! Este é o teste de voz. Estou feliz em te ajudar a organizar seus arquivos."
    
    # Nome da voz que você deseja usar
    # voz_escolhida = "pt-BR-AntonioNeural"
    voz_escolhida = "pt-BR-ThiagoNeural"
    # voz_escolhida = "pt-BR-FranciscaNeural"
    # voz_escolhida = "pt-BR-ThalitaMultilingualNeural"
    
    await gerar_audio(texto_para_narrar, voz_escolhida)

if __name__ == "__main__":
    asyncio.run(main())