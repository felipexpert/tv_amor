import asyncio
from pathlib import Path
import edge_tts
import os

# Define a pasta para salvar os arquivos
BASE_DIR = Path(__file__).resolve().parents[1]  # raiz do projeto
PASTA_DE_SAIDA = str(BASE_DIR / Path("teste_vozes"))

def _construir_ssml(texto: str, nome_voz: str, style: str = None) -> str:
    """
    Constrói a string completa em formato SSML.
    Isso garante que o edge-tts interprete as tags corretamente.
    """
    # Define o idioma do texto com base no nome da voz
    lang = nome_voz.split('-')[0] + '-' + nome_voz.split('-')[1]
    
    ssml = f'<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" ' \
           f'xmlns:mstts="http://www.w3.org/2001/mstts" xml:lang="{lang}">'
    
    if style:
        ssml += f'<mstts:express-as style="{style}">{texto}</mstts:express-as>'
    else:
        ssml += texto
        
    ssml += '</speak>'
    return ssml

async def gerar_audio_com_variacoes(texto: str, nome_voz: str, pitch: str = "+0Hz", style: str = None):
    """
    Gera um arquivo de áudio com variações de pitch e style, garantindo a interpretação correta.

    Args:
        texto (str): O texto que será narrado.
        nome_voz (str): O nome da voz a ser usada (ex: 'pt-BR-AntonioNeural').
        pitch (str, opcional): Variação no tom da voz (ex: "+50Hz").
        style (str, opcional): Estilo da voz (ex: "cheerful").
    """
    print(f"Gerando áudio com a voz '{nome_voz}'...")

    try:
        # Garante que a pasta de saída exista
        if not os.path.exists(PASTA_DE_SAIDA):
            os.makedirs(PASTA_DE_SAIDA)
        
        # Constrói o SSML completo para evitar que o código seja lido em voz alta
        texto_ssml = _construir_ssml(texto, nome_voz, style)

        # Passa o pitch diretamente na função Communicate
        communicate = edge_tts.Communicate(texto_ssml, nome_voz, pitch=pitch)

        # Define o nome do arquivo com base nos parâmetros
        nome_arquivo = f"{nome_voz.replace('pt-BR-', '')}_pitch{pitch or 'default'}_style{style or 'default'}.wav"
        caminho_completo = os.path.join(PASTA_DE_SAIDA, nome_arquivo)

        # Salva o arquivo de áudio
        await communicate.save(caminho_completo)

        print(f"Áudio gerado com sucesso! Arquivo salvo em '{caminho_completo}'.")
    
    except Exception as e:
        print(f"Ocorreu um erro ao gerar o áudio: {e}")

async def main():
    texto_simples = "Olá! Esta é uma demonstração de como aplicar variações de pitch e estilo."
    
    # Exemplo 1: Somente com pitch alterado
    await gerar_audio_com_variacoes(texto_simples, "pt-BR-AntonioNeural", pitch="+20Hz")

    # Exemplo 2: Somente com style alterado ('cheerful' para alegre)
    await gerar_audio_com_variacoes(texto_simples, "pt-BR-AntonioNeural", style="cheerful")

    # Exemplo 3: Com pitch e style alterados
    await gerar_audio_com_variacoes(texto_simples, "pt-BR-AntonioNeural", pitch="-10Hz", style="sad")

if __name__ == "__main__":
    asyncio.run(main())