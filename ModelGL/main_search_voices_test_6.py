import asyncio
from pathlib import Path
import edge_tts
import os

# Define a pasta para salvar os arquivos
BASE_DIR = Path(__file__).resolve().parents[1]  # raiz do projeto
PASTA_DE_SAIDA = BASE_DIR / "teste_vozes"

def _construir_ssml_completo(texto: str, nome_voz: str, pitch: str = None, style: str = None) -> str:
    """
    Constrói a string SSML completa e garante que o pitch e o style sejam incluídos
    corretamente dentro da estrutura.
    """
    lang = nome_voz.split('-')[0] + '-' + nome_voz.split('-')[1]
    
    # Começa com a tag <speak>
    ssml = f'<speak version="1.0" xmlns="http://www.w3.org/2001/10/synthesis" ' \
           f'xmlns:mstts="http://www.w3.org/2001/mstts" xml:lang="{lang}">'
    
    # Adiciona a variação de style se existir
    if style:
        ssml += f'<mstts:express-as style="{style}">'
    
    # Adiciona a variação de pitch se existir
    if pitch:
        ssml += f'<prosody pitch="{pitch}">{texto}</prosody>'
    else:
        ssml += texto

    # Fecha as tags se elas foram abertas
    if style:
        ssml += f'</mstts:express-as>'
    
    # Fecha a tag <speak>
    ssml += '</speak>'
    
    return ssml

async def gerar_audio_com_variacoes(texto: str, nome_voz: str, pitch: str = None, style: str = None):
    """
    Gera um arquivo de áudio com variações de pitch e style.
    """
    print(f"Gerando áudio para a voz '{nome_voz}'...")

    try:
        if not os.path.exists(PASTA_DE_SAIDA):
            os.makedirs(PASTA_DE_SAIDA)
        
        # Constrói o SSML completo e passa para a biblioteca
        ssml_final = _construir_ssml_completo(texto, nome_voz, pitch, style)

        # O Communicate agora recebe o SSML final e não precisa mais dos parâmetros de pitch ou style
        communicate = edge_tts.Communicate(ssml_final, nome_voz)

        nome_arquivo = f"{nome_voz.replace('pt-BR-', '')}_pitch{pitch or 'default'}_style{style or 'default'}.wav"
        caminho_completo = os.path.join(PASTA_DE_SAIDA, nome_arquivo)

        await communicate.save(caminho_completo)

        print(f"Áudio gerado com sucesso! Arquivo salvo em '{caminho_completo}'.")
    
    except Exception as e:
        print(f"Ocorreu um erro ao gerar o áudio: {e}")

async def main():
    texto_simples = "Olá! Esta é uma demonstração de como aplicar variações de pitch e estilo."
    
    await gerar_audio_com_variacoes(texto_simples, "pt-BR-AntonioNeural", pitch="+20Hz")

    await gerar_audio_com_variacoes(texto_simples, "pt-BR-AntonioNeural", style="cheerful")

    await gerar_audio_com_variacoes(texto_simples, "pt-BR-AntonioNeural", pitch="-10Hz", style="sad")

if __name__ == "__main__":
    asyncio.run(main())