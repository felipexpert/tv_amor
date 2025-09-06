from pathlib import Path
import asyncio

from utils.utils_generate_audio import generate_audio
from utils.classes.audios_info import AudioInfo, AudioRequest, AudioRequestConfig


# Parte 3: Método main para testes
async def main_bkp():
    """
    Função principal para testar a geração de áudio.
    """
    print("Iniciando o teste de geração de áudio...")

    # Texto a ser convertido
    text_to_convert = "Olá! Este é um teste com variação de pitch, taxa de fala e timbre."

    # Exemplo 1: Áudio padrão
    config_padrao = AudioRequestConfig(
        arcVoice="pt-BR-AntonioNeural",
        arcPitch="+0Hz",
        arcRate="+0%",
        arcTimbreScale=1.0
    )

    base_dir = Path(__file__).resolve().parents[1]  # raiz do projeto
    pasta_de_saida = base_dir / "teste_vozes"

    file_padrao = pasta_de_saida / Path("audio_padrao.wav")
    print(f"Gerando áudio padrão em {file_padrao}...")
    result_padrao = await generate_audio(AudioRequest(arText=text_to_convert, arConfig=config_padrao), file_padrao)
    print(f"Áudio padrão gerado com sucesso. Duração: {result_padrao.aiDuration}ms")
    print("-" * 20)

    # Exemplo 2: Áudio com variações
    config_variada = AudioRequestConfig(
        arcVoice="pt-BR-FranciscaNeural",
        arcPitch="+0Hz",  # Aumenta o tom
        arcRate="+0%",     # Diminui a velocidade
        arcTimbreScale=1.0 # Varia o timbre
    )
    file_variado = pasta_de_saida / Path("audio_variado.wav")
    print(f"Gerando áudio variado em {file_variado}...")
    result_variado:AudioInfo = await generate_audio(AudioRequest(arText=text_to_convert, arConfig=config_variada), file_variado)
    print(f"Áudio variado gerado com sucesso. Duração: {result_variado.aiDuration}ms")

async def main():
    # Texto a ser convertido
    text_to_convert = """
Este é o futuro da nutrição animal: a Sol Ração Líquida.
Um suplemento inovador, de alta tecnologia, desenvolvido especialmente para garantir equilíbrio nutricional e saúde superior aos equinos.

Enquanto outros suplementos precisam de tempo para agir, a Sol Ração Líquida oferece absorção imediata, aproveitando cada nutriente de forma prática e segura.
Feita com aminoácidos, melaço de cana e soja, vitaminas e minerais, ela proporciona animais mais fortes, saudáveis e dóceis, com consumo balanceado para cada fase — desde potros até adultos.

Além de resultados consistentes, a Sol Ração Líquida é prática no manejo, econômica no consumo e estável em qualquer clima, sol ou chuva.

Para produtores que buscam produtividade, bem-estar animal e custo-benefício, a Sol Ração Líquida é a escolha certa.

Sol Ração Líquida – inovação em nutrição animal.
Visite-nos em Itapira ou fale pelo WhatsApp: (19) 9 8183-7243.
"""

    # Exemplo 1: Áudio padrão
    config_padrao = AudioRequestConfig(
        arcVoice="pt-BR-AntonioNeural",
        arcPitch="+0Hz",
        arcRate="+0%",
        arcTimbreScale=1.0
    )

    base_dir = Path(__file__).resolve().parents[1]  # raiz do projeto
    pasta_de_saida = base_dir / "teste_vozes_audio_de_trabalho"

    file_padrao = pasta_de_saida / Path("audio_padrao.wav")
    await generate_audio(AudioRequest(arText=text_to_convert, arConfig=config_padrao), file_padrao)
    print("Áudio gerado com sucesso.")

async def main_bkp():
    """
    Função principal para testar a geração de áudio.
    """
    print("Iniciando o teste de geração de áudio...")

    # Texto a ser convertido
    text_to_convert = "Olá! Este é um teste com variação de pitch, taxa de fala e timbre."

    # Exemplo 1: Áudio padrão
    config_padrao = AudioRequestConfig(
        arcVoice="pt-BR-AntonioNeural",
        arcPitch="+0Hz",
        arcRate="+0%",
        arcTimbreScale=1.0
    )

    base_dir = Path(__file__).resolve().parents[1]  # raiz do projeto
    pasta_de_saida = base_dir / "teste_vozes"

    file_padrao = pasta_de_saida / Path("audio_padrao.wav")
    print(f"Gerando áudio padrão em {file_padrao}...")
    result_padrao = await generate_audio(AudioRequest(arText=text_to_convert, arConfig=config_padrao), file_padrao)
    print(f"Áudio padrão gerado com sucesso. Duração: {result_padrao.aiDuration}ms")
    print("-" * 20)

    # Exemplo 2: Áudio com variações
    config_variada = AudioRequestConfig(
        arcVoice="pt-BR-AntonioNeural",
        arcPitch="-20Hz",  # Aumenta o tom
        arcRate="-5%",     # Diminui a velocidade
        arcTimbreScale=1.0 # Varia o timbre
    )
    file_variado = pasta_de_saida / Path("audio_variado.wav")
    print(f"Gerando áudio variado em {file_variado}...")
    result_variado:AudioInfo = await generate_audio(AudioRequest(arText=text_to_convert, arConfig=config_variada), file_variado)
    print(f"Áudio variado gerado com sucesso. Duração: {result_variado.aiDuration}ms")
    
if __name__ == "__main__":
    asyncio.run(main())