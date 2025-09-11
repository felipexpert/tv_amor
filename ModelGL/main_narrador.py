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
Sol Ração Vaca de Leite: para cada 7 litros de leite produzido, apenas 1 quilo de Sol Ração Líquida já faz a diferença. Com tecnologia exclusiva, garante absorção imediata, sem perdas. O resultado é mais produtividade, melhor aproveitamento do pasto e manejo muito mais prático, aumenta a digestão do volumoso. Mais leite, mais resultado e menor custo por litro produzido, custo-benefício favorável. Conheça a Sol Ração Líquida – Inovação em Nutrição Animal
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