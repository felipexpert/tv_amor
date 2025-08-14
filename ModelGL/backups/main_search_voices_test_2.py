import asyncio
from pathlib import Path
import edge_tts

# Define o nome da pasta onde os arquivos de áudio serão salvos
BASE_DIR = Path(__file__).resolve().parents[1] # raiz do projeto
PASTA_DE_SAIDA = str(BASE_DIR / Path("teste_vozes"))

async def listar_vozes_ptbr():
    """Lista todas as vozes disponíveis em pt-BR"""
    voices_data = await edge_tts.list_voices()
    voices_ptbr = [v for v in voices_data if v["Locale"] == "pt-BR"]
    print("\nVozes disponíveis em pt-BR:")
    for v in voices_ptbr:
        print(f"- {v['ShortName']} ({v['Gender']})")
    return voices_ptbr

async def narrar_texto(texto, voz="pt-BR-FranciscaNeural"):

    arquivo_saida = str(Path(PASTA_DE_SAIDA ) / Path("saida.wav"))

    """Narra um texto com a voz e configurações definidas"""
    comunicador = edge_tts.Communicate(
        texto,
        voz,
        rate="-50%",
        pitch="-18Hz"
    )
    await comunicador.save(arquivo_saida)
    print(f"Áudio salvo em: {arquivo_saida}")

async def main():
    # 1 - Listar vozes pt-BR
    await listar_vozes_ptbr()

    # 2 - Texto para narrar
    texto_para_narrar = "Olá, cliente especial da Melhores Ofertas. Esperamos que seu dia esteja maravilhoso!"
    
    # 3 - Narrar
    await narrar_texto(texto_para_narrar)

if __name__ == "__main__":
    asyncio.run(main())
