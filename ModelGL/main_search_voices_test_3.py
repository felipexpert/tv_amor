import asyncio
from pathlib import Path
import edge_tts
import os

# ===== Configuração =====
BASE_DIR = Path(__file__).resolve().parents[1]  # raiz do projeto
PASTA_DE_SAIDA = BASE_DIR / "teste_vozes"

async def listar_vozes_ptbr():
    """Lista todas as vozes disponíveis em pt-BR"""
    voices_data = await edge_tts.list_voices()
    voices_ptbr = [v for v in voices_data if v["Locale"] == "pt-BR"]
    print("\nVozes disponíveis em pt-BR:")
    for v in voices_ptbr:
        print(f"- {v['ShortName']} ({v['Gender']})")
    return voices_ptbr

def criar_ssml(texto, voz, pitch="+0Hz", style=None, styledegree=None):
    """Cria SSML com pitch e style sem alterar o rate"""
    synth_ns = "http://www.w3.org/2001/10/synthesis"
    mstts_ns = "http://www.w3.org/2001/mstts"

    style_attr = f' style="{style}"' if style else ""
    styledegree_attr = f' styledegree="{styledegree}"' if styledegree else ""

    inner_text = f'<prosody pitch="{pitch}">{texto}</prosody>'

    if style:
        inner_text = f'<mstts:express-as{style_attr}{styledegree_attr}>{inner_text}</mstts:express-as>'

    ssml = f'''<speak version="1.0" xmlns="{synth_ns}" xmlns:mstts="{mstts_ns}" xml:lang="pt-BR">
  <voice name="{voz}">
    {inner_text}
  </voice>
</speak>'''
    return ssml

async def narrar_texto(texto, voz="pt-BR-FranciscaNeural", pitch="+0Hz", style=None, styledegree=None):
    """Narra um texto com SSML para aplicar pitch e style"""
    if not PASTA_DE_SAIDA.exists():
        os.makedirs(PASTA_DE_SAIDA)

    ssml = criar_ssml(texto, voz, pitch=pitch, style=style, styledegree=styledegree)
    arquivo_saida = PASTA_DE_SAIDA / "saida.wav"

    comunicador = edge_tts.Communicate(ssml=ssml,voice=voz)
    await comunicador.save(str(arquivo_saida))
    print(f"Áudio salvo em: {arquivo_saida}")

async def main():
    # 1 - Listar vozes pt-BR
    await listar_vozes_ptbr()

    # 2 - Texto para narrar
    texto_para_narrar = "Olá, cliente especial da Melhores Ofertas. Esperamos que seu dia esteja maravilhoso!"

    # 3 - Narrar com style "cheerful" e pitch levemente mais agudo
    await narrar_texto(
        texto_para_narrar,
        voz="pt-BR-FranciscaNeural",
        pitch="+3Hz",
        style="cheerful",
        styledegree="1.2"
    )

if __name__ == "__main__":
    asyncio.run(main())
