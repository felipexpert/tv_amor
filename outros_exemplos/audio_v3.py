import re 
import asyncio
import edge_tts
from pydub import AudioSegment
import os

# Regex para achar comandos no texto
RE_COMMAND = re.compile(r"\[command:([a-zA-Z0-9_]+)\]")

async def gerar_audio(texto, nome_arquivo, voz="pt-BR-AntonioNeural"):
    communicate = edge_tts.Communicate(texto, voice=voz)
    with open(nome_arquivo, "wb") as f:
        async for chunk in communicate.stream():
            if chunk["type"] == "audio":
                f.write(chunk["data"])

def dividir_texto(texto):
    """
    Divide texto em lista de (segmento_texto, comando_apos_segmento)
    O último comando pode ser None
    """
    partes = RE_COMMAND.split(texto)
    # split retorna lista tipo: [texto0, comando1, texto1, comando2, texto2,...]
    segmentos = []
    for i in range(0, len(partes), 2):
        segmento = partes[i].strip()
        comando = partes[i+1] if (i+1) < len(partes) else None
        segmentos.append((segmento, comando))
    return segmentos

async def gerar_audios_segmentados(segmentos, voz="pt-BR-AntonioNeural"):
    arquivos = []
    tempos = []
    tempo_acumulado = 0.0

    for idx, (texto_seg, comando) in enumerate(segmentos):
        if not texto_seg:
            audio = AudioSegment.silent(duration=300)  # 300ms silêncio
            nome_arquivo = f"segmento_{idx}.mp3"
            audio.export(nome_arquivo, format="mp3")
            duracao = 0.3
        else:
            nome_arquivo = f"segmento_{idx}.mp3"
            print(f"Gerando áudio para segmento {idx}: '{texto_seg[:30]}...'")
            await gerar_audio(texto_seg, nome_arquivo, voz=voz)
            audio = AudioSegment.from_file(nome_arquivo)
            duracao = audio.duration_seconds

        arquivos.append(nome_arquivo)

        if comando:
            tempos.append((comando, tempo_acumulado + duracao))
        tempo_acumulado += duracao

    return arquivos, tempos

def concatenar_audios(lista_arquivos, arquivo_saida="saida_final.mp3"):
    combined = AudioSegment.empty()
    for arquivo in lista_arquivos:
        audio = AudioSegment.from_file(arquivo)
        combined += audio
    combined.export(arquivo_saida, format="mp3")
    return arquivo_saida

async def main(texto):
    segmentos = dividir_texto(texto)
    arquivos, tempos = await gerar_audios_segmentados(segmentos)

    arquivo_final = concatenar_audios(arquivos)
    print(f"\nÁudio final gerado: {arquivo_final}\n")

    print("Comandos e seus tempos (segundos):")
    for comando, tempo in tempos:
        print(f" - {comando} aos {tempo:.2f}s")

    # Opcional: apagar arquivos temporários
    for f in arquivos:
        os.remove(f)

if __name__ == "__main__":
    texto_exemplo = """
    Olá Daniel, seja bem-vindo à Montreal Magazine. [command:abrircortina]
    Aqui temos ofertas especiais para você! [command:mostrarlogo]
    Aproveite as promoções e faça boas compras.
    """

    asyncio.run(main(texto_exemplo))
