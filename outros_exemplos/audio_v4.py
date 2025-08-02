import re
import asyncio
import edge_tts
from pydub import AudioSegment
import os
import json
import ast

RE_CHAR_LABEL = re.compile(r"\[char_label:([a-zA-Z0-9_]+)\]")
RE_COMMAND_JSON = re.compile(r"\{[^{}]+\}")

async def gerar_audio(texto, nome_arquivo, voz="pt-BR-AntonioNeural"):
    communicate = edge_tts.Communicate(texto, voice=voz)
    with open(nome_arquivo, "wb") as f:
        async for chunk in communicate.stream():
            if chunk["type"] == "audio":
                f.write(chunk["data"])

def parsear_bloco(texto):
    """
    Divide o texto por personagens e comandos embutidos.
    Retorna lista de dicionários: {char_label, tipo: 'fala' ou 'comando', conteudo}
    """
    blocos = []
    partes = RE_CHAR_LABEL.split(texto)
    # partes = [texto_antes, char1, fala1, char2, fala2, ...]
    for i in range(1, len(partes), 2):
        char = partes[i].strip()
        fala_e_comandos = partes[i+1]
        
        pos = 0
        comandos = list(RE_COMMAND_JSON.finditer(fala_e_comandos))

        for cmd_match in comandos:
            start, end = cmd_match.span()
            fala = fala_e_comandos[pos:start].strip()
            if fala:
                blocos.append({'char_label': char, 'tipo': 'fala', 'conteudo': fala})
            comando = ast.literal_eval(cmd_match.group())
            comando['char_label'] = comando.get('char_label', char)
            blocos.append({'tipo': 'comando', 'conteudo': comando})
            pos = end

        # Pega o resto depois do último comando, se houver
        resto = fala_e_comandos[pos:].strip()
        if resto:
            blocos.append({'char_label': char, 'tipo': 'fala', 'conteudo': resto})

    return blocos

async def processar_blocos(blocos, voz_default="pt-BR-AntonioNeural"):
    tempo_acumulado = 0.0
    eventos = []
    arquivos = []

    # Contador separado para nomear os arquivos de áudio
    contador_audio = 0  

    for idx, bloco in enumerate(blocos):
        if bloco['tipo'] == 'fala':
            texto = bloco['conteudo']
            char = bloco['char_label']
            nome_arquivo = f"{contador_audio}_{char}.mp3"
            print(f"[{tempo_acumulado:.2f}s] MP3-{contador_audio} fala: '{texto[:30]}...'")

            await gerar_audio(texto, nome_arquivo, voz=voz_default)
            audio = AudioSegment.from_file(nome_arquivo)
            duracao = audio.duration_seconds
            arquivos.append(nome_arquivo)

            eventos.append({
                'tipo': 'fala',
                'char_label': char,
                'arquivo': nome_arquivo,
                'inicio': tempo_acumulado,
                'fim': tempo_acumulado + duracao
            })

            tempo_acumulado += duracao
            
            # Incrementa apenas quando gera áudio
            contador_audio += 1

        elif bloco['tipo'] == 'comando':
            comando = bloco['conteudo']
            tipo = comando['type']
            char = comando.get('char_label', 'desconhecido')

            if tipo == 'pause':
                dur = float(comando.get('dur', 0.5))
                print(f"[{tempo_acumulado:.2f}s] MP3-{contador_audio} pausa de {dur:.1f}s")
                audio = AudioSegment.silent(duration=int(dur * 1000))
                nome_arquivo = f"{contador_audio}_pause.mp3"
                audio.export(nome_arquivo, format="mp3")
                arquivos.append(nome_arquivo)

                tempo_acumulado += dur

                # Incrementa apenas quando gera áudio
                contador_audio += 1

            else:
                print(f"[{tempo_acumulado:.2f}s] Comando '{tipo}' para {char}")
                eventos.append({
                    'tipo': 'comando',
                    'acao': tipo,
                    'char_label': char,
                    'tempo': tempo_acumulado
                })

    return arquivos, eventos

def concatenar_audios(lista_arquivos, arquivo_saida="saida_final.mp3"):
    combined = AudioSegment.empty()
    for arquivo in lista_arquivos:
        audio = AudioSegment.from_file(arquivo)
        combined += audio
    combined.export(arquivo_saida, format="mp3")
    return arquivo_saida

async def main(texto):
    blocos = parsear_bloco(texto)
    arquivos, eventos = await processar_blocos(blocos)

    arquivo_final = concatenar_audios(arquivos)
    print(f"\nÁudio final gerado: {arquivo_final}\n")

    print("🔊 Eventos de fala e comandos:")
    for evento in eventos:
        print(json.dumps(evento, indent=2))

    # Os arquivos individuais de áudio serão mantidos para uso na aplicação
    print("\nArquivos individuais salvos:")
    for f in arquivos:
        print(f" - {f}")

if __name__ == "__main__":
    texto_exemplo = """
    [char_label:char_felipe]Olá Gisele{'type':'wave','char_label':'char_felipe'}{'type':'wave','char_label':'char_gisele'}{'type':'pause','dur':'0.5'}Tudo bem por aí?
    [char_label:char_gisele]Olá Felipe! Tudo ótimo!
    """

    asyncio.run(main(texto_exemplo))

