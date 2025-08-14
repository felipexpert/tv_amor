import asyncio
import edge_tts

async def narrar_texto(texto, arquivo_saida, voz="pt-BR-FranciscaNeural", pitch="+3Hz", style=None):
    # Monta o SSML corretamente
    if style:
        ssml = f"""
        <speak version="1.0" xml:lang="pt-BR"
               xmlns:mstts="http://www.w3.org/2001/mstts"
               xmlns="http://www.w3.org/2001/10/synthesis">
            <voice name="{voz}">
                <mstts:express-as style="{style}">
                    <prosody pitch="{pitch}">
                        {texto}
                    </prosody>
                </mstts:express-as>
            </voice>
        </speak>
        """
    else:
        ssml = f"""
        <speak version="1.0" xml:lang="pt-BR"
               xmlns="http://www.w3.org/2001/10/synthesis">
            <voice name="{voz}">
                <prosody pitch="{pitch}">
                    {texto}
                </prosody>
            </voice>
        </speak>
        """

    comunicador = edge_tts.Communicate(ssml, voice=voz)
    await comunicador.save(arquivo_saida)
    print(f"Áudio salvo em: {arquivo_saida}")

async def main():
    await narrar_texto(
        "Olá, esta é uma fala com pitch e estilo!",
        "saida.mp3",
        voz="pt-BR-FranciscaNeural",
        pitch="+3Hz",
        style="cheerful"
    )

if __name__ == "__main__":
    asyncio.run(main())
