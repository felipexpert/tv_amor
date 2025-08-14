import asyncio
import edge_tts

async def list_brazilian_voices():
    # Obtém a lista completa de vozes
    voices = await edge_tts.list_voices()

    print("Vozes disponíveis em Português do Brasil (pt-BR):\n")
    
    # Filtra as vozes que têm "pt-BR" no atributo Locale
    brazilian_voices = [voice for voice in voices if "pt-BR" in voice["Locale"]]

    # Exibe as informações das vozes encontradas
    if brazilian_voices:
        for voice in brazilian_voices:
            # O "ShortName" é o nome que você vai usar para selecionar a voz
            print(f"Nome da Voz: {voice['ShortName']}")
            print(f"Gênero: {voice['Gender']}")
            print(print("-" * 30))
    else:
        print("Nenhuma voz em português do Brasil foi encontrada.")

if __name__ == "__main__":
    asyncio.run(list_brazilian_voices())