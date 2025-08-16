# tv_amor

Utilize o estilo de escrita 'underscore_case', exemplo: "arquivo_de_texto.txt"

## Bibliotecas Python

instale o pyautogui e o opencv-python:
pip install pyautogui opencv-python

pip install pillow

instale também o pydantic
pip install pydantic 

pip install edge-tts pydub
    -- E lembre-se: o pydub depende do ffmpeg instalado no sistema para converter áudios.

pip install aiofiles librosa soundfile

pip install returns

## Estrutura de pastas
tv_amor/
├── generated_content
│   └── ... 
├── ep_file_infos_temp/
│   └── 020250803_felipe_e_gisele/
│       ├── sprites/
│       │   ├── sprite01.psd
│       │   └── sprite02.psd
│       ├── background.jpg
│       ├── audios/
│       │   ├── char_felipe_01.wav
│       │   └── char_gisele_01.wav
│       └── episode.json   ← contém tudo referenciado

*O caminho do diretório `tv_amor` será configurado no código, num diretório do sistema fora do repo git

O arquivo de background (cujo diretório utilizado está em `config.json`) deve conter, para cada cenário, o arquivo de imagem (png ou jpg) e o arquivo de configuração json chamado {nome-sem-extensão}_config.json 

Exemplo de {nome-sem-extensão}_config.json 
```
{
    "width": 1536,
    "height": 1024,
    "sprite_positions1": { 
        "sprite1": { "x": 890, "y": 932 }
    },
    "sprite_positions2": { 
        "sprite1": { "x": 512, "y": 932 },
        "sprite2": { "x": 1144, "y": 932 }
    }
}
```

Dica especial, configure o code-runner
no vault obsidian você explica como fazer