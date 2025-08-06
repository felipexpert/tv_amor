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