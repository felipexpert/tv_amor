# main_video_subtitles_fixed_v3.py
import os
import subprocess
import shutil
import whisper
import sys
from pathlib import Path

# ====== CONFIG ======

VIDEO_PATH = r"C:\Users\MelhoresOfertas\OneDrive\clientes\sol-ração-líquida\video_2025-09-09\video_2025-09-09_novo.mp4"
MODEL_NAME = "small"   # tiny | base | small | medium | large
BURN_IN = True
# ====================

def _format_timestamp(seconds: float) -> str:
    total_ms = int(seconds * 1000)
    hours = total_ms // 3600000
    minutes = (total_ms % 3600000) // 60000
    secs = (total_ms % 60000) // 1000
    ms = total_ms % 1000
    return f"{hours:02}:{minutes:02}:{secs:02},{ms:03}"

def write_srt_custom(segments, file_path: Path):
    with file_path.open("w", encoding="utf-8") as f:
        for i, seg in enumerate(segments, start=1):
            start = _format_timestamp(seg["start"])
            end = _format_timestamp(seg["end"])
            text = seg.get("text", "").strip()
            f.write(f"{i}\n{start} --> {end}\n{text}\n\n")

def ensure_ffmpeg():
    if shutil.which("ffmpeg") is None:
        raise EnvironmentError("ffmpeg não encontrado no PATH. Instale-o e torne-o acessível (ex: chocolatey, winget, ou baixar do site).")

def legendar_video(video_path: str, model_name: str = "small"):
    ensure_ffmpeg()

    video = Path(video_path)
    if not video.is_file():
        raise FileNotFoundError(f"Arquivo de vídeo não encontrado: {video_path}")

    base_name = video.stem
    base_dir = video.parent
    srt_path = base_dir / f"{base_name}.srt"
    output_path = base_dir / f"{base_name}_legendado_final.mp4"

    print(f"[1/3] Carregando modelo Whisper '{model_name}' (pode baixar na primeira execução)...")
    model = whisper.load_model(model_name)

    print("[2/3] Transcrevendo áudio e gerando legenda (isso pode demorar)...")
    result = model.transcribe(str(video), language="pt")

    write_func = getattr(whisper.utils, "write_srt", None)
    if callable(write_func):
        print("Usando whisper.utils.write_srt()")
        with srt_path.open("w", encoding="utf-8") as f:
            write_func(result["segments"], file=f)
    else:
        print("whisper.utils.write_srt() não disponível — usando writer interno")
        write_srt_custom(result["segments"], srt_path)

    print(f"\n✅ Legenda gerada: {srt_path}")
    print("👉 Edite o arquivo se desejar corrigir. Salve as alterações e, quando pronto, digite 'CONTINUAR' e pressione Enter.\n")

    user_input = input(">>> ")
    while user_input.strip().upper() != "CONTINUAR":
        print("Digite 'CONTINUAR' para prosseguir com a criação do vídeo legendado...")
        user_input = input(">>> ")

    print("[3/3] Criando vídeo com legenda 'queimada'...")
    try:
        if BURN_IN:
            # CORREÇÃO APLICADA AQUI: usar aspas duplas para o caminho do SRT
            # e aspas duplas para o caminho do output. O caminho do input já está entre aspas
            # pelo subprocess.run, mas vamos ser consistentes para evitar problemas.
            
            # Formata os caminhos com aspas duplas, a sintaxe preferida para caminhos com espaços no Windows
            input_path_str = f'"{str(video)}"'
            srt_path_str = f'"{str(srt_path)}"'
            output_path_str = f'"{str(output_path)}"'

            # Cria a string de comando completa para ser passada para o shell
            command = f'ffmpeg -i {input_path_str} -vf "subtitles={srt_path_str}" -c:a copy -y {output_path_str}'

            # Executa o comando via shell. Isso contorna problemas de parsing de argumentos
            print(f"Executando comando: {command}")
            subprocess.run(command, shell=True, check=True)

        else:
            input_path_str = f'"{str(video)}"'
            srt_path_str = f'"{str(srt_path)}"'
            output_path_str = f'"{str(output_path)}"'
            
            command = f'ffmpeg -i {input_path_str} -i {srt_path_str} -c copy -c:s mov_text -y {output_path_str}'
            
            print(f"Executando comando: {command}")
            subprocess.run(command, shell=True, check=True)

    except subprocess.CalledProcessError as e:
        print("Erro ao rodar ffmpeg. Veja a saída do ffmpeg acima.")
        print(f"Comando executado: {e.cmd}")
        raise

    print(f"\n✅ Vídeo legendado criado com sucesso: {output_path}")

def copy_video(video_path: str) -> str | None:
    video_file = Path(video_path)

    if not video_file.is_file():
        print(f"Erro: O arquivo não foi encontrado em {video_path}")
        return None
    else:
        new_video_path = video_file.parent / f"{video_file.stem}_legenda{video_file.suffix}"

        try:
            shutil.copyfile(video_file, new_video_path)
            print(f"Arquivo copiado com sucesso para: {new_video_path}")
            return str(new_video_path)
        except Exception as e:
            print(f"Ocorreu um erro ao copiar o arquivo: {e}")
            return None

if __name__ == "__main__":
    new_video_path = copy_video(VIDEO_PATH)

    if new_video_path:
        legendar_video(new_video_path)
        # Os arquivos temporários podem ser removidos aqui para limpeza
        # os.remove(new_video_path)
        # os.remove(Path(new_video_path).parent / f"{Path(new_video_path).stem}.srt")
        # print("Arquivos temporários removidos.")