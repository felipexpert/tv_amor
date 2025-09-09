# main_video_subtitles_fixed.py
import os
import subprocess
import shutil
import whisper
import sys
from pathlib import Path

# ====== CONFIG ======
# VIDEO_PATH = r"C:\Users\MelhoresOfertas\OneDrive\clientes\sol-ração-líquida\video_2025-09-09.mp4"
VIDEO_PATH = r"C:\Users\MelhoresOfertas\OneDrive\clientes\sol-ração-líquida\video_2025-09-09\video_2025-09-09_novo.mp4"
WORKING_DIR = r"C:\working_subtitles"
MODEL_NAME = "small"   # tiny | base | small | medium | large
# BURN_IN = False        # False = embute como faixa (softsub mov_text). True = "queima" a legenda no vídeo (-vf subtitles)
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

def legendar_video(video_file_str: str, model_name: str = "small"):
    # Muda o diretório de trabalho atual
    os.chdir(WORKING_DIR)
  
    ensure_ffmpeg()

    video_file_path = Path(video_file_str)
    if not video_file_path.is_file():
        raise FileNotFoundError(f"Arquivo não encontrado: {video_file_path}")

    base_name = video_file_path.stem
    srt_file_str = f"{base_name}.srt"
    str_file_path = Path(srt_file_str)
    # output_path = base_dir / f"{base_name}_legendado.mp4"

    print(f"[1/3] Carregando modelo Whisper '{model_name}' (pode baixar na primeira execução)...")
    model = whisper.load_model(model_name)

    print("[2/3] Transcrevendo áudio e gerando legenda (isso pode demorar)...")
    result = model.transcribe(video_file_str, language="pt")

    # -------------- tentar usar util do whisper se existir ---------------
    write_func = getattr(whisper.utils, "write_srt", None)
    if callable(write_func):
        print("Usando whisper.utils.write_srt()")
        with str_file_path.open("w", encoding="utf-8") as f:
            write_func(result["segments"], file=f)
    else:
        print("whisper.utils.write_srt() não disponível — usando writer interno")
        write_srt_custom(result["segments"], str_file_path)

    print(f"\n✅ Legenda gerada: {str_file_path}")
    print("👉 Edite o arquivo se desejar corrigir. Salve as alterações e, quando pronto, digite 'CONTINUAR' e pressione Enter.\n")

    # espera confirmação do usuário
    user_input = input(">>> ")
    while user_input.strip().upper() != "CONTINUAR":
        print("Digite 'CONTINUAR' para prosseguir com a criação do vídeo legendado...")
        user_input = input(">>> ")

    # -------------- embutir legendas ---------------
    print("[3/3] Criando vídeo com legenda...")
    try:
      # Executa o ffmpeg dentro do diretório working_dir
      video_output = f"{video_file_path.stem}_legendado.mp4"
      command = [
          "ffmpeg",
          "-i", video_file_str,
          "-vf", f"subtitles={srt_file_str}",
          "-c:a", "copy",
          video_output
      ]
      subprocess.run(command, cwd=WORKING_DIR)

    except subprocess.CalledProcessError as e:
        print("Erro ao rodar ffmpeg. Veja a saída do ffmpeg acima.")
        raise

    print(f"\n✅ Vídeo legendado criado")


def copy_video(video_path: str) -> str | None:
  # Caminho para o vídeo original

  # Cria um objeto Path para facilitar a manipulação
  video_file = Path(video_path)
  video_file_name = video_file.name

  # Verifica se o arquivo existe
  if not video_file.is_file():
    print(f"Erro: O arquivo não foi encontrado em {video_path}")
    return None
  else:
    # Cria o nome do novo arquivo (com '_legendado' e a mesma extensão)
    new_video_path = Path(WORKING_DIR) / video_file_name

    # Copia o arquivo original para o novo caminho
    try:
      shutil.copyfile(video_file, new_video_path)
      print(f"Arquivo copiado com sucesso para: {new_video_path}")
      return str(video_file_name)
    except Exception as e:
      print(f"Ocorreu um erro ao copiar o arquivo: {e}")
      return None

def limpar_diretorio_working_subtitles():
  diretorio = WORKING_DIR

  for arquivo in os.listdir(diretorio):
      caminho_arquivo = os.path.join(diretorio, arquivo)
      if os.path.isfile(caminho_arquivo):
          os.remove(caminho_arquivo)

if __name__ == "__main__":
  # if len(sys.argv) < 2:
  #   print("Uso: python legendar.py <caminho-do-video.mp4>")
  #   sys.exit(1)
  # video_path = sys.argv[1]
  # limpar_diretorio_working_subtitles()
  video_file = copy_video(VIDEO_PATH)
  

  legendar_video(video_file)
