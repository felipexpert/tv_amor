# main_video_subtitles_fixed_v2.py
import os
import subprocess
import shutil
import whisper
import sys
from pathlib import Path

# ====== CONFIG ======
# Mantenha o caminho original, o script vai lidar com a cópia
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
    
    # Define o caminho do vídeo de saída legendado
    # Mudando o nome para evitar sobrescrever o arquivo de trabalho
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
            # CORREÇÃO APLICADA AQUI: Envolver o caminho do arquivo SRT entre aspas duplas.
            # O f-string garante que o caminho seja tratado como uma única string para o FFmpeg.
            # Também usamos pathlib para garantir que o caminho esteja no formato correto para o sistema.
            vf = f"subtitles='{str(srt_path).replace(os.path.sep, '/')}'"
            
            subprocess.run([
                "ffmpeg",
                "-i", str(video),
                "-vf", vf,
                "-c:a", "copy",
                "-y",  # Sobrescreve o arquivo de saída, se existir
                str(output_path)
            ], check=True)
        else:
            # Ajuste similar para o caso de 'softsub'
            subprocess.run([
                "ffmpeg",
                "-i", str(video),
                "-i", str(srt_path),
                "-c", "copy",
                "-c:s", "mov_text",
                "-y",
                str(output_path)
            ], check=True)

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
        # A cópia de trabalho agora terá um nome mais claro para não confundir
        new_video_path = video_file.parent / f"{video_file.stem}_trabalho{video_file.suffix}"

        try:
            shutil.copyfile(video_file, new_video_path)
            print(f"Arquivo copiado com sucesso para: {new_video_path}")
            return str(new_video_path)
        except Exception as e:
            print(f"Ocorreu um erro ao copiar o arquivo: {e}")
            return None

if __name__ == "__main__":
    # Remove a linha que redefine o VIDEO_PATH para não confundir com o CONFIG
    # video_path = r"C:\Users\MelhoresOfertas\OneDrive\clientes\sol-ração-líquida\video_2025-09-09\video_2025-09-09_novo.mp4"
    
    # A variável original do CONFIG é usada aqui para a cópia
    new_video_path = copy_video(VIDEO_PATH)

    if new_video_path:
        legendar_video(new_video_path)
        # Opcional: remova os arquivos de trabalho para manter a pasta limpa
        # os.remove(new_video_path)
        # os.remove(new_video_path.replace("_trabalho.mp4", ".srt"))
        # print(f"Arquivos de trabalho removidos.")