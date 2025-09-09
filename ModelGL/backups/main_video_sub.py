import subprocess
from pathlib import Path

# Diretório onde estão os arquivos
working_dir = Path(r"C:\working_subtitles")

video_input = "video.mp4"
subtitle_file = "video.srt"
video_output = "video_legen.mp4"

# Executa o ffmpeg dentro do diretório working_dir
command = [
    "ffmpeg",
    "-i", video_input,
    "-vf", f"subtitles={subtitle_file}",
    "-c:a", "copy",
    video_output
]

# subprocess.run com cwd = working_dir
subprocess.run(command, cwd=working_dir)
