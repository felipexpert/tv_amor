from pathlib import Path
import subprocess
import sys
from typing import Dict, List, Union

from PIL import Image
import cv2
import pyautogui
import pyperclip

from utils.utils_ani_auto_task import get_aat_action_speechs, make_gestures_complete
from utils.classes.config import Config
from utils.utils_autogui_ca4_details import milliseconds_to_frames, persona_number_ca4_selector
from utils.utils_conexao import assegura_offline
from utils.utils_print import print_alt
from utils.classes.ani_auto_task import AAction, AGesture, ASpeech, AniAutoTask, CGesture, EPeNumber, TPeAction, TPersona
from utils.utils_autogui import click_img, click_img_s, click_point, click_to_deselect, contains_img, focus_window_ca4, wait_for_img
from utils.utils_paths_config import Paths

def open_ca4():
    _caminho_ca4: str = "C:\\Program Files\\Reallusion\\Cartoon Animator 4\\bin64\\CartoonAnimator.exe"
    print("Vai abrir o CA4 do caminho:")
    print(_caminho_ca4)
    subprocess.Popen([_caminho_ca4])
    
    print("Aguardando CA4 carregar...")
    
    wait_for_img(Paths.IMG_CA4_START)
    click_img_s(Paths.IMG_CA4_BTN_THANK_YOU)

def start_ca4():
    assegura_offline()
    print_alt("Iniciando vídeo...")
    open_ca4()

def working_dir_file(file: Union[str,Path]) -> str:
   return str(Path(Paths.AAT_WORKING_DIR) / (Path(str(file))))


def add_personas(aat: AniAutoTask):
    # IMPORTANTE: Precisa configurar o "Change View Mode" (esta opção aparece quando clica com o botão direito em
    # uma parte da "Content Manager"), para ficar itens quadradinhos e preenchendo a horizontal

    # Vai clicar no botão para adicionar personagem
    # de acordo com o número de personagens, faz esta ação
    personasQtd = len(aat.aatPersonas)
    for pe_number in range(1, (personasQtd + 1)):
        # primeiramente, precisamos adicionar o actor correto do CA4
        # comece vendo se está no content manager
        if not contains_img(Paths.IMG_CA4_CONTENT_MANAGER, confidence=0.90):
            click_img_s(Paths.IMG_CA4_CONTENT_MANAGER_2)
        # deixa a aba "Actor" selecionada
        if not contains_img(Paths.IMG_CA4_BUTTON_ACTOR, confidence=0.90):
            click_img_s(Paths.IMG_CA4_BUTTON_ACTOR_2, confidence=0.90)
        
        # se o voltar estiver "pretinho" clica 4 vezes
        if contains_img(Paths.IMG_CA4_MENU_VOLTAR, confidence=0.90):
            click_img_s(Paths.IMG_CA4_MENU_VOLTAR, confidence=0.90)
            pyautogui.sleep(0.5)
            pyautogui.click()
            pyautogui.sleep(0.5)
            pyautogui.click()
            pyautogui.sleep(0.5)
            pyautogui.click()
        
        # cliques para apresentar os atores, e seleciona o ator correto
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_CHARACTER, double_click=True)
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_CHARACTER_G3, double_click=True)
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_CHARACTER_G3_HUMAN, double_click=True)
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_CHARACTER_G3_HUMAN_G3_S, double_click=True)

        # espera o character carregar
        pyautogui.sleep(5)

        # coloca o PSD correto
        click_img_s(Paths.IMG_CA4_COMPOSER)
        pyautogui.sleep(3)
        click_img_s(Paths.IMG_CA4_IMPORT_PSD_ASSETS)
        pyautogui.sleep(1)
        psd_path:str = working_dir_file(f"persona_{pe_number}.psd")
        pyautogui.write(psd_path)
        pyautogui.sleep(0.5)
        pyautogui.press("enter")
        pyautogui.sleep(3)
        click_to_deselect()
        if contains_img(Paths.IMG_CA4_PSD_LOADING_OPTIONS_OK):
            click_img_s(Paths.IMG_CA4_PSD_LOADING_OPTIONS_OK)
        else:
            click_img_s(Paths.IMG_CA4_PSD_LOADING_OPTIONS_OK_2)
        pyautogui.sleep(0.5)
        click_to_deselect()
        pyautogui.sleep(0.5)
        wait_for_img(Paths.IMG_CA4_PSD_LOADING_OPTIONS_CLOSE)
        click_img_s(Paths.IMG_CA4_PSD_LOADING_OPTIONS_CLOSE)
        click_to_deselect()
        pyautogui.sleep(2)
        
        # volta
        click_img_s(Paths.IMG_CA4_BACK_STAGE)


        


def add_personas_bkp(aat: AniAutoTask):
    # Vai clicar no botão para adicionar personagem
    # de acordo com o número de personagens, faz esta ação
    personasQtd = len(aat.aatPersonas)
    for pe_number in range(1, (personasQtd + 1)):
        pyautogui.sleep(2)
        click_img_s(Paths.IMG_CA4_CREATE_G3_FREE_BONE_ACTOR)
        # psd_path:str = str(Path(Paths.AAT_WORKING_DIR) / Path(f"{pe_number}.psd"))
        psd_path:str = working_dir_file(f"persona_{pe_number}.psd")
        pyautogui.sleep(1)
        pyautogui.write(psd_path)
        pyautogui.sleep(0.5)
        pyautogui.press("enter")
        pyautogui.sleep(7)
        # Depois que ele adiciona, tem que voltar para a tela anterior
        click_img_s(Paths.IMG_CA4_BACK_STAGE)

def place_personas(aat: AniAutoTask):
    # Itera cada TPersona
    for persona in aat.aatPersonas:
        persona: TPersona = persona
        pNumber: EPeNumber = persona.pNumber
        posX: int = persona.pX
        posY: int = persona.pY
        pyautogui.sleep(0.5)
        persona_number_ca4_selector(pNumber)

        # Posição X
        # eu coloquei um offset_x 31, porque o campo está um pouco deslocado em relação à label
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_POS_X, offset_x=31)
        pyautogui.sleep(0.5)
        pyautogui.hotkey('ctrl', 'a')
        pyautogui.sleep(0.5)
        pyautogui.write(str(posX))
        pyautogui.sleep(0.5)
        pyautogui.press("enter")

        # Posição Y
        pyautogui.sleep(0.5)
        click_img_s(Paths.IMG_CA4_POS_Y, 31)
        pyautogui.sleep(0.5)
        pyautogui.hotkey('ctrl', 'a')
        pyautogui.sleep(0.5)
        pyautogui.write(str(posY))
        # pyautogui.sleep(0.5)
        # pyautogui.press("enter")

        # temporário START
        press_key_n_times("tab", 2)
        pyautogui.write("60")
        pyautogui.sleep(0.5)
        pyautogui.press("tab")
        pyautogui.sleep(0.5)
        pyautogui.write("60")
        pyautogui.sleep(0.5)
        pyautogui.press("enter")
        # temporário END
        
        

         

def flip_persona_1_if_needed(aat: AniAutoTask):
    personasQtd = len(aat.aatPersonas)
    if personasQtd > 1:
        persona_number_ca4_selector(EPeNumber.EPeNum1)
        # pyautogui.sleep(1)
        # click_img_s(Paths.IMG_CA4_FIND_PERSONA_1)
        pyautogui.sleep(0.5)
        pyautogui.keyDown('alt')
        pyautogui.sleep(0.5)
        pyautogui.press('e')
        pyautogui.sleep(0.5)
        pyautogui.press('f')
        pyautogui.sleep(0.5)
        pyautogui.press('h')
        pyautogui.sleep(0.5)
        pyautogui.keyUp('alt')
        pyautogui.sleep(0.5)

def render_video():
    # Abrir o menu e colocar MP4
    render_video_open_menu_and_set_MP4()

    # Export
    click_img_s(Paths.IMG_CA4_RENDER_SETTINGS_EXPORT)
    pyautogui.sleep(1)
    video_file_temp_path:str = str(Path(Paths.AAT_WORKING_DIR) / Path('video_temp.mp4')) # video temp
    pyautogui.write(video_file_temp_path)
    pyautogui.sleep(0.5)
    pyautogui.press('enter')
    wait_for_img(Paths.IMG_CA4_VIDEO_EXPORT_COMPLETE)
    click_img_s(Paths.IMG_CA4_WATCH_VIDEO_NOW_NO_BUTTON)
    pyautogui.sleep(0.5)
    pyautogui.press('esc')

    compact_video(video_file_temp_path)

def compact_video(video_file_temp_path:str):
    print(f"compactando o vídeo '{video_file_temp_path}'...")

    video_file_path:str = str(Path(Paths.AAT_WORKING_DIR) / Path('video.mp4')) # video pronto


        # Executa ffmpeg
    subprocess.run([
        "ffmpeg", "-y",
        "-i", video_file_temp_path,
        "-vcodec", "libx264",
        "-crf", "20",
        "-preset", "slow",
        "-acodec", "aac", "-b:a", "192k",
        video_file_path
    ], check=True)

    # Remove video temporário
    Path(video_file_temp_path).unlink()

    print(f"Vídeo compactado e salvo em: {video_file_path}")

def prepare_render_video_configs(aat: AniAutoTask):
    bg_width, bg_height = get_background_size(aat)
    
    # Abrir o menu e colocar MP4
    render_video_open_menu_and_set_MP4()

    # set export configs
    press_key_n_times('tab', 2)
    pyautogui.press('7')
    pyautogui.sleep(0.5)
    press_key_n_times('tab', 2)
    pyautogui.press('7')
    pyautogui.sleep(0.5)
    press_key_n_times('tab', 3)
    pyautogui.press('space')
    pyautogui.sleep(0.5)
    pyautogui.press('tab')
    pyautogui.sleep(0.5)
    pyautogui.write(str(bg_height))
    pyautogui.sleep(0.5)
    pyautogui.press('enter')
    pyautogui.sleep(0.5)
    pyautogui.press('tab')
    pyautogui.sleep(0.5)
    pyautogui.write(str(bg_width))
    pyautogui.sleep(0.5)
    pyautogui.press('enter')
    pyautogui.sleep(0.5)
    click_to_deselect()
    pyautogui.sleep(0.5)
    pyautogui.press('esc')
    pyautogui.sleep(0.5)

def render_video_open_menu_and_set_MP4():
    pyautogui.sleep(0.5)
    pyautogui.keyDown('alt')
    pyautogui.sleep(0.5)
    pyautogui.press('r')
    pyautogui.sleep(0.5)
    pyautogui.press('v')
    pyautogui.sleep(0.5)
    pyautogui.keyUp('alt')
    pyautogui.sleep(0.5)

    # certifica que a janela está "scrolada" no começo
    click_img_s(Paths.IMG_CA4_RENDER_SETTINGS_FIELD_REFERENCE)
    press_key_n_times('up', 15)

    # mudar o tipo do vídeo para MP4
    click_img_s(Paths.IMG_CA4_RENDER_SETTINGS_FORMAT, offset_x=65)
    pyautogui.sleep(0.5)
    press_key_n_times('down', 2)
    pyautogui.press('enter')


def add_background(aat: AniAutoTask):
    pyautogui.sleep(2)
    click_img_s(Paths.IMG_CA4_CREATE_MEDIA)
    print_alt("funcionou click em IMG_CA4_CREATE_MEDIA")
    pyautogui.sleep(1)
    click_img_s(Paths.IMG_CA4_BACKGROUND)
    pyautogui.sleep(1)
    bg_path = working_dir_file(aat.aatBackgroundImage)
    pyautogui.write(bg_path)
    pyautogui.sleep(0.5)
    pyautogui.press("enter")

def get_image_size(path: Path) -> tuple[int, int]:
    """Retorna (width, height) da imagem no caminho informado."""
    with Image.open(path) as img:
        return img.width, img.height

def get_video_size(path: Path) -> tuple[int, int]:
    """Retorna (width, height) do vídeo no caminho informado."""
    cap = cv2.VideoCapture(str(path))
    if not cap.isOpened():
        raise ValueError(f"Não foi possível abrir o vídeo: {path}")
    
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    cap.release()
    return width, height

def get_background_size(aat: AniAutoTask) -> tuple[int, int]:
    bg_path_str = working_dir_file(aat.aatBackgroundImage)

    caminho = Path(bg_path_str)

    if not caminho.exists():
        print_alt(f"Arquivo não encontrado: {caminho}")
        return

    return get_image_size(caminho)

def focus_or_open_ca4():
    # pyautogui.sleep(2)  # Sleep for a while before checking again
    janela_encontrada = focus_window_ca4()
    print_alt("janela_encontrada", janela_encontrada)
    if not janela_encontrada:
        print_alt("O Cartoon Animator 4 não está disponível, vamos carregar o programa...")
        start_ca4()

def set_video_total_duration(aat: AniAutoTask):
    # Obter os frames
    duration = aat.aatTotalDuration
    durationFrames = milliseconds_to_frames(duration)
    
    # focalizar o campo
    pyautogui.sleep(0.5)
    pyautogui.hotkey('ctrl', 'shift', 'p')
    pyautogui.sleep(0.5)
    pyautogui.press('tab')
    pyautogui.sleep(0.5)
    pyautogui.write(str(durationFrames))
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(0.5)
    pyautogui.press('esc')
    pyautogui.sleep(0.5)

def add_personas_gestures(aat: AniAutoTask):
    def make_person_gestures_dict():
        person_gestures_dict:Dict[EPeNumber,List[AGesture]] = {}
        for action in aat.aatActions:
            pe_number: EPeNumber = action.tpaNumber
            action: AAction = action.tpaAction
            if action.tag != "AGesture":
                continue # é um speech, pode ir para a próxima ação
            gesture: AGesture = action # converte para AGesture

            if pe_number in person_gestures_dict:
                actions:List[AGesture] = person_gestures_dict.get(pe_number)
                actions.append(gesture)
            else:
                person_gestures_dict.update({pe_number: [ gesture ]})

        # adiciona as pessoas mesmo se não houverem gestos
        for p in aat.aatPersonas:
            persona: TPersona = p
            if persona.pNumber not in person_gestures_dict:
                # se um personagem da história, não houver sido adicionado ainda, por ausência de gestos, adiciona sem gestos
                person_gestures_dict.update({persona.pNumber: [ ]})

        return person_gestures_dict

    person_gestures_dict:Dict[EPeNumber,List[AGesture]] = make_person_gestures_dict()

    print('person_gestures_dict', person_gestures_dict)

    for pe_number, gestures in person_gestures_dict.items():
        add_persona_gestures(pe_number, gestures, aat.aatTotalDuration)


def add_persona_gestures(pe_number: EPeNumber, gestures: List[AGesture], total_duration_millis: int):
    print(f"Vai fazer a persona {str(pe_number)}")
    # select persona
    persona_number_ca4_selector(pe_number)
    pyautogui.sleep(0.5)
    add_gestures(gestures, total_duration_millis)

# Você já vai ter selecionado o persona
def add_gestures(gestures_initial: List[AGesture], total_duration_millis: int):
    gestures: List[AGesture] = make_gestures_complete(gestures_initial, total_duration_millis)


    for gesture in gestures:
        gesture: AGesture = gesture
        
        gesture_enum: CGesture = gesture.agGesture
        # prepare
        # click_to_deselect()
        # pyautogui.sleep(0.5)
        # pyautogui.press('a')
        # pyautogui.sleep(0.5)
        time_frames = milliseconds_to_frames(gesture.agStartTime)
        match gesture_enum:
            case CGesture.GHi:
                start_inserting_action(time_frames)
                press_key_n_times('up', 3)
                gesture_conclude()
            case CGesture.GStandShort:
                start_inserting_action(time_frames)
                press_key_n_times('down', 3)
                gesture_conclude()
                time_frames += 28 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 5)
                gesture_conclude()
            case CGesture.GStandLong:
                start_inserting_action(time_frames)
                press_key_n_times('down', 3)
                gesture_conclude()
                time_frames += 28 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 4)
                gesture_conclude()
                time_frames += 41 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 5)
                gesture_conclude()
            case CGesture.GThinkShort:
                start_inserting_action(time_frames)
                press_key_n_times('down', 17)
                gesture_conclude()
                time_frames += 13 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 19)
                gesture_conclude()
            case CGesture.GThinkLong:
                start_inserting_action(time_frames)
                press_key_n_times('down', 17)
                gesture_conclude()
                time_frames += 13 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 18)
                gesture_conclude()
                time_frames += 18 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 19)
                gesture_conclude()
            case CGesture.GTalkShort:
                start_inserting_action(time_frames)
                press_key_n_times('down', 6)
                gesture_conclude()
                time_frames += 12 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 8)
                gesture_conclude()
            case CGesture.GTalkLong:
                start_inserting_action(time_frames)
                press_key_n_times('down', 6)
                gesture_conclude()
                time_frames += 12 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 7)
                gesture_conclude()
                time_frames += 38 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 8)
                gesture_conclude()
            case CGesture.GWorryShort:
                start_inserting_action(time_frames)
                press_key_n_times('down', 14)
                gesture_conclude()
                time_frames += 19 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 16)
                gesture_conclude()
            case CGesture.GWorryLong:
                start_inserting_action(time_frames)
                press_key_n_times('down', 14)
                gesture_conclude()
                time_frames += 19 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 15)
                gesture_conclude()
                time_frames += 29 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('down', 16)
                gesture_conclude()
            case CGesture.GShakeLegShort:
                start_inserting_action(time_frames)
                press_key_n_times('up', 15)
                gesture_conclude()
                time_frames += 10 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('up', 13)
                gesture_conclude()
                pass
            case CGesture.GShakeLegLong:
                start_inserting_action(time_frames)
                press_key_n_times('up', 15)
                gesture_conclude()
                time_frames += 10 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('up', 14)
                gesture_conclude()
                time_frames += 27 - 1 # duração menos o "overlap"
                start_inserting_action(time_frames)
                press_key_n_times('up', 13)
                gesture_conclude()
            case CGesture.GExcited:
                start_inserting_action(time_frames)
                press_key_n_times('up', 16)
                gesture_conclude()
            case CGesture.GDance:
                start_inserting_action(time_frames)
                press_key_n_times('up', 2)
                gesture_conclude()
            case CGesture.GDefault:
                start_inserting_action(time_frames)
                press_key_n_times('down', 2)
                gesture_conclude()

def start_inserting_action(time_frames:int):
    set_time_position_in_frames(time_frames)
    click_to_deselect()
    pyautogui.sleep(0.2)
    pyautogui.press('a')
    pyautogui.sleep(0.2)

def press_key_n_times(key:str, times:int):
    for _ in range(0, times):
        pyautogui.sleep(0.1)
        pyautogui.press(key)
    pyautogui.sleep(0.5)

def hot_key_n_times(keys:List[str], times:int):
    for _ in range(0, times):
        pyautogui.sleep(0.1)
        pyautogui.hotkey(*keys)
    pyautogui.sleep(0.5)

def gesture_conclude():
    pyautogui.press('enter')
    ensure_paused_after_timeline_action()
    # pyautogui.press('space')
    # if contains_img(Paths.IMG_CA4_PAUSE_TIMELINE, confidence=0.90):
    #     click_img(Paths.IMG_CA4_PAUSE_TIMELINE)
    click_to_deselect()


def natural_standing_pesonas_bkp(aat: AniAutoTask):
    for persona in aat.aatPersonas:
        natural_standing_pesona(aat, persona.pNumber)
    pyautogui.sleep(0.5)
    set_time_position_in_millis(0) # volta o temporizador para o começo (não era necessario)
    pyautogui.sleep(0.5)

def natural_standing_pesona(aat: AniAutoTask, pNumber:EPeNumber):

    total_time = aat.aatTotalDuration
    total_time_frames = milliseconds_to_frames(total_time)

    # select persona
    persona_number_ca4_selector(pNumber)

    # Começa do início
    set_time_position_in_millis(0)

    curr_frame = 1 # Os frames começam do 1

    while total_time_frames > curr_frame:
        click_to_deselect()
        pyautogui.sleep(0.5)
        pyautogui.press('a')
        pyautogui.sleep(0.5)
        pyautogui.press('down')
        pyautogui.sleep(0.5)
        pyautogui.press('down')
        pyautogui.sleep(0.5)
        pyautogui.press('enter')
        pyautogui.sleep(0.5)
        # aguardando ele posicionar a agulha de tempo
        pyautogui.sleep(4) 
        # obten os frames
        curr_frame = get_time_position_in_frames()


        

def add_all_speeches(aat: AniAutoTask, config: Config):
    action_speechs:List[TPeAction] = get_aat_action_speechs(aat)

    for action_speech in action_speechs:
        action_speech:TPeAction = action_speech
        add_persona_speech(action_speech.tpaNumber, action_speech.tpaAction, config)
    
    set_time_position_in_millis(0) # volta a timeline no começo
    click_to_deselect()

def add_persona_speech(pNumber:EPeNumber, speech: ASpeech, config: Config):

    # select persona
    persona_number_ca4_selector(pNumber)

    #set time
    set_time_position_in_millis(speech.asStartTime)
    # click_img_s(Paths.IMG_CA4_TIMELINE_PLAYHEAD, offset_x=31)
    # pyautogui.hotkey('ctrl', 'a')
    # pyautogui.sleep(0.5)
    # time_frame = milliseconds_to_frames(speech.asStartTime)
    # pyautogui.write(str(time_frame))
    # pyautogui.sleep(0.5)
    # pyautogui.press("enter")
    # pyautogui.sleep(0.5)

    # insert audio file
    click_img_s(Paths.IMG_CA4_CREATE_SCRIPT)
    pyautogui.sleep(0.5)
    click_img_s(Paths.IMG_CA4_WAVE_FILE)
    pyautogui.sleep(0.5)
    audio_path: Path = config.workingDir / Path(speech.asAudioWav)
    pyautogui.write(str(audio_path))
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    ensure_paused_after_timeline_action()
    # pyautogui.press("space")
    # if contains_img(Paths.IMG_CA4_PAUSE_TIMELINE):
    #     click_img_s(Paths.IMG_CA4_PAUSE_TIMELINE)
    pyautogui.sleep(0.5)

def ensure_paused_after_timeline_action():
    pyautogui.press("space")
    if contains_img(Paths.IMG_CA4_PAUSE_TIMELINE):
        pyautogui.sleep(0.5)
        if contains_img(Paths.IMG_CA4_PAUSE_TIMELINE):
            click_img_s(Paths.IMG_CA4_PAUSE_TIMELINE)

def set_time_position_in_frames(time_frames:int):
    click_img_s(Paths.IMG_CA4_TIMELINE_PLAYHEAD, offset_x=31)
    pyautogui.hotkey('ctrl', 'a')
    pyautogui.sleep(0.5)
    pyautogui.write(str(time_frames))
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(0.5)

def set_time_position_in_millis(milliseconds:int):
    click_img_s(Paths.IMG_CA4_TIMELINE_PLAYHEAD, offset_x=31)
    pyautogui.hotkey('ctrl', 'a')
    pyautogui.sleep(0.5)
    time_frames = milliseconds_to_frames(milliseconds)
    pyautogui.write(str(time_frames))
    pyautogui.sleep(0.5)
    pyautogui.press("enter")
    pyautogui.sleep(0.5)

def get_time_position_in_frames() -> int:
    click_img_s(Paths.IMG_CA4_TIMELINE_PLAYHEAD, offset_x=31)
    pyautogui.hotkey('ctrl', 'a')
    pyautogui.sleep(0.5)
    pyautogui.hotkey('ctrl', 'c')
    pyautogui.sleep(0.5)
    frames:int = int(pyperclip.paste().strip())
    return frames


def display_timeline_if_hidden():
    # mostra a timeline, APENAS se ela não estiver visível
    if not contains_img(Paths.IMG_CA4_TIMELINE):
        pyautogui.press('f3')