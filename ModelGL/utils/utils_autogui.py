import pyautogui

from utils.utils_print import print_alt
import utils.utils_paths_config as p
import pygetwindow as gw

def contains_img(img_path: str, confidence=0.9):
    try:
        # Tenta localizar a imagem na tela
        pyautogui.locateOnScreen(img_path, confidence)
        # Se a linha acima não lançar uma exceção, a imagem foi encontrada
        print_alt(f'Procura pela imagem "{img_path}": IMAGEM ENCONTRADA')
        return True
    except pyautogui.ImageNotFoundException:
        # Se a exceção for lançada, a imagem não foi encontrada
        print_alt(f'Procura pela imagem "{img_path}": IMAGEM NÃO ENCONTRADA')
        return False

# def contains_img(img_path: str):
#     loc = pyautogui.locateOnScreen(img_path, confidence=0.9)
#     # se loc for None, não tem na tela, se for outra coisa, tem
#     return (False if loc is None else True)


def wait_for_img(img_path: str):
    """
    Waits for an image to appear on the screen.
    """
    print(f'Esperando a imagem "{p.basename(img_path)}"')
    while True:
        # Wait for the Firefox window to appear
        try:
            if pyautogui.locateOnScreen(img_path, confidence=0.9):
                break
        except pyautogui.ImageNotFoundException:
            pass
        pyautogui.sleep(1)  # Sleep for a while before checking again

# Clica em uma parte da tela para tirar seleções
def click_to_deselect():
    click_point(1467, 72) 

def click_point(x:int, y:int):
    pos = pyautogui.Point(x, y)
    pyautogui.click(pos)
    print(f'Clicou em X="{x}" Y="{y}"')

def click_img(img_path: str, offset_x:int = 0, offset_y:int = 0, confidence = 0.9, double_click=False):
    """
    Clicks on the center of the image if it is found on the screen.
    """
    print(f'Vai clicar na imagem "{p.basename(img_path)}"')
    pos_center = pyautogui.locateCenterOnScreen(img_path, confidence=confidence)
    pos = pyautogui.Point(pos_center.x + offset_x, pos_center.y + offset_y)
    if pos is not None:
        if double_click:
            pyautogui.doubleClick(pos)
        else:
            pyautogui.click(pos)
        print(f'Clicou "{p.basename(img_path)}"')
    else:
        print(f'Botão "{p.basename(img_path)}" não encontrado na tela')

def wait_s(func):
    # Pass *args and **kwargs so the `func` function will be called with
    # all the necessary arguments!
    def wrapper(*args, **kwargs):
        pyautogui.sleep(1)  # Wait for 1 second before executing the function
        print(f'Esperando 1 s para a função "{func.__name__}" rodar...')
        func(*args, **kwargs)
    return wrapper

@wait_s
def click_img_s(img_path: str, offset_x:int = 0, offset_y:int = 0, confidence=0.9, double_click=False): click_img(img_path, offset_x, offset_y, confidence, double_click)

def focus_window_ca4() -> bool:
    return focus_window('Cartoon Animator 4')

# Retorna True se a janela foi encontrada, ou False se não foi
def focus_window(windowsTitle: str) -> bool:
    # Lista todas janelas abertas e procura pela janela do Cartoon Animator 4 pelo título
    janelas = gw.getWindowsWithTitle(windowsTitle)

    if not janelas:
        print_alt("Janela do CA4 não encontrada!")
        return False

    janela_ca4 = janelas[0]

    # Se a janela estiver minimizada, restaura
    if janela_ca4.isMinimized:
        janela_ca4.restore()

    # Foca a janela para trazê-la para frente
    janela_ca4.activate()

    # Pequena pausa para garantir que a janela está ativa
    pyautogui.sleep(1)
    print_alt("focar_janela vai retornar True")
    return True