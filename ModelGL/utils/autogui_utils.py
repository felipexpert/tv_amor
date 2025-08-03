import pyautogui

import utils.paths as p

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

def click_img(img_path: str):
    """
    Clicks on the center of the image if it is found on the screen.
    """
    print(f'Vai clicar na imagem "{p.basename(img_path)}"')
    pos = pyautogui.locateCenterOnScreen(img_path, confidence=0.9)
    if pos is not None:
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
def click_img_s(img_path: str): click_img(img_path)