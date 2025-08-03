import pyautogui

def wait_for_img(img_path):
    """
    Waits for the Firefox window to appear on the screen.
    This function will continuously check for the presence of the Firefox identifier image.
    """
    while True:
        # Wait for the Firefox window to appear
        try:
            if pyautogui.locateOnScreen(img_path, confidence=0.9):
                break
        except pyautogui.ImageNotFoundException:
            pass
        pyautogui.sleep(1)  # Sleep for a while before checking again