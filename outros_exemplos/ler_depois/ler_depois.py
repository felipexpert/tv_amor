import sys 

import pyautogui

import subprocess

import autogui_utils as au

query = sys.argv[1] if len(sys.argv) >= 2 else "Humanos"

# Abre o Firefox direto (se estiver no PATH do sistema)
subprocess.Popen("firefox")

# Alternativa para o linux
# pyautogui.hotkey('alt', 'space')
# pyautogui.sleep(1)
# pyautogui.typewrite('firefox\n')

au.wait_for_img("01-firefox-identifier.jpg")
print('localizou firefox')

pyautogui.sleep(1)
pyautogui.typewrite('https://pt.wikipedia.org\n')

au.wait_for_img("02-wikipedia-identifier.jpg")
print('localizou wikipedia')

# 4 vezes tab
for _ in range(4):
    pyautogui.press('tab')

pyautogui.typewrite(query)
pyautogui.press('enter')

pyautogui.sleep(5)

pyautogui.hotkey('ctrl', 'p')

pyautogui.sleep(2) 

pyautogui.press('enter')

pyautogui.sleep(2) 

pyautogui.press('enter')

pyautogui.sleep(2) 

au.wait_for_img("02-wikipedia-identifier.jpg")
print('localizou wikipedia')

pyautogui.hotkey('ctrl', 'w')