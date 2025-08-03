import subprocess
import time

import socket
import time

def verificar_internet() -> bool:
    """
    Verifica se há conexão ativa com a internet.
    Retorna True se conectado, False caso contrário.
    """
    
    try:
        # Tenta criar uma conexão com um servidor externo (Google DNS)
        # Isso verifica se o computador pode resolver um nome de domínio e se conectar.
        s = socket.create_connection(("8.8.8.8", 53), timeout=5)
        return True
    except OSError:
        return False

def assegura_offline():
    print("Iniciando verificação de status da internet...")

    if verificar_internet():
        print("**************************************************")
        print("* ATENÇÃO: ESTÁ ONLINE.                          *")
        print("* O script será encerrado para evitar conflitos. *")
        print("**************************************************")
        # O script simplesmente termina aqui, ou você pode usar sys.exit()
        import sys
        sys.exit()
    else:
        print("--------------------------------------------------")
        print("- OFFLINE. Ótimo!                                -")
        print("- Prosseguindo com a automação do CA4...         -")
        print("--------------------------------------------------")
        # --- SEU CÓDIGO PYAUTOGUI VAI ENTRAR AQUI ---
        # Exemplo de onde você colocaria o código:
        # import pyautogui
        # pyautogui.alert("Internet está offline, pronto para prosseguir!")
        # pyautogui.press('win') # Exemplo de ação para testar