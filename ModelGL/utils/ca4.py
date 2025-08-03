import subprocess

class CA4:
    _caminho_ca4: str = "C:\\Program Files\\Reallusion\\Cartoon Animator 4\\bin64\\CartoonAnimator.exe"
    
    def open_ca4():
        print("Vai abrir o CA4 do caminho:")
        print(CA4._caminho_ca4)
        subprocess.Popen([CA4._caminho_ca4])
        
        print("Aguardando CA4 carregar...")