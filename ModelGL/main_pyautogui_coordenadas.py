from pynput import mouse

def on_click(x, y, button, pressed):
    if pressed:
        print(f'Clicou em: ({x}, {y})')

def main():
    print("Clique com o mouse para ver as coordenadas. Pressione Ctrl+C para sair.")
    with mouse.Listener(on_click=on_click) as listener:
        listener.join()

if __name__ == "__main__":
    main()

