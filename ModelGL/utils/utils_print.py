
import sys

def print_err(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

def print_alt(*args, **kwargs):
    """
    print_alt Print Alternative
    Serve para não jogar texto no stdout, que já está com a informação do json
    Por enquanto vamos jogar no stderr, depois podemos criar um log específico
    """
    # invocando print_err
    print_err(*args, **kwargs)

def log_str(text_str: str, path_str: str) -> None:
    """
    Adiciona o texto em uma nova linha no arquivo especificado.
    Se o arquivo não existir, ele será criado automaticamente.
    """
    with open(path_str, "a", encoding="utf-8") as arquivo:
        arquivo.write(text_str + "\n")