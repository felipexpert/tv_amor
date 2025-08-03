from pathlib import Path


def cp(*paths: str) -> str:
    """
    cp - concat paths
    Recebe vários paths em str, onde o primeiro é a base,
    e concatena todos usando Path, retornando o resultado como string.
    """
    if not paths:
        raise ValueError("É necessário pelo menos um path para base")

    base = Path(paths[0])
    for p in paths[1:]:
        base /= p
    return str(base)