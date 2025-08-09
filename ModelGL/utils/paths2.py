from pathlib import Path
from typing import List, Union


def cp(*paths: Union[Path, str]) -> str:
    """
    cp - concat paths
    """
    if not paths:
        raise ValueError("É necessário pelo menos um path para base")

    base = Path(paths[0])
    for p in paths[1:]:
        p2 = Path(p)
        base /= p2
    return str(base)