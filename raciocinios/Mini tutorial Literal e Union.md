# Mini tutorial Literal e Union

Esses dois recursos vêm do módulo typing e ajudam muito a dar mais estrutura ao código

## Literal

O Literal é parecido com o Enum do java

O Literal no GuidoLang (Python) é parecido com o enum do Java no sentido de restringir os valores possíveis de um campo ou parâmetro

```

from typing import Literal

def definir_status(status: Literal["ativo", "inativo"]) -> None:
    print(f"Status definido para: {status}")

definir_status("ativo")   # ✅ válido
definir_status("inativo") # ✅ válido
definir_status("pendente") # ❌ erro em checagem de tipo (mypy ou Pyright)

```

Exemplo com Pydantic

```

from pydantic import BaseModel
from typing import Literal

class Config(BaseModel):
    modo: Literal["simples", "avançado"]

cfg = Config(modo="simples")  # ✅ ok
cfg = Config(modo="errado")   # ❌ ValidationError no Pydantic

```

## Union

Indica que um valor pode ser de mais de um tipo
É como dizer “ou isso, ou aquilo”

```

from typing import Union

def mostrar(valor: Union[int, str]) -> None:
    print(f"Valor: {valor}")

mostrar(42)      # ✅ ok
mostrar("texto") # ✅ ok
mostrar(3.14)    # ❌ erro na checagem de tipo

```

Exemplo prático com Pydantic (múltiplas formas de dado)

```

from pydantic import BaseModel
from typing import Union

class Pessoa(BaseModel):
    nome: str
    idade: int

class Empresa(BaseModel):
    razao_social: str
    cnpj: str

Entidade = Union[Pessoa, Empresa]

# Podemos criar tanto Pessoa quanto Empresa
e1 = Pessoa(nome="João", idade=30)
e2 = Empresa(razao_social="Tech Ltda", cnpj="00.000.000/0001-00")

# Como Entidade aceita os dois:
entidades: list[Entidade] = [e1, e2]
print(entidades)


```

## Reflection, introspecção

NOTA, o GL tem recurso de "reflection", introspecção, acessando os tipos das anotações, em tempo de runtime, para validar

```

from pydantic import BaseModel
from typing import Literal, Union

class ASpeech(BaseModel):
    tag: Literal["ASpeech"]
    texto: str

class AGesture(BaseModel):
    tag: Literal["AGesture"]
    gesto: str

AAction = Union[ASpeech, AGesture]

class TPeAction(BaseModel):
    tpaAction: AAction

# Testando
acao_fala = {"tpaAction": {"tag": "ASpeech", "texto": "Olá"}}
acao_gesto = {"tpaAction": {"tag": "AGesture", "gesto": "Acenar"}}

print(TPeAction(**acao_fala))   # vira ASpeech
print(TPeAction(**acao_gesto))  # vira AGesture


```

aqui, pelo campo "tag" que é uma Literal, ele consegue instanciar corretamente o tipo ao ler o json

### Mais aprofundamento sobre essa introspecção:

O Python guarda as anotações de tipo (Literal, Union, int, str etc.) na memória em tempo de execução

Quando você declara algo assim:

```
def soma(a: int, b: int) -> int:
    return a + b
```

O Python não vai forçar a e b a serem int.
Mas ele guarda essa informação na função, no atributo __annotations__:

```
print(soma.__annotations__)
# {'a': <class 'int'>, 'b': <class 'int'>, 'return': <class 'int'>}
```

O Pydantic consegue instanciar corretamente pelo campo `tag` do exemplo, porque o pydantic consulta as anotações de tipo (__annotations__) para cada campo, e valida e converte os dados de acordo com o tipo informado