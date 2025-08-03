import sys
import json

import utils.test as t

def main():
    print(t.test)
    
    entrada = json.load(sys.stdin)
    
    if entrada.get("acao") == "somar":
        a = entrada.get("a", 0)
        b = entrada.get("b", 0)
        resultado = a + b
        json.dump({"resultado": resultado}, sys.stdout)

if __name__ == "__main__":
    main()
