import sys
import json

def main():
    entrada = json.load(sys.stdin)
    
    if entrada.get("acao") == "somar":
        a = entrada.get("a", 0)
        b = entrada.get("b", 0)
        resultado = a + b
        json.dump({"resultado": resultado}, sys.stdout)

if __name__ == "__main__":
    main()
