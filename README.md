# Analizador Léxico y Sintáctico en Python

## Descripción
Este proyecto implementa un componente de traductor (compilador/intérprete) para un lenguaje de programación simplificado.  
En esta fase se cubren dos etapas principales del proceso de traducción:

1. **Análisis Léxico (Lexer):** Convierte el código fuente en una secuencia de tokens.
2. **Análisis Sintáctico (Parser):** Verifica que la secuencia de tokens cumple con la gramática definida.

## Características
- Reconoce identificadores, enteros, reales y cadenas.
- Soporta palabras reservadas (`if`, `while`, `return`, `else`, `int`, `float`, `void`).
- Detecta operadores aritméticos, relacionales y lógicos.
- Maneja símbolos: paréntesis, llaves, punto y coma, coma.
- Reporta errores léxicos y sintácticos con línea y columna.

## Estructura del Proyecto
- `compilador.py` → Código fuente del analizador léxico y sintáctico.

## ▶Ejecución
1. Clonar el repositorio:
   ```bash
   https://github.com/LuisMa1602/avances
   cd analizador-lexico-sintactico
   ```

2. Ejecutar el analizador:
   ```bash
   python3 compiler_frontend.py
   ```

3. Para analizar un archivo de entrada:
   ```python
   from compiler_frontend import analyze_text

   with open("tests/ejemplo1.src", "r", encoding="utf-8") as f:
       code = f.read()

   tokens, ok, errors = analyze_text(code)
   for t in tokens:
       print(t)
   print("Resultado:", "Correcto" if ok else "Errores")
   print(errors)
   ```

## Ejemplos

### Ejemplo válido
```c
int x = 23;
float y = 1.73;
if (x > 0) { y = y + 2.0; } else { return; }
```

### Error léxico
```c
int ?bad = 5;
```

Salida:
```
Carácter inválido '?' en línea 1, columna 5
```

### Error sintáctico
```c
int x = 5
float y;
```

Salida:
```
Se esperaba ';' en línea 1, columna ...
```

## Conclusión
Este proyecto constituye la base de un compilador.  
Se puede extender para incluir generación de árboles sintácticos, análisis semántico y producción de código intermedio.
