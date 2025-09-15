#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
from dataclasses import dataclass
from typing import List, Optional, Tuple

# -------------------------
# Token & códigos (según PDF)
# -------------------------
# Valores de tipo (según el pdf "simbolos_lexicos.pdf")
TOKEN_CODES = {
    "IDENTIFIER": 0,
    "INT": 1,
    "REAL": 2,
    "STRING": 3,
    "TYPE": 4,       # int, float, void (PDF menciona void en la tabla)
    "OP_SUMA": 5,    # +, -
    "OP_MUL": 6,     # *, /
    "OP_REL": 7,     # <, <=, >, >=
    "OP_OR": 8,      # ||
    "OP_AND": 9,     # &&
    "OP_NOT": 10,    # !
    "OP_IGUAL": 11,  # ==, !=
    "SEMICOLON": 12, # ;
    "COMMA": 13,     # ,
    "LPAREN": 14,    # (
    "RPAREN": 15,    # )
    "LBRACE": 16,    # {
    "RBRACE": 17,    # }
    "ASSIGN": 18,    # =
    "IF": 19,
    "WHILE": 20,
    "RETURN": 21,
    "ELSE": 22,
    "DOLLAR": 23
}

# Map de palabras reservadas a tipos/códigos
RESERVED = {
    "if": ("IF", TOKEN_CODES["IF"]),
    "while": ("WHILE", TOKEN_CODES["WHILE"]),
    "return": ("RETURN", TOKEN_CODES["RETURN"]),
    "else": ("ELSE", TOKEN_CODES["ELSE"]),
    "int": ("TYPE", TOKEN_CODES["TYPE"]),
    "float": ("TYPE", TOKEN_CODES["TYPE"]),
    "void": ("TYPE", TOKEN_CODES["TYPE"])  # si se usa
}

# Mapeo de operadores simples a (tipo_nombre, código)
OPERATORS = {
    "+": ("OP_SUMA", TOKEN_CODES["OP_SUMA"]),
    "-": ("OP_SUMA", TOKEN_CODES["OP_SUMA"]),
    "*": ("OP_MUL", TOKEN_CODES["OP_MUL"]),
    "/": ("OP_MUL", TOKEN_CODES["OP_MUL"]),
    "=": ("ASSIGN", TOKEN_CODES["ASSIGN"]),
    "==": ("OP_IGUAL", TOKEN_CODES["OP_IGUAL"]),
    "!=": ("OP_IGUAL", TOKEN_CODES["OP_IGUAL"]),
    "<": ("OP_REL", TOKEN_CODES["OP_REL"]),
    "<=": ("OP_REL", TOKEN_CODES["OP_REL"]),
    ">": ("OP_REL", TOKEN_CODES["OP_REL"]),
    ">=": ("OP_REL", TOKEN_CODES["OP_REL"]),
    "&&": ("OP_AND", TOKEN_CODES["OP_AND"]),
    "||": ("OP_OR", TOKEN_CODES["OP_OR"]),
    "!": ("OP_NOT", TOKEN_CODES["OP_NOT"]),
    ";": ("SEMICOLON", TOKEN_CODES["SEMICOLON"]),
    ",": ("COMMA", TOKEN_CODES["COMMA"]),
    "(": ("LPAREN", TOKEN_CODES["LPAREN"]),
    ")": ("RPAREN", TOKEN_CODES["RPAREN"]),
    "{": ("LBRACE", TOKEN_CODES["LBRACE"]),
    "}": ("RBRACE", TOKEN_CODES["RBRACE"])
}

# -------------------------
# Token dataclass
# -------------------------
@dataclass
class Token:
    lexeme: str
    type_name: str
    code: int
    line: int
    column: int

    def __repr__(self):
        return f"{self.lexeme!r} | {self.type_name} | {self.code} (Ln {self.line}, Col {self.column})"


# -------------------------
# Lexer
# -------------------------
class LexerError(Exception):
    pass

class Lexer:
    """
    Analizador léxico simple:
    - reconoce identificadores, enteros, reales, strings simples (double quotes)
    - operadores y símbolos definidos en OPERATORS
    - palabras reservadas en RESERVED
    - reporta errores con línea y columna
    """

    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.line = 1
        self.col = 1
        self.length = len(text)

        # Patterns
        self._re_whitespace = re.compile(r"\s+")
        self._re_identifier = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
        self._re_int = re.compile(r"\d+")
        self._re_real = re.compile(r"\d+\.\d+")
        self._re_string = re.compile(r'"([^"\\]|\\.)*"')
        # multi-char operators first (order matters)
        self._multi_ops = ["==", "!=", "<=", ">=", "&&", "||"]

    def _peek(self, n=1) -> str:
        if self.pos + n - 1 >= self.length:
            return ''
        return self.text[self.pos:self.pos + n]

    def _advance(self, n=1):
        for _ in range(n):
            if self.pos >= self.length:
                return
            ch = self.text[self.pos]
            self.pos += 1
            if ch == '\n':
                self.line += 1
                self.col = 1
            else:
                self.col += 1

    def _skip_whitespace_and_comments(self):
        while self.pos < self.length:
            m = self._re_whitespace.match(self.text, self.pos)
            if m:
                adv = m.end() - m.start()
                # update line/col properly
                chunk = self.text[self.pos:self.pos+adv]
                lines = chunk.splitlines(True)
                if len(lines) > 1:
                    self.line += len(lines) - 1
                    self.col = 1 + len(lines[-1])
                else:
                    self.col += adv
                self.pos += adv
                continue
            # soportar comentarios tipo // ... hasta el final de línea
            if self._peek(2) == "//":
                while self.pos < self.length and self.text[self.pos] != '\n':
                    self.pos += 1
                continue
            # soportar comentarios tipo /* ... */
            if self._peek(2) == "/*":
                self.pos += 2
                while self.pos < self.length and self._peek(2) != "*/":
                    if self.text[self.pos] == '\n':
                        self.line += 1
                        self.col = 1
                        self.pos += 1
                    else:
                        self.pos += 1
                if self._peek(2) == "*/":
                    self.pos += 2
                else:
                    raise LexerError(f"Comentario sin cerrar (línea {self.line})")
                continue
            break

    def next_token(self) -> Optional[Token]:
        self._skip_whitespace_and_comments()
        if self.pos >= self.length:
            return None

        start_pos = self.pos
        start_line = self.line
        start_col = self.col

        # Multi-char operators
        for op in self._multi_ops:
            if self._peek(len(op)) == op:
                self._advance(len(op))
                tname, code = OPERATORS[op]
                return Token(op, tname, code, start_line, start_col)

        ch = self._peek(1)
        # String literal
        if ch == '"':
            m = self._re_string.match(self.text, self.pos)
            if not m:
                raise LexerError(f"String mal formado en línea {start_line}, columna {start_col}")
            lex = m.group(0)
            self._advance(len(lex))
            return Token(lex, "STRING", TOKEN_CODES["STRING"], start_line, start_col)

        # Real (must check before int)
        m_real = self._re_real.match(self.text, self.pos)
        if m_real:
            lex = m_real.group(0)
            self._advance(len(lex))
            return Token(lex, "REAL", TOKEN_CODES["REAL"], start_line, start_col)

        # Integer
        m_int = self._re_int.match(self.text, self.pos)
        if m_int:
            lex = m_int.group(0)
            self._advance(len(lex))
            return Token(lex, "INT", TOKEN_CODES["INT"], start_line, start_col)

        # Identifier or reserved
        m_id = self._re_identifier.match(self.text, self.pos)
        if m_id:
            lex = m_id.group(0)
            self._advance(len(lex))
            lower = lex.lower()
            if lower in RESERVED:
                type_name, code = RESERVED[lower]
                return Token(lex, type_name, code, start_line, start_col)
            else:
                return Token(lex, "IDENTIFIER", TOKEN_CODES["IDENTIFIER"], start_line, start_col)

        # Single-char operators and punctuation
        ch1 = self._peek(1)
        if ch1 and ch1 in OPERATORS:
            self._advance(1)
            tname, code = OPERATORS[ch1]
            return Token(ch1, tname, code, start_line, start_col)

        # Si llegamos aquí: carácter no válido
        raise LexerError(f"Carácter inválido '{ch}' en línea {start_line}, columna {start_col}")

    def tokenize(self) -> List[Token]:
        tokens = []
        while True:
            tok = self.next_token()
            if tok is None:
                break
            tokens.append(tok)
        # Append end-of-input token
        tokens.append(Token("$", "DOLLAR", TOKEN_CODES["DOLLAR"], self.line, self.col))
        return tokens


# -------------------------
# Parser (Descenso recursivo)
# Gramática simplificada (ejemplo):
# program -> stmt_list
# stmt_list -> stmt stmt_list | ε
# stmt -> decl ';' | assign ';' | if_stmt | while_stmt | return_stmt ';' | block
# decl -> TYPE IDENTIFIER ( '=' expr )?
# assign -> IDENTIFIER '=' expr
# if_stmt -> 'if' '(' expr ')' stmt ('else' stmt)?
# while_stmt -> 'while' '(' expr ')' stmt
# return_stmt -> 'return' expr?
# block -> '{' stmt_list '}'
# expr -> equality
# equality -> relational ( (== | !=) relational )*
# relational -> sum ( (<|>|<=|>=) sum )*
# sum -> term ( (+|-) term )*
# term -> factor ( (*|/) factor )*
# factor -> ('+'|'-'|'!') factor | INT | REAL | IDENTIFIER | '(' expr ')' | STRING
# -------------------------

class ParseError(Exception):
    pass

class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0
        self.errors: List[str] = []

    def _peek(self) -> Token:
        if self.pos >= len(self.tokens):
            return self.tokens[-1]
        return self.tokens[self.pos]

    def _advance(self):
        if self.pos < len(self.tokens):
            self.pos += 1

    def _expect(self, type_name: str):
        tok = self._peek()
        if tok.type_name == type_name:
            self._advance()
            return tok
        raise ParseError(f"Se esperaba {type_name} en línea {tok.line}, columna {tok.column}, se obtuvo '{tok.lexeme}'")

    def parse(self):
        try:
            self.program()
            # si hay tokens sin consumir (excepto DOLLAR) -> error
            if self._peek().type_name != "DOLLAR":
                t = self._peek()
                raise ParseError(f"Token inesperado {t.lexeme!r} en línea {t.line}, columna {t.column}")
            return True, self.errors
        except ParseError as e:
            self.errors.append(str(e))
            return False, self.errors

    # program -> stmt_list
    def program(self):
        self.stmt_list()

    def stmt_list(self):
        while True:
            tok = self._peek()
            if tok.type_name in ("RBRACE", "DOLLAR"):
                return
            self.stmt()

    def stmt(self):
        tok = self._peek()
        if tok.type_name == "TYPE":      # declaración
            self.decl()
            self._expect("SEMICOLON")
        elif tok.type_name == "IDENTIFIER":
            # could be assignment or function call (not implemented)
            # Lookahead to see if '=' follows
            if self._lookahead_is_assign():
                self.assign()
                self._expect("SEMICOLON")
            else:
                # For now treat as error (function calls not supported)
                t = self._peek()
                raise ParseError(f"Uso de identificador inesperado '{t.lexeme}' en línea {t.line}, col {t.column}")
        elif tok.type_name == "IF":
            self.if_stmt()
        elif tok.type_name == "WHILE":
            self.while_stmt()
        elif tok.type_name == "RETURN":
            self.return_stmt()
            self._expect("SEMICOLON")
        elif tok.type_name == "LBRACE":
            self.block()
        else:
            t = self._peek()
            raise ParseError(f"Sentencia inválida que comienza con '{t.lexeme}' en línea {t.line}, col {t.column}")

    def decl(self):
        # decl -> TYPE IDENTIFIER ( '=' expr )?
        self._expect("TYPE")
        ident = self._expect("IDENTIFIER")
        if self._peek().type_name == "ASSIGN":
            self._advance()
            self.expr()

    def _lookahead_is_assign(self) -> bool:
        # Si pattern IDENTIFIER ASSIGN
        if self.pos + 1 < len(self.tokens) and self.tokens[self.pos + 1].type_name == "ASSIGN":
            return True
        return False

    def assign(self):
        # assign -> IDENTIFIER '=' expr
        self._expect("IDENTIFIER")
        self._expect("ASSIGN")
        self.expr()

    def if_stmt(self):
        self._expect("IF")
        self._expect("LPAREN")
        self.expr()
        self._expect("RPAREN")
        self.stmt()
        if self._peek().type_name == "ELSE":
            self._advance()
            self.stmt()

    def while_stmt(self):
        self._expect("WHILE")
        self._expect("LPAREN")
        self.expr()
        self._expect("RPAREN")
        self.stmt()

    def return_stmt(self):
        self._expect("RETURN")
        # RETURN may have optional expression
        if self._peek().type_name not in ("SEMICOLON", "RBRACE", "DOLLAR"):
            self.expr()

    def block(self):
        self._expect("LBRACE")
        self.stmt_list()
        self._expect("RBRACE")

    # EXPRESSION PARSING (precedence)
    def expr(self):
        self.equality()

    def equality(self):
        self.relational()
        while self._peek().type_name == "OP_IGUAL":
            self._advance()
            self.relational()

    def relational(self):
        self.sum_expr()
        while self._peek().type_name == "OP_REL":
            self._advance()
            self.sum_expr()

    def sum_expr(self):
        # + and - are OP_SUMA tokens
        self.term()
        while self._peek().type_name == "OP_SUMA":
            self._advance()
            self.term()

    def term(self):
        self.factor()
        while self._peek().type_name == "OP_MUL":
            self._advance()
            self.factor()

    def factor(self):
        tok = self._peek()
        # unary + - !
        if tok.lexeme in ("+", "-", "!"):
            self._advance()
            self.factor()
            return
        if tok.type_name in ("INT", "REAL", "IDENTIFIER", "STRING"):
            self._advance()
            return
        if tok.type_name == "LPAREN":
            self._advance()
            self.expr()
            self._expect("RPAREN")
            return
        raise ParseError(f"Factor inesperado '{tok.lexeme}' en línea {tok.line}, col {tok.column}")


# -------------------------
# Utility: run lexer+parser on a file/text
# -------------------------
def analyze_text(text: str) -> Tuple[List[Token], bool, List[str]]:
    lexer = Lexer(text)
    try:
        tokens = lexer.tokenize()
    except LexerError as le:
        return [], False, [str(le)]

    parser = Parser(tokens)
    ok, errors = parser.parse()
    return tokens, ok, errors

# -------------------------
# Ejecución ejemplo (si se ejecuta directamente)
# -------------------------
if __name__ == "__main__":
    example = """
    // Ejemplo correcto
    int x = 23;
    float y = 1.73;
    if (x > 0) {
        y = y + 2.0;
    } else {
        return;
    }

    // Ejemplo con error léxico
    int ?bad = 5;
    """

    tokens, ok, errors = analyze_text(example)
    print("---- TOKENS ----")
    for t in tokens:
        print(t)
    print("---- PARSER ----")
    print("Analizado correctamente?" , ok)
    if errors:
        print("Errores:")
        for e in errors:
            print(" -", e)
