module Types

type TokenType =
    | Number
    | Plus
    | Minus
    | Multiply
    | Divide
    | LeftParen
    | RightParen
    | EOI
    | Power
    | Modulo
    | Dot
    | Identifier
    | Equals
    | Semicolon

type Token = {
    Type: TokenType
    Value: string
    Position: int
}