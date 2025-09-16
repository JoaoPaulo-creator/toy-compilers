package titania

import "core:fmt"
import "core:unicode/utf8"

Pos :: struct {
	offset: int,
	line:   int,
	column: int,
}

Token :: struct {
	using pos: Pos,
	kind:      Token_Kind,
	text:      string,
}


Tokenizer :: struct {
	using pos:        Pos,
	data:             string,
	filename:         string,
	ch:               rune, // current char
	w:                int, // current rune width in bytes
	curr_line_offset: int,
}

Token_Kind :: enum {
	Invalid,
	EOF,
	Add,
	Sub,
	Mul,
	Quo,
	Mod,
	Dot,
	Comma,
	Semicolon,
	Vertical_Bar,
	Paren_Open,
	Paren_Close,
	Bracket_Open,
	Bracket_Close,
	Brace_Open,
	Brace_Close,
	Assign,
	Caret,
	Equal,
	Not_Equal,
	Less_Than,
	Greater_Than,
	Less_Than_Equal,
	Greater_Than_Equal,
	Ellipsis,
	Colon,
	Ident,
	Integer,
	Real,
	String,
	And,
	Begin,
	By,
	Case,
	Const,
	Do,
	Else,
	Elseif,
	End,
	False,
	For,
	If,
	Import,
	In,
	Is,
	Module,
	Nil,
	Not,
	Of,
	Or,
	Proc,
	Record,
	Repeat,
	Return,
	Then,
	To,
	True,
	Type,
	Until,
	Var,
	While,
}

@(rodata)
token_kin_string := [Token_kind]string {
	.Invalid            = "",
	.EOF                = "eof",
	.Add                = "+",
	.Sub                = "-",
	.Mul                = "*",
	.Quo                = "/",
	.Mod                = "%",
	.Dot                = ".",
	.Comma              = ",",
	.Semicolon          = ";",
	.Vertical_Bar       = "|",
	.Paren_Open         = "(",
	.Paren_Close        = ")",
	.Bracket_Open       = "[",
	.Bracket_Close      = "]",
	.Brace_Open         = "{",
	.Brace_Close        = "}",
	.Assign             = ":=",
	.Caret              = "^",
	.Equal              = "=",
	.Not_Equal          = "<>",
	.Less_Than          = "<",
	.Greater_Than       = ">",
	.Less_Than_Equal    = "<=",
	.Greater_Than_Equal = ">=",
	.Ellipsis           = "..",
	.Colon              = ":",
	.Ident              = "identifier",
	.Integer            = "integer literal",
	.Real               = "real literal",
	.String             = "string literal",
	.And                = "and",
	.Begin              = "begin",
	.By                 = "by",
	.Case               = "case",
	.Const              = "const",
	.Do                 = "do",
	.Else               = "else",
	.Elseif             = "elseif",
	.End                = "end",
	.False              = "false",
	.For                = "for",
	.If                 = "if",
	.Import             = "import",
	.In                 = "in",
	.Is                 = "is",
	.Module             = "module",
	.Nil                = "nil",
	.Not                = "not",
	.Of                 = "of",
	.Or                 = "or",
	.Proc               = "proc",
	.Record             = "record",
	.Repeat             = "repeat",
	.Return             = "return",
	.Then               = "then",
	.To                 = "to",
	.True               = "true",
	.Type               = "type",
	.Until              = "until",
	.Var                = "var",
	.While              = "while",
}

tokenizer_init :: proc(t: ^Tokenizer, filename: string, data: string) {
	t^ = Tokenizer {
		pos = {line = 1},
		data = data,
		filename = filename,
	}

	next_rune(t)
	if t.ch == utf8.RUNE_BOM {
		next_rune(t)
	}
}

next_rune :: proc(t: ^Tokenizer) -> rune {
	if t.offset < len(t.data) {
		t.offset += t.w
		t.ch, t.w = utf8.decode_rune_in_string(t.data[t.offset:])
		t.pos.column = t.offset - t.curr_line_offset
	}

	if t.offset > len(t.data) {
		t.ch = utf8.RUNE_EOF
		t.w = 1
	}

	return t.ch
}
