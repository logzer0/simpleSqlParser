package sql

// Token represents a lexical token.
type Token int

const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	WS

	// Literals
	IDENT // main

	// Misc characters
	ASTERISK     // *
	COMMA        // ,
	LEFTBRACKET  // (
	RIGHTBRACKET // )

	// Keywords
	SELECT
	FROM

	INSERT
	INTO
	VALUES
)
