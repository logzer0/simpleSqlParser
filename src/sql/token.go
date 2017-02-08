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
	ASTERISK // *
	COMMA    // ,
	// Keywords
	SELECT
	FROM
	INSERT
	INTO
	VALUES
)

var tokenMap = map[Token]bool{
	ILLEGAL:  true,
	EOF:      true,
	WS:       true,
	ASTERISK: true,
	COMMA:    true,
	SELECT:   true,
	FROM:     true,
	INSERT:   true,
	INTO:     true,
	VALUES:   true,
}
