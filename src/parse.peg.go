package main

import (
	"fmt"
	"math"
	"sort"
	"strconv"
)

const endSymbol rune = 1114112

/* The rule types inferred from the grammar are below. */
type pegRule uint8

const (
	ruleUnknown pegRule = iota
	ruleG
	ruleSelectStatement
	ruleColumns
	ruleParaColList
	ruleColList
	ruleColName
	ruleTable
	rulelineComment
	ruleblockComment
	rulews
	rule_
	ruleLetter
	ruleNumber
	ruleAlphaNum
	ruleAsterisk
	ruleInsertStatement
	rulePegText
	ruleAction0
	ruleAction1
	ruleAction2
)

var rul3s = [...]string{
	"Unknown",
	"G",
	"SelectStatement",
	"Columns",
	"ParaColList",
	"ColList",
	"ColName",
	"Table",
	"lineComment",
	"blockComment",
	"ws",
	"_",
	"Letter",
	"Number",
	"AlphaNum",
	"Asterisk",
	"InsertStatement",
	"PegText",
	"Action0",
	"Action1",
	"Action2",
}

type token32 struct {
	pegRule
	begin, end uint32
}

func (t *token32) String() string {
	return fmt.Sprintf("\x1B[34m%v\x1B[m %v %v", rul3s[t.pegRule], t.begin, t.end)
}

type node32 struct {
	token32
	up, next *node32
}

func (node *node32) print(pretty bool, buffer string) {
	var print func(node *node32, depth int)
	print = func(node *node32, depth int) {
		for node != nil {
			for c := 0; c < depth; c++ {
				fmt.Printf(" ")
			}
			rule := rul3s[node.pegRule]
			quote := strconv.Quote(string(([]rune(buffer)[node.begin:node.end])))
			if !pretty {
				fmt.Printf("%v %v\n", rule, quote)
			} else {
				fmt.Printf("\x1B[34m%v\x1B[m %v\n", rule, quote)
			}
			if node.up != nil {
				print(node.up, depth+1)
			}
			node = node.next
		}
	}
	print(node, 0)
}

func (node *node32) Print(buffer string) {
	node.print(false, buffer)
}

func (node *node32) PrettyPrint(buffer string) {
	node.print(true, buffer)
}

type tokens32 struct {
	tree []token32
}

func (t *tokens32) Trim(length uint32) {
	t.tree = t.tree[:length]
}

func (t *tokens32) Print() {
	for _, token := range t.tree {
		fmt.Println(token.String())
	}
}

func (t *tokens32) AST() *node32 {
	type element struct {
		node *node32
		down *element
	}
	tokens := t.Tokens()
	var stack *element
	for _, token := range tokens {
		if token.begin == token.end {
			continue
		}
		node := &node32{token32: token}
		for stack != nil && stack.node.begin >= token.begin && stack.node.end <= token.end {
			stack.node.next = node.up
			node.up = stack.node
			stack = stack.down
		}
		stack = &element{node: node, down: stack}
	}
	if stack != nil {
		return stack.node
	}
	return nil
}

func (t *tokens32) PrintSyntaxTree(buffer string) {
	t.AST().Print(buffer)
}

func (t *tokens32) PrettyPrintSyntaxTree(buffer string) {
	t.AST().PrettyPrint(buffer)
}

func (t *tokens32) Add(rule pegRule, begin, end, index uint32) {
	if tree := t.tree; int(index) >= len(tree) {
		expanded := make([]token32, 2*len(tree))
		copy(expanded, tree)
		t.tree = expanded
	}
	t.tree[index] = token32{
		pegRule: rule,
		begin:   begin,
		end:     end,
	}
}

func (t *tokens32) Tokens() []token32 {
	return t.tree
}

type SQL struct {
	SelectStatement
	InsertStatement

	Buffer string
	buffer []rune
	rules  [21]func() bool
	parse  func(rule ...int) error
	reset  func()
	Pretty bool
	tokens32
}

func (p *SQL) Parse(rule ...int) error {
	return p.parse(rule...)
}

func (p *SQL) Reset() {
	p.reset()
}

type textPosition struct {
	line, symbol int
}

type textPositionMap map[int]textPosition

func translatePositions(buffer []rune, positions []int) textPositionMap {
	length, translations, j, line, symbol := len(positions), make(textPositionMap, len(positions)), 0, 1, 0
	sort.Ints(positions)

search:
	for i, c := range buffer {
		if c == '\n' {
			line, symbol = line+1, 0
		} else {
			symbol++
		}
		if i == positions[j] {
			translations[positions[j]] = textPosition{line, symbol}
			for j++; j < length; j++ {
				if i != positions[j] {
					continue search
				}
			}
			break search
		}
	}

	return translations
}

type parseError struct {
	p   *SQL
	max token32
}

func (e *parseError) Error() string {
	tokens, error := []token32{e.max}, "\n"
	positions, p := make([]int, 2*len(tokens)), 0
	for _, token := range tokens {
		positions[p], p = int(token.begin), p+1
		positions[p], p = int(token.end), p+1
	}
	translations := translatePositions(e.p.buffer, positions)
	format := "parse error near %v (line %v symbol %v - line %v symbol %v):\n%v\n"
	if e.p.Pretty {
		format = "parse error near \x1B[34m%v\x1B[m (line %v symbol %v - line %v symbol %v):\n%v\n"
	}
	for _, token := range tokens {
		begin, end := int(token.begin), int(token.end)
		error += fmt.Sprintf(format,
			rul3s[token.pegRule],
			translations[begin].line, translations[begin].symbol,
			translations[end].line, translations[end].symbol,
			strconv.Quote(string(e.p.buffer[begin:end])))
	}

	return error
}

func (p *SQL) PrintSyntaxTree() {
	if p.Pretty {
		p.tokens32.PrettyPrintSyntaxTree(p.Buffer)
	} else {
		p.tokens32.PrintSyntaxTree(p.Buffer)
	}
}

func (p *SQL) Execute() {
	buffer, _buffer, text, begin, end := p.Buffer, p.buffer, "", 0, 0
	for _, token := range p.Tokens() {
		switch token.pegRule {

		case rulePegText:
			begin, end = int(token.begin), int(token.end)
			text = string(_buffer[begin:end])

		case ruleAction0:
			p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end])
		case ruleAction1:
			p.SelectStatement.TableName = buffer[begin:end]
		case ruleAction2:
			p.SelectStatement.AllColumns = true

		}
	}
	_, _, _, _, _ = buffer, _buffer, text, begin, end
}

func (p *SQL) Init() {
	var (
		max                  token32
		position, tokenIndex uint32
		buffer               []rune
	)
	p.reset = func() {
		max = token32{}
		position, tokenIndex = 0, 0

		p.buffer = []rune(p.Buffer)
		if len(p.buffer) == 0 || p.buffer[len(p.buffer)-1] != endSymbol {
			p.buffer = append(p.buffer, endSymbol)
		}
		buffer = p.buffer
	}
	p.reset()

	_rules := p.rules
	tree := tokens32{tree: make([]token32, math.MaxInt16)}
	p.parse = func(rule ...int) error {
		r := 1
		if len(rule) > 0 {
			r = rule[0]
		}
		matches := p.rules[r]()
		p.tokens32 = tree
		if matches {
			p.Trim(tokenIndex)
			return nil
		}
		return &parseError{p, max}
	}

	add := func(rule pegRule, begin uint32) {
		tree.Add(rule, begin, position, tokenIndex)
		tokenIndex++
		if begin != position && position > max.end {
			max = token32{rule, begin, position}
		}
	}

	matchDot := func() bool {
		if buffer[position] != endSymbol {
			position++
			return true
		}
		return false
	}

	/*matchChar := func(c byte) bool {
		if buffer[position] == c {
			position++
			return true
		}
		return false
	}*/

	/*matchRange := func(lower byte, upper byte) bool {
		if c := buffer[position]; c >= lower && c <= upper {
			position++
			return true
		}
		return false
	}*/

	_rules = [...]func() bool{
		nil,
		/* 0 G <- <(SelectStatement / (InsertStatement !.))> */
		func() bool {
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				{
					position2, tokenIndex2 := position, tokenIndex
					if !_rules[ruleSelectStatement]() {
						goto l3
					}
					goto l2
				l3:
					position, tokenIndex = position2, tokenIndex2
					if !_rules[ruleInsertStatement]() {
						goto l0
					}
					{
						position4, tokenIndex4 := position, tokenIndex
						if !matchDot() {
							goto l4
						}
						goto l0
					l4:
						position, tokenIndex = position4, tokenIndex4
					}
				}
			l2:
				add(ruleG, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 SelectStatement <- <(('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T') _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ Table _ ';')> */
		func() bool {
			position5, tokenIndex5 := position, tokenIndex
			{
				position6 := position
				{
					position7, tokenIndex7 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l8
					}
					position++
					goto l7
				l8:
					position, tokenIndex = position7, tokenIndex7
					if buffer[position] != rune('S') {
						goto l5
					}
					position++
				}
			l7:
				{
					position9, tokenIndex9 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l10
					}
					position++
					goto l9
				l10:
					position, tokenIndex = position9, tokenIndex9
					if buffer[position] != rune('E') {
						goto l5
					}
					position++
				}
			l9:
				{
					position11, tokenIndex11 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l12
					}
					position++
					goto l11
				l12:
					position, tokenIndex = position11, tokenIndex11
					if buffer[position] != rune('L') {
						goto l5
					}
					position++
				}
			l11:
				{
					position13, tokenIndex13 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l14
					}
					position++
					goto l13
				l14:
					position, tokenIndex = position13, tokenIndex13
					if buffer[position] != rune('E') {
						goto l5
					}
					position++
				}
			l13:
				{
					position15, tokenIndex15 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l16
					}
					position++
					goto l15
				l16:
					position, tokenIndex = position15, tokenIndex15
					if buffer[position] != rune('C') {
						goto l5
					}
					position++
				}
			l15:
				{
					position17, tokenIndex17 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l18
					}
					position++
					goto l17
				l18:
					position, tokenIndex = position17, tokenIndex17
					if buffer[position] != rune('T') {
						goto l5
					}
					position++
				}
			l17:
				if !_rules[rule_]() {
					goto l5
				}
				if !_rules[ruleColumns]() {
					goto l5
				}
				if !_rules[rule_]() {
					goto l5
				}
				{
					position19, tokenIndex19 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l20
					}
					position++
					goto l19
				l20:
					position, tokenIndex = position19, tokenIndex19
					if buffer[position] != rune('F') {
						goto l5
					}
					position++
				}
			l19:
				{
					position21, tokenIndex21 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l22
					}
					position++
					goto l21
				l22:
					position, tokenIndex = position21, tokenIndex21
					if buffer[position] != rune('R') {
						goto l5
					}
					position++
				}
			l21:
				{
					position23, tokenIndex23 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l24
					}
					position++
					goto l23
				l24:
					position, tokenIndex = position23, tokenIndex23
					if buffer[position] != rune('O') {
						goto l5
					}
					position++
				}
			l23:
				{
					position25, tokenIndex25 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l26
					}
					position++
					goto l25
				l26:
					position, tokenIndex = position25, tokenIndex25
					if buffer[position] != rune('M') {
						goto l5
					}
					position++
				}
			l25:
				if !_rules[rule_]() {
					goto l5
				}
				if !_rules[ruleTable]() {
					goto l5
				}
				if !_rules[rule_]() {
					goto l5
				}
				if buffer[position] != rune(';') {
					goto l5
				}
				position++
				add(ruleSelectStatement, position6)
			}
			return true
		l5:
			position, tokenIndex = position5, tokenIndex5
			return false
		},
		/* 2 Columns <- <(ParaColList / ColList / Asterisk)> */
		func() bool {
			position27, tokenIndex27 := position, tokenIndex
			{
				position28 := position
				{
					position29, tokenIndex29 := position, tokenIndex
					if !_rules[ruleParaColList]() {
						goto l30
					}
					goto l29
				l30:
					position, tokenIndex = position29, tokenIndex29
					if !_rules[ruleColList]() {
						goto l31
					}
					goto l29
				l31:
					position, tokenIndex = position29, tokenIndex29
					if !_rules[ruleAsterisk]() {
						goto l27
					}
				}
			l29:
				add(ruleColumns, position28)
			}
			return true
		l27:
			position, tokenIndex = position27, tokenIndex27
			return false
		},
		/* 3 ParaColList <- <('(' _ ColList _ ')')> */
		func() bool {
			position32, tokenIndex32 := position, tokenIndex
			{
				position33 := position
				if buffer[position] != rune('(') {
					goto l32
				}
				position++
				if !_rules[rule_]() {
					goto l32
				}
				if !_rules[ruleColList]() {
					goto l32
				}
				if !_rules[rule_]() {
					goto l32
				}
				if buffer[position] != rune(')') {
					goto l32
				}
				position++
				add(ruleParaColList, position33)
			}
			return true
		l32:
			position, tokenIndex = position32, tokenIndex32
			return false
		},
		/* 4 ColList <- <((ColName _ ',' _)* ColName)> */
		func() bool {
			position34, tokenIndex34 := position, tokenIndex
			{
				position35 := position
			l36:
				{
					position37, tokenIndex37 := position, tokenIndex
					if !_rules[ruleColName]() {
						goto l37
					}
					if !_rules[rule_]() {
						goto l37
					}
					if buffer[position] != rune(',') {
						goto l37
					}
					position++
					if !_rules[rule_]() {
						goto l37
					}
					goto l36
				l37:
					position, tokenIndex = position37, tokenIndex37
				}
				if !_rules[ruleColName]() {
					goto l34
				}
				add(ruleColList, position35)
			}
			return true
		l34:
			position, tokenIndex = position34, tokenIndex34
			return false
		},
		/* 5 ColName <- <(<AlphaNum+> Action0)> */
		func() bool {
			position38, tokenIndex38 := position, tokenIndex
			{
				position39 := position
				{
					position40 := position
					if !_rules[ruleAlphaNum]() {
						goto l38
					}
				l41:
					{
						position42, tokenIndex42 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l42
						}
						goto l41
					l42:
						position, tokenIndex = position42, tokenIndex42
					}
					add(rulePegText, position40)
				}
				if !_rules[ruleAction0]() {
					goto l38
				}
				add(ruleColName, position39)
			}
			return true
		l38:
			position, tokenIndex = position38, tokenIndex38
			return false
		},
		/* 6 Table <- <(<(('t' / 'T') ('b' / 'B') ('l' / 'L'))> Action1)> */
		func() bool {
			position43, tokenIndex43 := position, tokenIndex
			{
				position44 := position
				{
					position45 := position
					{
						position46, tokenIndex46 := position, tokenIndex
						if buffer[position] != rune('t') {
							goto l47
						}
						position++
						goto l46
					l47:
						position, tokenIndex = position46, tokenIndex46
						if buffer[position] != rune('T') {
							goto l43
						}
						position++
					}
				l46:
					{
						position48, tokenIndex48 := position, tokenIndex
						if buffer[position] != rune('b') {
							goto l49
						}
						position++
						goto l48
					l49:
						position, tokenIndex = position48, tokenIndex48
						if buffer[position] != rune('B') {
							goto l43
						}
						position++
					}
				l48:
					{
						position50, tokenIndex50 := position, tokenIndex
						if buffer[position] != rune('l') {
							goto l51
						}
						position++
						goto l50
					l51:
						position, tokenIndex = position50, tokenIndex50
						if buffer[position] != rune('L') {
							goto l43
						}
						position++
					}
				l50:
					add(rulePegText, position45)
				}
				if !_rules[ruleAction1]() {
					goto l43
				}
				add(ruleTable, position44)
			}
			return true
		l43:
			position, tokenIndex = position43, tokenIndex43
			return false
		},
		/* 7 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position52, tokenIndex52 := position, tokenIndex
			{
				position53 := position
				if buffer[position] != rune('/') {
					goto l52
				}
				position++
				if buffer[position] != rune('/') {
					goto l52
				}
				position++
			l54:
				{
					position55, tokenIndex55 := position, tokenIndex
					{
						position56, tokenIndex56 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l56
						}
						position++
						goto l55
					l56:
						position, tokenIndex = position56, tokenIndex56
					}
					if !matchDot() {
						goto l55
					}
					goto l54
				l55:
					position, tokenIndex = position55, tokenIndex55
				}
				add(rulelineComment, position53)
			}
			return true
		l52:
			position, tokenIndex = position52, tokenIndex52
			return false
		},
		/* 8 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position57, tokenIndex57 := position, tokenIndex
			{
				position58 := position
				if buffer[position] != rune('/') {
					goto l57
				}
				position++
				if buffer[position] != rune('*') {
					goto l57
				}
				position++
			l59:
				{
					position60, tokenIndex60 := position, tokenIndex
					{
						position61, tokenIndex61 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l61
						}
						position++
						if buffer[position] != rune('/') {
							goto l61
						}
						position++
						goto l60
					l61:
						position, tokenIndex = position61, tokenIndex61
					}
					{
						position62, tokenIndex62 := position, tokenIndex
						if !matchDot() {
							goto l63
						}
						goto l62
					l63:
						position, tokenIndex = position62, tokenIndex62
						if buffer[position] != rune('\n') {
							goto l60
						}
						position++
					}
				l62:
					goto l59
				l60:
					position, tokenIndex = position60, tokenIndex60
				}
				if buffer[position] != rune('*') {
					goto l57
				}
				position++
				if buffer[position] != rune('/') {
					goto l57
				}
				position++
				add(ruleblockComment, position58)
			}
			return true
		l57:
			position, tokenIndex = position57, tokenIndex57
			return false
		},
		/* 9 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position64, tokenIndex64 := position, tokenIndex
			{
				position65 := position
				{
					position66, tokenIndex66 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l67
					}
					position++
					goto l66
				l67:
					position, tokenIndex = position66, tokenIndex66
					if buffer[position] != rune('\t') {
						goto l68
					}
					position++
					goto l66
				l68:
					position, tokenIndex = position66, tokenIndex66
					if buffer[position] != rune('\n') {
						goto l69
					}
					position++
					goto l66
				l69:
					position, tokenIndex = position66, tokenIndex66
					if buffer[position] != rune('\r') {
						goto l64
					}
					position++
				}
			l66:
				add(rulews, position65)
			}
			return true
		l64:
			position, tokenIndex = position64, tokenIndex64
			return false
		},
		/* 10 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position71 := position
			l72:
				{
					position73, tokenIndex73 := position, tokenIndex
					{
						position74, tokenIndex74 := position, tokenIndex
						if !_rules[rulews]() {
							goto l75
						}
						goto l74
					l75:
						position, tokenIndex = position74, tokenIndex74
						if !_rules[rulelineComment]() {
							goto l76
						}
						goto l74
					l76:
						position, tokenIndex = position74, tokenIndex74
						if !_rules[ruleblockComment]() {
							goto l73
						}
					}
				l74:
					goto l72
				l73:
					position, tokenIndex = position73, tokenIndex73
				}
				add(rule_, position71)
			}
			return true
		},
		/* 11 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position77, tokenIndex77 := position, tokenIndex
			{
				position78 := position
				{
					position79, tokenIndex79 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l80
					}
					position++
					goto l79
				l80:
					position, tokenIndex = position79, tokenIndex79
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l81
					}
					position++
					goto l79
				l81:
					position, tokenIndex = position79, tokenIndex79
					if buffer[position] != rune('_') {
						goto l77
					}
					position++
				}
			l79:
				add(ruleLetter, position78)
			}
			return true
		l77:
			position, tokenIndex = position77, tokenIndex77
			return false
		},
		/* 12 Number <- <[0-9]> */
		func() bool {
			position82, tokenIndex82 := position, tokenIndex
			{
				position83 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l82
				}
				position++
				add(ruleNumber, position83)
			}
			return true
		l82:
			position, tokenIndex = position82, tokenIndex82
			return false
		},
		/* 13 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position84, tokenIndex84 := position, tokenIndex
			{
				position85 := position
				{
					position86, tokenIndex86 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l87
					}
					goto l86
				l87:
					position, tokenIndex = position86, tokenIndex86
					if !_rules[ruleNumber]() {
						goto l84
					}
				}
			l86:
				add(ruleAlphaNum, position85)
			}
			return true
		l84:
			position, tokenIndex = position84, tokenIndex84
			return false
		},
		/* 14 Asterisk <- <('*' Action2)> */
		func() bool {
			position88, tokenIndex88 := position, tokenIndex
			{
				position89 := position
				if buffer[position] != rune('*') {
					goto l88
				}
				position++
				if !_rules[ruleAction2]() {
					goto l88
				}
				add(ruleAsterisk, position89)
			}
			return true
		l88:
			position, tokenIndex = position88, tokenIndex88
			return false
		},
		/* 15 InsertStatement <- <(('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T') ' ' ('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O') ' ' ('t' / 'T') ('b' / 'B') ('l' / 'L') ' ' ('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S') '(' ' ' '1' ',' ' ' '"' ('a' / 'A') '"' ',' ' ' '1' '.' '2' '2' '4' ' ' ' ' ')' ';')> */
		func() bool {
			position90, tokenIndex90 := position, tokenIndex
			{
				position91 := position
				{
					position92, tokenIndex92 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l93
					}
					position++
					goto l92
				l93:
					position, tokenIndex = position92, tokenIndex92
					if buffer[position] != rune('I') {
						goto l90
					}
					position++
				}
			l92:
				{
					position94, tokenIndex94 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l95
					}
					position++
					goto l94
				l95:
					position, tokenIndex = position94, tokenIndex94
					if buffer[position] != rune('N') {
						goto l90
					}
					position++
				}
			l94:
				{
					position96, tokenIndex96 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l97
					}
					position++
					goto l96
				l97:
					position, tokenIndex = position96, tokenIndex96
					if buffer[position] != rune('S') {
						goto l90
					}
					position++
				}
			l96:
				{
					position98, tokenIndex98 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l99
					}
					position++
					goto l98
				l99:
					position, tokenIndex = position98, tokenIndex98
					if buffer[position] != rune('E') {
						goto l90
					}
					position++
				}
			l98:
				{
					position100, tokenIndex100 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l101
					}
					position++
					goto l100
				l101:
					position, tokenIndex = position100, tokenIndex100
					if buffer[position] != rune('R') {
						goto l90
					}
					position++
				}
			l100:
				{
					position102, tokenIndex102 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l103
					}
					position++
					goto l102
				l103:
					position, tokenIndex = position102, tokenIndex102
					if buffer[position] != rune('T') {
						goto l90
					}
					position++
				}
			l102:
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				{
					position104, tokenIndex104 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l105
					}
					position++
					goto l104
				l105:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('I') {
						goto l90
					}
					position++
				}
			l104:
				{
					position106, tokenIndex106 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l107
					}
					position++
					goto l106
				l107:
					position, tokenIndex = position106, tokenIndex106
					if buffer[position] != rune('N') {
						goto l90
					}
					position++
				}
			l106:
				{
					position108, tokenIndex108 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l109
					}
					position++
					goto l108
				l109:
					position, tokenIndex = position108, tokenIndex108
					if buffer[position] != rune('T') {
						goto l90
					}
					position++
				}
			l108:
				{
					position110, tokenIndex110 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l111
					}
					position++
					goto l110
				l111:
					position, tokenIndex = position110, tokenIndex110
					if buffer[position] != rune('O') {
						goto l90
					}
					position++
				}
			l110:
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				{
					position112, tokenIndex112 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l113
					}
					position++
					goto l112
				l113:
					position, tokenIndex = position112, tokenIndex112
					if buffer[position] != rune('T') {
						goto l90
					}
					position++
				}
			l112:
				{
					position114, tokenIndex114 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l115
					}
					position++
					goto l114
				l115:
					position, tokenIndex = position114, tokenIndex114
					if buffer[position] != rune('B') {
						goto l90
					}
					position++
				}
			l114:
				{
					position116, tokenIndex116 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l117
					}
					position++
					goto l116
				l117:
					position, tokenIndex = position116, tokenIndex116
					if buffer[position] != rune('L') {
						goto l90
					}
					position++
				}
			l116:
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				{
					position118, tokenIndex118 := position, tokenIndex
					if buffer[position] != rune('v') {
						goto l119
					}
					position++
					goto l118
				l119:
					position, tokenIndex = position118, tokenIndex118
					if buffer[position] != rune('V') {
						goto l90
					}
					position++
				}
			l118:
				{
					position120, tokenIndex120 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l121
					}
					position++
					goto l120
				l121:
					position, tokenIndex = position120, tokenIndex120
					if buffer[position] != rune('A') {
						goto l90
					}
					position++
				}
			l120:
				{
					position122, tokenIndex122 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l123
					}
					position++
					goto l122
				l123:
					position, tokenIndex = position122, tokenIndex122
					if buffer[position] != rune('L') {
						goto l90
					}
					position++
				}
			l122:
				{
					position124, tokenIndex124 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l125
					}
					position++
					goto l124
				l125:
					position, tokenIndex = position124, tokenIndex124
					if buffer[position] != rune('U') {
						goto l90
					}
					position++
				}
			l124:
				{
					position126, tokenIndex126 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l127
					}
					position++
					goto l126
				l127:
					position, tokenIndex = position126, tokenIndex126
					if buffer[position] != rune('E') {
						goto l90
					}
					position++
				}
			l126:
				{
					position128, tokenIndex128 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l129
					}
					position++
					goto l128
				l129:
					position, tokenIndex = position128, tokenIndex128
					if buffer[position] != rune('S') {
						goto l90
					}
					position++
				}
			l128:
				if buffer[position] != rune('(') {
					goto l90
				}
				position++
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				if buffer[position] != rune('1') {
					goto l90
				}
				position++
				if buffer[position] != rune(',') {
					goto l90
				}
				position++
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				if buffer[position] != rune('"') {
					goto l90
				}
				position++
				{
					position130, tokenIndex130 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l131
					}
					position++
					goto l130
				l131:
					position, tokenIndex = position130, tokenIndex130
					if buffer[position] != rune('A') {
						goto l90
					}
					position++
				}
			l130:
				if buffer[position] != rune('"') {
					goto l90
				}
				position++
				if buffer[position] != rune(',') {
					goto l90
				}
				position++
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				if buffer[position] != rune('1') {
					goto l90
				}
				position++
				if buffer[position] != rune('.') {
					goto l90
				}
				position++
				if buffer[position] != rune('2') {
					goto l90
				}
				position++
				if buffer[position] != rune('2') {
					goto l90
				}
				position++
				if buffer[position] != rune('4') {
					goto l90
				}
				position++
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				if buffer[position] != rune(' ') {
					goto l90
				}
				position++
				if buffer[position] != rune(')') {
					goto l90
				}
				position++
				if buffer[position] != rune(';') {
					goto l90
				}
				position++
				add(ruleInsertStatement, position91)
			}
			return true
		l90:
			position, tokenIndex = position90, tokenIndex90
			return false
		},
		nil,
		/* 18 Action0 <- <{ p.SelectStatement.Columns = append( p.SelectStatement.Columns  ,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 19 Action1 <- <{ p.SelectStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 20 Action2 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
	}
	p.rules = _rules
}
