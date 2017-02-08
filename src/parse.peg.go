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

	Buffer string
	buffer []rune
	rules  [20]func() bool
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
		/* 0 G <- <(SelectStatement !.)> */
		func() bool {
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				if !_rules[ruleSelectStatement]() {
					goto l0
				}
				{
					position2, tokenIndex2 := position, tokenIndex
					if !matchDot() {
						goto l2
					}
					goto l0
				l2:
					position, tokenIndex = position2, tokenIndex2
				}
				add(ruleG, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 SelectStatement <- <(('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T') _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ Table _ ';')> */
		func() bool {
			position3, tokenIndex3 := position, tokenIndex
			{
				position4 := position
				{
					position5, tokenIndex5 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l6
					}
					position++
					goto l5
				l6:
					position, tokenIndex = position5, tokenIndex5
					if buffer[position] != rune('S') {
						goto l3
					}
					position++
				}
			l5:
				{
					position7, tokenIndex7 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l8
					}
					position++
					goto l7
				l8:
					position, tokenIndex = position7, tokenIndex7
					if buffer[position] != rune('E') {
						goto l3
					}
					position++
				}
			l7:
				{
					position9, tokenIndex9 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l10
					}
					position++
					goto l9
				l10:
					position, tokenIndex = position9, tokenIndex9
					if buffer[position] != rune('L') {
						goto l3
					}
					position++
				}
			l9:
				{
					position11, tokenIndex11 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l12
					}
					position++
					goto l11
				l12:
					position, tokenIndex = position11, tokenIndex11
					if buffer[position] != rune('E') {
						goto l3
					}
					position++
				}
			l11:
				{
					position13, tokenIndex13 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l14
					}
					position++
					goto l13
				l14:
					position, tokenIndex = position13, tokenIndex13
					if buffer[position] != rune('C') {
						goto l3
					}
					position++
				}
			l13:
				{
					position15, tokenIndex15 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l16
					}
					position++
					goto l15
				l16:
					position, tokenIndex = position15, tokenIndex15
					if buffer[position] != rune('T') {
						goto l3
					}
					position++
				}
			l15:
				if !_rules[rule_]() {
					goto l3
				}
				if !_rules[ruleColumns]() {
					goto l3
				}
				if !_rules[rule_]() {
					goto l3
				}
				{
					position17, tokenIndex17 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l18
					}
					position++
					goto l17
				l18:
					position, tokenIndex = position17, tokenIndex17
					if buffer[position] != rune('F') {
						goto l3
					}
					position++
				}
			l17:
				{
					position19, tokenIndex19 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l20
					}
					position++
					goto l19
				l20:
					position, tokenIndex = position19, tokenIndex19
					if buffer[position] != rune('R') {
						goto l3
					}
					position++
				}
			l19:
				{
					position21, tokenIndex21 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l22
					}
					position++
					goto l21
				l22:
					position, tokenIndex = position21, tokenIndex21
					if buffer[position] != rune('O') {
						goto l3
					}
					position++
				}
			l21:
				{
					position23, tokenIndex23 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l24
					}
					position++
					goto l23
				l24:
					position, tokenIndex = position23, tokenIndex23
					if buffer[position] != rune('M') {
						goto l3
					}
					position++
				}
			l23:
				if !_rules[rule_]() {
					goto l3
				}
				if !_rules[ruleTable]() {
					goto l3
				}
				if !_rules[rule_]() {
					goto l3
				}
				if buffer[position] != rune(';') {
					goto l3
				}
				position++
				add(ruleSelectStatement, position4)
			}
			return true
		l3:
			position, tokenIndex = position3, tokenIndex3
			return false
		},
		/* 2 Columns <- <(ParaColList / ColList / Asterisk)> */
		func() bool {
			position25, tokenIndex25 := position, tokenIndex
			{
				position26 := position
				{
					position27, tokenIndex27 := position, tokenIndex
					if !_rules[ruleParaColList]() {
						goto l28
					}
					goto l27
				l28:
					position, tokenIndex = position27, tokenIndex27
					if !_rules[ruleColList]() {
						goto l29
					}
					goto l27
				l29:
					position, tokenIndex = position27, tokenIndex27
					if !_rules[ruleAsterisk]() {
						goto l25
					}
				}
			l27:
				add(ruleColumns, position26)
			}
			return true
		l25:
			position, tokenIndex = position25, tokenIndex25
			return false
		},
		/* 3 ParaColList <- <('(' _ ColList _ ')')> */
		func() bool {
			position30, tokenIndex30 := position, tokenIndex
			{
				position31 := position
				if buffer[position] != rune('(') {
					goto l30
				}
				position++
				if !_rules[rule_]() {
					goto l30
				}
				if !_rules[ruleColList]() {
					goto l30
				}
				if !_rules[rule_]() {
					goto l30
				}
				if buffer[position] != rune(')') {
					goto l30
				}
				position++
				add(ruleParaColList, position31)
			}
			return true
		l30:
			position, tokenIndex = position30, tokenIndex30
			return false
		},
		/* 4 ColList <- <((ColName _ ',' _)* ColName)> */
		func() bool {
			position32, tokenIndex32 := position, tokenIndex
			{
				position33 := position
			l34:
				{
					position35, tokenIndex35 := position, tokenIndex
					if !_rules[ruleColName]() {
						goto l35
					}
					if !_rules[rule_]() {
						goto l35
					}
					if buffer[position] != rune(',') {
						goto l35
					}
					position++
					if !_rules[rule_]() {
						goto l35
					}
					goto l34
				l35:
					position, tokenIndex = position35, tokenIndex35
				}
				if !_rules[ruleColName]() {
					goto l32
				}
				add(ruleColList, position33)
			}
			return true
		l32:
			position, tokenIndex = position32, tokenIndex32
			return false
		},
		/* 5 ColName <- <(<AlphaNum+> Action0)> */
		func() bool {
			position36, tokenIndex36 := position, tokenIndex
			{
				position37 := position
				{
					position38 := position
					if !_rules[ruleAlphaNum]() {
						goto l36
					}
				l39:
					{
						position40, tokenIndex40 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l40
						}
						goto l39
					l40:
						position, tokenIndex = position40, tokenIndex40
					}
					add(rulePegText, position38)
				}
				if !_rules[ruleAction0]() {
					goto l36
				}
				add(ruleColName, position37)
			}
			return true
		l36:
			position, tokenIndex = position36, tokenIndex36
			return false
		},
		/* 6 Table <- <(<(('t' / 'T') ('b' / 'B') ('l' / 'L'))> Action1)> */
		func() bool {
			position41, tokenIndex41 := position, tokenIndex
			{
				position42 := position
				{
					position43 := position
					{
						position44, tokenIndex44 := position, tokenIndex
						if buffer[position] != rune('t') {
							goto l45
						}
						position++
						goto l44
					l45:
						position, tokenIndex = position44, tokenIndex44
						if buffer[position] != rune('T') {
							goto l41
						}
						position++
					}
				l44:
					{
						position46, tokenIndex46 := position, tokenIndex
						if buffer[position] != rune('b') {
							goto l47
						}
						position++
						goto l46
					l47:
						position, tokenIndex = position46, tokenIndex46
						if buffer[position] != rune('B') {
							goto l41
						}
						position++
					}
				l46:
					{
						position48, tokenIndex48 := position, tokenIndex
						if buffer[position] != rune('l') {
							goto l49
						}
						position++
						goto l48
					l49:
						position, tokenIndex = position48, tokenIndex48
						if buffer[position] != rune('L') {
							goto l41
						}
						position++
					}
				l48:
					add(rulePegText, position43)
				}
				if !_rules[ruleAction1]() {
					goto l41
				}
				add(ruleTable, position42)
			}
			return true
		l41:
			position, tokenIndex = position41, tokenIndex41
			return false
		},
		/* 7 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position50, tokenIndex50 := position, tokenIndex
			{
				position51 := position
				if buffer[position] != rune('/') {
					goto l50
				}
				position++
				if buffer[position] != rune('/') {
					goto l50
				}
				position++
			l52:
				{
					position53, tokenIndex53 := position, tokenIndex
					{
						position54, tokenIndex54 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l54
						}
						position++
						goto l53
					l54:
						position, tokenIndex = position54, tokenIndex54
					}
					if !matchDot() {
						goto l53
					}
					goto l52
				l53:
					position, tokenIndex = position53, tokenIndex53
				}
				add(rulelineComment, position51)
			}
			return true
		l50:
			position, tokenIndex = position50, tokenIndex50
			return false
		},
		/* 8 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position55, tokenIndex55 := position, tokenIndex
			{
				position56 := position
				if buffer[position] != rune('/') {
					goto l55
				}
				position++
				if buffer[position] != rune('*') {
					goto l55
				}
				position++
			l57:
				{
					position58, tokenIndex58 := position, tokenIndex
					{
						position59, tokenIndex59 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l59
						}
						position++
						if buffer[position] != rune('/') {
							goto l59
						}
						position++
						goto l58
					l59:
						position, tokenIndex = position59, tokenIndex59
					}
					{
						position60, tokenIndex60 := position, tokenIndex
						if !matchDot() {
							goto l61
						}
						goto l60
					l61:
						position, tokenIndex = position60, tokenIndex60
						if buffer[position] != rune('\n') {
							goto l58
						}
						position++
					}
				l60:
					goto l57
				l58:
					position, tokenIndex = position58, tokenIndex58
				}
				if buffer[position] != rune('*') {
					goto l55
				}
				position++
				if buffer[position] != rune('/') {
					goto l55
				}
				position++
				add(ruleblockComment, position56)
			}
			return true
		l55:
			position, tokenIndex = position55, tokenIndex55
			return false
		},
		/* 9 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position62, tokenIndex62 := position, tokenIndex
			{
				position63 := position
				{
					position64, tokenIndex64 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l65
					}
					position++
					goto l64
				l65:
					position, tokenIndex = position64, tokenIndex64
					if buffer[position] != rune('\t') {
						goto l66
					}
					position++
					goto l64
				l66:
					position, tokenIndex = position64, tokenIndex64
					if buffer[position] != rune('\n') {
						goto l67
					}
					position++
					goto l64
				l67:
					position, tokenIndex = position64, tokenIndex64
					if buffer[position] != rune('\r') {
						goto l62
					}
					position++
				}
			l64:
				add(rulews, position63)
			}
			return true
		l62:
			position, tokenIndex = position62, tokenIndex62
			return false
		},
		/* 10 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position69 := position
			l70:
				{
					position71, tokenIndex71 := position, tokenIndex
					{
						position72, tokenIndex72 := position, tokenIndex
						if !_rules[rulews]() {
							goto l73
						}
						goto l72
					l73:
						position, tokenIndex = position72, tokenIndex72
						if !_rules[rulelineComment]() {
							goto l74
						}
						goto l72
					l74:
						position, tokenIndex = position72, tokenIndex72
						if !_rules[ruleblockComment]() {
							goto l71
						}
					}
				l72:
					goto l70
				l71:
					position, tokenIndex = position71, tokenIndex71
				}
				add(rule_, position69)
			}
			return true
		},
		/* 11 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position75, tokenIndex75 := position, tokenIndex
			{
				position76 := position
				{
					position77, tokenIndex77 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l78
					}
					position++
					goto l77
				l78:
					position, tokenIndex = position77, tokenIndex77
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l79
					}
					position++
					goto l77
				l79:
					position, tokenIndex = position77, tokenIndex77
					if buffer[position] != rune('_') {
						goto l75
					}
					position++
				}
			l77:
				add(ruleLetter, position76)
			}
			return true
		l75:
			position, tokenIndex = position75, tokenIndex75
			return false
		},
		/* 12 Number <- <[0-9]> */
		func() bool {
			position80, tokenIndex80 := position, tokenIndex
			{
				position81 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l80
				}
				position++
				add(ruleNumber, position81)
			}
			return true
		l80:
			position, tokenIndex = position80, tokenIndex80
			return false
		},
		/* 13 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position82, tokenIndex82 := position, tokenIndex
			{
				position83 := position
				{
					position84, tokenIndex84 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l85
					}
					goto l84
				l85:
					position, tokenIndex = position84, tokenIndex84
					if !_rules[ruleNumber]() {
						goto l82
					}
				}
			l84:
				add(ruleAlphaNum, position83)
			}
			return true
		l82:
			position, tokenIndex = position82, tokenIndex82
			return false
		},
		/* 14 Asterisk <- <('*' Action2)> */
		func() bool {
			position86, tokenIndex86 := position, tokenIndex
			{
				position87 := position
				if buffer[position] != rune('*') {
					goto l86
				}
				position++
				if !_rules[ruleAction2]() {
					goto l86
				}
				add(ruleAsterisk, position87)
			}
			return true
		l86:
			position, tokenIndex = position86, tokenIndex86
			return false
		},
		nil,
		/* 17 Action0 <- <{ p.SelectStatement.Columns = append( p.SelectStatement.Columns  ,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 18 Action1 <- <{ p.SelectStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 19 Action2 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
	}
	p.rules = _rules
}
