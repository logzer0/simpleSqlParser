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
	ruleColumns
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
	rulePegText
	ruleAction0
	ruleAction1
)

var rul3s = [...]string{
	"Unknown",
	"G",
	"Columns",
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
	"PegText",
	"Action0",
	"Action1",
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
	Expression

	Buffer string
	buffer []rune
	rules  [16]func() bool
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
			p.captureColName(buffer[begin:end])
		case ruleAction1:
			p.TableName = buffer[begin:end]

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
		/* 0 G <- <(('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T') _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ Table _ ';' !.)> */
		func() bool {
			position0, tokenIndex0 := position, tokenIndex
			{
				position1 := position
				{
					position2, tokenIndex2 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l3
					}
					position++
					goto l2
				l3:
					position, tokenIndex = position2, tokenIndex2
					if buffer[position] != rune('S') {
						goto l0
					}
					position++
				}
			l2:
				{
					position4, tokenIndex4 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l5
					}
					position++
					goto l4
				l5:
					position, tokenIndex = position4, tokenIndex4
					if buffer[position] != rune('E') {
						goto l0
					}
					position++
				}
			l4:
				{
					position6, tokenIndex6 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l7
					}
					position++
					goto l6
				l7:
					position, tokenIndex = position6, tokenIndex6
					if buffer[position] != rune('L') {
						goto l0
					}
					position++
				}
			l6:
				{
					position8, tokenIndex8 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l9
					}
					position++
					goto l8
				l9:
					position, tokenIndex = position8, tokenIndex8
					if buffer[position] != rune('E') {
						goto l0
					}
					position++
				}
			l8:
				{
					position10, tokenIndex10 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l11
					}
					position++
					goto l10
				l11:
					position, tokenIndex = position10, tokenIndex10
					if buffer[position] != rune('C') {
						goto l0
					}
					position++
				}
			l10:
				{
					position12, tokenIndex12 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l13
					}
					position++
					goto l12
				l13:
					position, tokenIndex = position12, tokenIndex12
					if buffer[position] != rune('T') {
						goto l0
					}
					position++
				}
			l12:
				if !_rules[rule_]() {
					goto l0
				}
				if !_rules[ruleColumns]() {
					goto l0
				}
				if !_rules[rule_]() {
					goto l0
				}
				{
					position14, tokenIndex14 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l15
					}
					position++
					goto l14
				l15:
					position, tokenIndex = position14, tokenIndex14
					if buffer[position] != rune('F') {
						goto l0
					}
					position++
				}
			l14:
				{
					position16, tokenIndex16 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l17
					}
					position++
					goto l16
				l17:
					position, tokenIndex = position16, tokenIndex16
					if buffer[position] != rune('R') {
						goto l0
					}
					position++
				}
			l16:
				{
					position18, tokenIndex18 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l19
					}
					position++
					goto l18
				l19:
					position, tokenIndex = position18, tokenIndex18
					if buffer[position] != rune('O') {
						goto l0
					}
					position++
				}
			l18:
				{
					position20, tokenIndex20 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l21
					}
					position++
					goto l20
				l21:
					position, tokenIndex = position20, tokenIndex20
					if buffer[position] != rune('M') {
						goto l0
					}
					position++
				}
			l20:
				if !_rules[rule_]() {
					goto l0
				}
				if !_rules[ruleTable]() {
					goto l0
				}
				if !_rules[rule_]() {
					goto l0
				}
				if buffer[position] != rune(';') {
					goto l0
				}
				position++
				{
					position22, tokenIndex22 := position, tokenIndex
					if !matchDot() {
						goto l22
					}
					goto l0
				l22:
					position, tokenIndex = position22, tokenIndex22
				}
				add(ruleG, position1)
			}
			return true
		l0:
			position, tokenIndex = position0, tokenIndex0
			return false
		},
		/* 1 Columns <- <('(' ColList ')')> */
		func() bool {
			position23, tokenIndex23 := position, tokenIndex
			{
				position24 := position
				if buffer[position] != rune('(') {
					goto l23
				}
				position++
				if !_rules[ruleColList]() {
					goto l23
				}
				if buffer[position] != rune(')') {
					goto l23
				}
				position++
				add(ruleColumns, position24)
			}
			return true
		l23:
			position, tokenIndex = position23, tokenIndex23
			return false
		},
		/* 2 ColList <- <((ColName ',')* ColName)> */
		func() bool {
			position25, tokenIndex25 := position, tokenIndex
			{
				position26 := position
			l27:
				{
					position28, tokenIndex28 := position, tokenIndex
					if !_rules[ruleColName]() {
						goto l28
					}
					if buffer[position] != rune(',') {
						goto l28
					}
					position++
					goto l27
				l28:
					position, tokenIndex = position28, tokenIndex28
				}
				if !_rules[ruleColName]() {
					goto l25
				}
				add(ruleColList, position26)
			}
			return true
		l25:
			position, tokenIndex = position25, tokenIndex25
			return false
		},
		/* 3 ColName <- <(<AlphaNum+> Action0)> */
		func() bool {
			position29, tokenIndex29 := position, tokenIndex
			{
				position30 := position
				{
					position31 := position
					if !_rules[ruleAlphaNum]() {
						goto l29
					}
				l32:
					{
						position33, tokenIndex33 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l33
						}
						goto l32
					l33:
						position, tokenIndex = position33, tokenIndex33
					}
					add(rulePegText, position31)
				}
				if !_rules[ruleAction0]() {
					goto l29
				}
				add(ruleColName, position30)
			}
			return true
		l29:
			position, tokenIndex = position29, tokenIndex29
			return false
		},
		/* 4 Table <- <(<(('t' / 'T') ('b' / 'B') ('l' / 'L'))> Action1)> */
		func() bool {
			position34, tokenIndex34 := position, tokenIndex
			{
				position35 := position
				{
					position36 := position
					{
						position37, tokenIndex37 := position, tokenIndex
						if buffer[position] != rune('t') {
							goto l38
						}
						position++
						goto l37
					l38:
						position, tokenIndex = position37, tokenIndex37
						if buffer[position] != rune('T') {
							goto l34
						}
						position++
					}
				l37:
					{
						position39, tokenIndex39 := position, tokenIndex
						if buffer[position] != rune('b') {
							goto l40
						}
						position++
						goto l39
					l40:
						position, tokenIndex = position39, tokenIndex39
						if buffer[position] != rune('B') {
							goto l34
						}
						position++
					}
				l39:
					{
						position41, tokenIndex41 := position, tokenIndex
						if buffer[position] != rune('l') {
							goto l42
						}
						position++
						goto l41
					l42:
						position, tokenIndex = position41, tokenIndex41
						if buffer[position] != rune('L') {
							goto l34
						}
						position++
					}
				l41:
					add(rulePegText, position36)
				}
				if !_rules[ruleAction1]() {
					goto l34
				}
				add(ruleTable, position35)
			}
			return true
		l34:
			position, tokenIndex = position34, tokenIndex34
			return false
		},
		/* 5 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position43, tokenIndex43 := position, tokenIndex
			{
				position44 := position
				if buffer[position] != rune('/') {
					goto l43
				}
				position++
				if buffer[position] != rune('/') {
					goto l43
				}
				position++
			l45:
				{
					position46, tokenIndex46 := position, tokenIndex
					{
						position47, tokenIndex47 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l47
						}
						position++
						goto l46
					l47:
						position, tokenIndex = position47, tokenIndex47
					}
					if !matchDot() {
						goto l46
					}
					goto l45
				l46:
					position, tokenIndex = position46, tokenIndex46
				}
				add(rulelineComment, position44)
			}
			return true
		l43:
			position, tokenIndex = position43, tokenIndex43
			return false
		},
		/* 6 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position48, tokenIndex48 := position, tokenIndex
			{
				position49 := position
				if buffer[position] != rune('/') {
					goto l48
				}
				position++
				if buffer[position] != rune('*') {
					goto l48
				}
				position++
			l50:
				{
					position51, tokenIndex51 := position, tokenIndex
					{
						position52, tokenIndex52 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l52
						}
						position++
						if buffer[position] != rune('/') {
							goto l52
						}
						position++
						goto l51
					l52:
						position, tokenIndex = position52, tokenIndex52
					}
					{
						position53, tokenIndex53 := position, tokenIndex
						if !matchDot() {
							goto l54
						}
						goto l53
					l54:
						position, tokenIndex = position53, tokenIndex53
						if buffer[position] != rune('\n') {
							goto l51
						}
						position++
					}
				l53:
					goto l50
				l51:
					position, tokenIndex = position51, tokenIndex51
				}
				if buffer[position] != rune('*') {
					goto l48
				}
				position++
				if buffer[position] != rune('/') {
					goto l48
				}
				position++
				add(ruleblockComment, position49)
			}
			return true
		l48:
			position, tokenIndex = position48, tokenIndex48
			return false
		},
		/* 7 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position55, tokenIndex55 := position, tokenIndex
			{
				position56 := position
				{
					position57, tokenIndex57 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l58
					}
					position++
					goto l57
				l58:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('\t') {
						goto l59
					}
					position++
					goto l57
				l59:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('\n') {
						goto l60
					}
					position++
					goto l57
				l60:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('\r') {
						goto l55
					}
					position++
				}
			l57:
				add(rulews, position56)
			}
			return true
		l55:
			position, tokenIndex = position55, tokenIndex55
			return false
		},
		/* 8 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position62 := position
			l63:
				{
					position64, tokenIndex64 := position, tokenIndex
					{
						position65, tokenIndex65 := position, tokenIndex
						if !_rules[rulews]() {
							goto l66
						}
						goto l65
					l66:
						position, tokenIndex = position65, tokenIndex65
						if !_rules[rulelineComment]() {
							goto l67
						}
						goto l65
					l67:
						position, tokenIndex = position65, tokenIndex65
						if !_rules[ruleblockComment]() {
							goto l64
						}
					}
				l65:
					goto l63
				l64:
					position, tokenIndex = position64, tokenIndex64
				}
				add(rule_, position62)
			}
			return true
		},
		/* 9 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position68, tokenIndex68 := position, tokenIndex
			{
				position69 := position
				{
					position70, tokenIndex70 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l71
					}
					position++
					goto l70
				l71:
					position, tokenIndex = position70, tokenIndex70
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l72
					}
					position++
					goto l70
				l72:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('_') {
						goto l68
					}
					position++
				}
			l70:
				add(ruleLetter, position69)
			}
			return true
		l68:
			position, tokenIndex = position68, tokenIndex68
			return false
		},
		/* 10 Number <- <[0-9]> */
		func() bool {
			position73, tokenIndex73 := position, tokenIndex
			{
				position74 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l73
				}
				position++
				add(ruleNumber, position74)
			}
			return true
		l73:
			position, tokenIndex = position73, tokenIndex73
			return false
		},
		/* 11 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position75, tokenIndex75 := position, tokenIndex
			{
				position76 := position
				{
					position77, tokenIndex77 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l78
					}
					goto l77
				l78:
					position, tokenIndex = position77, tokenIndex77
					if !_rules[ruleNumber]() {
						goto l75
					}
				}
			l77:
				add(ruleAlphaNum, position76)
			}
			return true
		l75:
			position, tokenIndex = position75, tokenIndex75
			return false
		},
		nil,
		/* 14 Action0 <- <{ p.captureColName(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 15 Action1 <- <{ p.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
	}
	p.rules = _rules
}
