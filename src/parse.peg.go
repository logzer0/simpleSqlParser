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
	ruleParaColList
	ruleColList
	ruleColName
	ruleSelectStatement
	ruleInsertStatement
	ruleInsertBody
	ruleValues
	ruleValuesBody
	ruleValList
	ruleInsertValue
	ruleEachValue
	ruleAsterisk
	rulelineComment
	ruleblockComment
	rulews
	rule_
	ruleLetter
	ruleNumber
	ruleAlphaNum
	ruleTable
	ruleAction0
	rulePegText
	ruleAction1
	ruleAction2
	ruleAction3
	ruleAction4
	ruleAction5
	ruleAction6
	ruleAction7
)

var rul3s = [...]string{
	"Unknown",
	"G",
	"Columns",
	"ParaColList",
	"ColList",
	"ColName",
	"SelectStatement",
	"InsertStatement",
	"InsertBody",
	"Values",
	"ValuesBody",
	"ValList",
	"InsertValue",
	"EachValue",
	"Asterisk",
	"lineComment",
	"blockComment",
	"ws",
	"_",
	"Letter",
	"Number",
	"AlphaNum",
	"Table",
	"Action0",
	"PegText",
	"Action1",
	"Action2",
	"Action3",
	"Action4",
	"Action5",
	"Action6",
	"Action7",
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
	rules  [32]func() bool
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
			fmt.Println("I should be first")
		case ruleAction1:
			p.captureColumns(buffer[begin:end])
		case ruleAction2:
			p.captureType(buffer[begin:end])
			fmt.Println("Is this second?")
		case ruleAction3:
			p.captureType(buffer[begin:end])
			fmt.Println("Is this the second one?")
		case ruleAction4:
			fmt.Println("Bro ")
			p.captureType("INSERT")
			fmt.Println("Bro")
		case ruleAction5:
			p.captureValues(buffer[begin:end])
		case ruleAction6:
			p.SelectStatement.AllColumns = true
		case ruleAction7:
			fmt.Println(p.Expression.sType, " This is the type I think")
			p.captureTableName(buffer[begin:end])

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
		/* 0 G <- <(SelectStatement / (InsertStatement !. Action0))> */
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
					if !_rules[ruleAction0]() {
						goto l0
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
		/* 1 Columns <- <(ParaColList / ColList / Asterisk)> */
		func() bool {
			position5, tokenIndex5 := position, tokenIndex
			{
				position6 := position
				{
					position7, tokenIndex7 := position, tokenIndex
					if !_rules[ruleParaColList]() {
						goto l8
					}
					goto l7
				l8:
					position, tokenIndex = position7, tokenIndex7
					if !_rules[ruleColList]() {
						goto l9
					}
					goto l7
				l9:
					position, tokenIndex = position7, tokenIndex7
					if !_rules[ruleAsterisk]() {
						goto l5
					}
				}
			l7:
				add(ruleColumns, position6)
			}
			return true
		l5:
			position, tokenIndex = position5, tokenIndex5
			return false
		},
		/* 2 ParaColList <- <('(' _ ColList _ ')')> */
		func() bool {
			position10, tokenIndex10 := position, tokenIndex
			{
				position11 := position
				if buffer[position] != rune('(') {
					goto l10
				}
				position++
				if !_rules[rule_]() {
					goto l10
				}
				if !_rules[ruleColList]() {
					goto l10
				}
				if !_rules[rule_]() {
					goto l10
				}
				if buffer[position] != rune(')') {
					goto l10
				}
				position++
				add(ruleParaColList, position11)
			}
			return true
		l10:
			position, tokenIndex = position10, tokenIndex10
			return false
		},
		/* 3 ColList <- <((ColName _ ',' _)* ColName)> */
		func() bool {
			position12, tokenIndex12 := position, tokenIndex
			{
				position13 := position
			l14:
				{
					position15, tokenIndex15 := position, tokenIndex
					if !_rules[ruleColName]() {
						goto l15
					}
					if !_rules[rule_]() {
						goto l15
					}
					if buffer[position] != rune(',') {
						goto l15
					}
					position++
					if !_rules[rule_]() {
						goto l15
					}
					goto l14
				l15:
					position, tokenIndex = position15, tokenIndex15
				}
				if !_rules[ruleColName]() {
					goto l12
				}
				add(ruleColList, position13)
			}
			return true
		l12:
			position, tokenIndex = position12, tokenIndex12
			return false
		},
		/* 4 ColName <- <(<AlphaNum+> Action1)> */
		func() bool {
			position16, tokenIndex16 := position, tokenIndex
			{
				position17 := position
				{
					position18 := position
					if !_rules[ruleAlphaNum]() {
						goto l16
					}
				l19:
					{
						position20, tokenIndex20 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l20
						}
						goto l19
					l20:
						position, tokenIndex = position20, tokenIndex20
					}
					add(rulePegText, position18)
				}
				if !_rules[ruleAction1]() {
					goto l16
				}
				add(ruleColName, position17)
			}
			return true
		l16:
			position, tokenIndex = position16, tokenIndex16
			return false
		},
		/* 5 SelectStatement <- <(('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T') _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ Table _ ';' Action2)> */
		func() bool {
			position21, tokenIndex21 := position, tokenIndex
			{
				position22 := position
				{
					position23, tokenIndex23 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l24
					}
					position++
					goto l23
				l24:
					position, tokenIndex = position23, tokenIndex23
					if buffer[position] != rune('S') {
						goto l21
					}
					position++
				}
			l23:
				{
					position25, tokenIndex25 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l26
					}
					position++
					goto l25
				l26:
					position, tokenIndex = position25, tokenIndex25
					if buffer[position] != rune('E') {
						goto l21
					}
					position++
				}
			l25:
				{
					position27, tokenIndex27 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l28
					}
					position++
					goto l27
				l28:
					position, tokenIndex = position27, tokenIndex27
					if buffer[position] != rune('L') {
						goto l21
					}
					position++
				}
			l27:
				{
					position29, tokenIndex29 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l30
					}
					position++
					goto l29
				l30:
					position, tokenIndex = position29, tokenIndex29
					if buffer[position] != rune('E') {
						goto l21
					}
					position++
				}
			l29:
				{
					position31, tokenIndex31 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l32
					}
					position++
					goto l31
				l32:
					position, tokenIndex = position31, tokenIndex31
					if buffer[position] != rune('C') {
						goto l21
					}
					position++
				}
			l31:
				{
					position33, tokenIndex33 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l34
					}
					position++
					goto l33
				l34:
					position, tokenIndex = position33, tokenIndex33
					if buffer[position] != rune('T') {
						goto l21
					}
					position++
				}
			l33:
				if !_rules[rule_]() {
					goto l21
				}
				if !_rules[ruleColumns]() {
					goto l21
				}
				if !_rules[rule_]() {
					goto l21
				}
				{
					position35, tokenIndex35 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l36
					}
					position++
					goto l35
				l36:
					position, tokenIndex = position35, tokenIndex35
					if buffer[position] != rune('F') {
						goto l21
					}
					position++
				}
			l35:
				{
					position37, tokenIndex37 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l38
					}
					position++
					goto l37
				l38:
					position, tokenIndex = position37, tokenIndex37
					if buffer[position] != rune('R') {
						goto l21
					}
					position++
				}
			l37:
				{
					position39, tokenIndex39 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l40
					}
					position++
					goto l39
				l40:
					position, tokenIndex = position39, tokenIndex39
					if buffer[position] != rune('O') {
						goto l21
					}
					position++
				}
			l39:
				{
					position41, tokenIndex41 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l42
					}
					position++
					goto l41
				l42:
					position, tokenIndex = position41, tokenIndex41
					if buffer[position] != rune('M') {
						goto l21
					}
					position++
				}
			l41:
				if !_rules[rule_]() {
					goto l21
				}
				if !_rules[ruleTable]() {
					goto l21
				}
				if !_rules[rule_]() {
					goto l21
				}
				if buffer[position] != rune(';') {
					goto l21
				}
				position++
				if !_rules[ruleAction2]() {
					goto l21
				}
				add(ruleSelectStatement, position22)
			}
			return true
		l21:
			position, tokenIndex = position21, tokenIndex21
			return false
		},
		/* 6 InsertStatement <- <(<(('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T') _ (('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O')) _ InsertBody)> Action3)> */
		func() bool {
			position43, tokenIndex43 := position, tokenIndex
			{
				position44 := position
				{
					position45 := position
					{
						position46, tokenIndex46 := position, tokenIndex
						if buffer[position] != rune('i') {
							goto l47
						}
						position++
						goto l46
					l47:
						position, tokenIndex = position46, tokenIndex46
						if buffer[position] != rune('I') {
							goto l43
						}
						position++
					}
				l46:
					{
						position48, tokenIndex48 := position, tokenIndex
						if buffer[position] != rune('n') {
							goto l49
						}
						position++
						goto l48
					l49:
						position, tokenIndex = position48, tokenIndex48
						if buffer[position] != rune('N') {
							goto l43
						}
						position++
					}
				l48:
					{
						position50, tokenIndex50 := position, tokenIndex
						if buffer[position] != rune('s') {
							goto l51
						}
						position++
						goto l50
					l51:
						position, tokenIndex = position50, tokenIndex50
						if buffer[position] != rune('S') {
							goto l43
						}
						position++
					}
				l50:
					{
						position52, tokenIndex52 := position, tokenIndex
						if buffer[position] != rune('e') {
							goto l53
						}
						position++
						goto l52
					l53:
						position, tokenIndex = position52, tokenIndex52
						if buffer[position] != rune('E') {
							goto l43
						}
						position++
					}
				l52:
					{
						position54, tokenIndex54 := position, tokenIndex
						if buffer[position] != rune('r') {
							goto l55
						}
						position++
						goto l54
					l55:
						position, tokenIndex = position54, tokenIndex54
						if buffer[position] != rune('R') {
							goto l43
						}
						position++
					}
				l54:
					{
						position56, tokenIndex56 := position, tokenIndex
						if buffer[position] != rune('t') {
							goto l57
						}
						position++
						goto l56
					l57:
						position, tokenIndex = position56, tokenIndex56
						if buffer[position] != rune('T') {
							goto l43
						}
						position++
					}
				l56:
					if !_rules[rule_]() {
						goto l43
					}
					{
						position58, tokenIndex58 := position, tokenIndex
						if buffer[position] != rune('i') {
							goto l59
						}
						position++
						goto l58
					l59:
						position, tokenIndex = position58, tokenIndex58
						if buffer[position] != rune('I') {
							goto l43
						}
						position++
					}
				l58:
					{
						position60, tokenIndex60 := position, tokenIndex
						if buffer[position] != rune('n') {
							goto l61
						}
						position++
						goto l60
					l61:
						position, tokenIndex = position60, tokenIndex60
						if buffer[position] != rune('N') {
							goto l43
						}
						position++
					}
				l60:
					{
						position62, tokenIndex62 := position, tokenIndex
						if buffer[position] != rune('t') {
							goto l63
						}
						position++
						goto l62
					l63:
						position, tokenIndex = position62, tokenIndex62
						if buffer[position] != rune('T') {
							goto l43
						}
						position++
					}
				l62:
					{
						position64, tokenIndex64 := position, tokenIndex
						if buffer[position] != rune('o') {
							goto l65
						}
						position++
						goto l64
					l65:
						position, tokenIndex = position64, tokenIndex64
						if buffer[position] != rune('O') {
							goto l43
						}
						position++
					}
				l64:
					if !_rules[rule_]() {
						goto l43
					}
					if !_rules[ruleInsertBody]() {
						goto l43
					}
					add(rulePegText, position45)
				}
				if !_rules[ruleAction3]() {
					goto l43
				}
				add(ruleInsertStatement, position44)
			}
			return true
		l43:
			position, tokenIndex = position43, tokenIndex43
			return false
		},
		/* 7 InsertBody <- <(<(Table _ Values _ ';')> Action4)> */
		func() bool {
			position66, tokenIndex66 := position, tokenIndex
			{
				position67 := position
				{
					position68 := position
					if !_rules[ruleTable]() {
						goto l66
					}
					if !_rules[rule_]() {
						goto l66
					}
					if !_rules[ruleValues]() {
						goto l66
					}
					if !_rules[rule_]() {
						goto l66
					}
					if buffer[position] != rune(';') {
						goto l66
					}
					position++
					add(rulePegText, position68)
				}
				if !_rules[ruleAction4]() {
					goto l66
				}
				add(ruleInsertBody, position67)
			}
			return true
		l66:
			position, tokenIndex = position66, tokenIndex66
			return false
		},
		/* 8 Values <- <(ParaColList? _ (('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S')) _ ValuesBody)> */
		func() bool {
			position69, tokenIndex69 := position, tokenIndex
			{
				position70 := position
				{
					position71, tokenIndex71 := position, tokenIndex
					if !_rules[ruleParaColList]() {
						goto l71
					}
					goto l72
				l71:
					position, tokenIndex = position71, tokenIndex71
				}
			l72:
				if !_rules[rule_]() {
					goto l69
				}
				{
					position73, tokenIndex73 := position, tokenIndex
					if buffer[position] != rune('v') {
						goto l74
					}
					position++
					goto l73
				l74:
					position, tokenIndex = position73, tokenIndex73
					if buffer[position] != rune('V') {
						goto l69
					}
					position++
				}
			l73:
				{
					position75, tokenIndex75 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l76
					}
					position++
					goto l75
				l76:
					position, tokenIndex = position75, tokenIndex75
					if buffer[position] != rune('A') {
						goto l69
					}
					position++
				}
			l75:
				{
					position77, tokenIndex77 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l78
					}
					position++
					goto l77
				l78:
					position, tokenIndex = position77, tokenIndex77
					if buffer[position] != rune('L') {
						goto l69
					}
					position++
				}
			l77:
				{
					position79, tokenIndex79 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l80
					}
					position++
					goto l79
				l80:
					position, tokenIndex = position79, tokenIndex79
					if buffer[position] != rune('U') {
						goto l69
					}
					position++
				}
			l79:
				{
					position81, tokenIndex81 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l82
					}
					position++
					goto l81
				l82:
					position, tokenIndex = position81, tokenIndex81
					if buffer[position] != rune('E') {
						goto l69
					}
					position++
				}
			l81:
				{
					position83, tokenIndex83 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l84
					}
					position++
					goto l83
				l84:
					position, tokenIndex = position83, tokenIndex83
					if buffer[position] != rune('S') {
						goto l69
					}
					position++
				}
			l83:
				if !_rules[rule_]() {
					goto l69
				}
				if !_rules[ruleValuesBody]() {
					goto l69
				}
				add(ruleValues, position70)
			}
			return true
		l69:
			position, tokenIndex = position69, tokenIndex69
			return false
		},
		/* 9 ValuesBody <- <('(' _ ValList _ ')')> */
		func() bool {
			position85, tokenIndex85 := position, tokenIndex
			{
				position86 := position
				if buffer[position] != rune('(') {
					goto l85
				}
				position++
				if !_rules[rule_]() {
					goto l85
				}
				if !_rules[ruleValList]() {
					goto l85
				}
				if !_rules[rule_]() {
					goto l85
				}
				if buffer[position] != rune(')') {
					goto l85
				}
				position++
				add(ruleValuesBody, position86)
			}
			return true
		l85:
			position, tokenIndex = position85, tokenIndex85
			return false
		},
		/* 10 ValList <- <((_ InsertValue _ ',' _)* _ InsertValue)> */
		func() bool {
			position87, tokenIndex87 := position, tokenIndex
			{
				position88 := position
			l89:
				{
					position90, tokenIndex90 := position, tokenIndex
					if !_rules[rule_]() {
						goto l90
					}
					if !_rules[ruleInsertValue]() {
						goto l90
					}
					if !_rules[rule_]() {
						goto l90
					}
					if buffer[position] != rune(',') {
						goto l90
					}
					position++
					if !_rules[rule_]() {
						goto l90
					}
					goto l89
				l90:
					position, tokenIndex = position90, tokenIndex90
				}
				if !_rules[rule_]() {
					goto l87
				}
				if !_rules[ruleInsertValue]() {
					goto l87
				}
				add(ruleValList, position88)
			}
			return true
		l87:
			position, tokenIndex = position87, tokenIndex87
			return false
		},
		/* 11 InsertValue <- <('"'? EachValue '"'?)> */
		func() bool {
			position91, tokenIndex91 := position, tokenIndex
			{
				position92 := position
				{
					position93, tokenIndex93 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l93
					}
					position++
					goto l94
				l93:
					position, tokenIndex = position93, tokenIndex93
				}
			l94:
				if !_rules[ruleEachValue]() {
					goto l91
				}
				{
					position95, tokenIndex95 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l95
					}
					position++
					goto l96
				l95:
					position, tokenIndex = position95, tokenIndex95
				}
			l96:
				add(ruleInsertValue, position92)
			}
			return true
		l91:
			position, tokenIndex = position91, tokenIndex91
			return false
		},
		/* 12 EachValue <- <(<AlphaNum+> Action5)> */
		func() bool {
			position97, tokenIndex97 := position, tokenIndex
			{
				position98 := position
				{
					position99 := position
					if !_rules[ruleAlphaNum]() {
						goto l97
					}
				l100:
					{
						position101, tokenIndex101 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l101
						}
						goto l100
					l101:
						position, tokenIndex = position101, tokenIndex101
					}
					add(rulePegText, position99)
				}
				if !_rules[ruleAction5]() {
					goto l97
				}
				add(ruleEachValue, position98)
			}
			return true
		l97:
			position, tokenIndex = position97, tokenIndex97
			return false
		},
		/* 13 Asterisk <- <('*' Action6)> */
		func() bool {
			position102, tokenIndex102 := position, tokenIndex
			{
				position103 := position
				if buffer[position] != rune('*') {
					goto l102
				}
				position++
				if !_rules[ruleAction6]() {
					goto l102
				}
				add(ruleAsterisk, position103)
			}
			return true
		l102:
			position, tokenIndex = position102, tokenIndex102
			return false
		},
		/* 14 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position104, tokenIndex104 := position, tokenIndex
			{
				position105 := position
				if buffer[position] != rune('/') {
					goto l104
				}
				position++
				if buffer[position] != rune('/') {
					goto l104
				}
				position++
			l106:
				{
					position107, tokenIndex107 := position, tokenIndex
					{
						position108, tokenIndex108 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l108
						}
						position++
						goto l107
					l108:
						position, tokenIndex = position108, tokenIndex108
					}
					if !matchDot() {
						goto l107
					}
					goto l106
				l107:
					position, tokenIndex = position107, tokenIndex107
				}
				add(rulelineComment, position105)
			}
			return true
		l104:
			position, tokenIndex = position104, tokenIndex104
			return false
		},
		/* 15 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position109, tokenIndex109 := position, tokenIndex
			{
				position110 := position
				if buffer[position] != rune('/') {
					goto l109
				}
				position++
				if buffer[position] != rune('*') {
					goto l109
				}
				position++
			l111:
				{
					position112, tokenIndex112 := position, tokenIndex
					{
						position113, tokenIndex113 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l113
						}
						position++
						if buffer[position] != rune('/') {
							goto l113
						}
						position++
						goto l112
					l113:
						position, tokenIndex = position113, tokenIndex113
					}
					{
						position114, tokenIndex114 := position, tokenIndex
						if !matchDot() {
							goto l115
						}
						goto l114
					l115:
						position, tokenIndex = position114, tokenIndex114
						if buffer[position] != rune('\n') {
							goto l112
						}
						position++
					}
				l114:
					goto l111
				l112:
					position, tokenIndex = position112, tokenIndex112
				}
				if buffer[position] != rune('*') {
					goto l109
				}
				position++
				if buffer[position] != rune('/') {
					goto l109
				}
				position++
				add(ruleblockComment, position110)
			}
			return true
		l109:
			position, tokenIndex = position109, tokenIndex109
			return false
		},
		/* 16 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position116, tokenIndex116 := position, tokenIndex
			{
				position117 := position
				{
					position118, tokenIndex118 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l119
					}
					position++
					goto l118
				l119:
					position, tokenIndex = position118, tokenIndex118
					if buffer[position] != rune('\t') {
						goto l120
					}
					position++
					goto l118
				l120:
					position, tokenIndex = position118, tokenIndex118
					if buffer[position] != rune('\n') {
						goto l121
					}
					position++
					goto l118
				l121:
					position, tokenIndex = position118, tokenIndex118
					if buffer[position] != rune('\r') {
						goto l116
					}
					position++
				}
			l118:
				add(rulews, position117)
			}
			return true
		l116:
			position, tokenIndex = position116, tokenIndex116
			return false
		},
		/* 17 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position123 := position
			l124:
				{
					position125, tokenIndex125 := position, tokenIndex
					{
						position126, tokenIndex126 := position, tokenIndex
						if !_rules[rulews]() {
							goto l127
						}
						goto l126
					l127:
						position, tokenIndex = position126, tokenIndex126
						if !_rules[rulelineComment]() {
							goto l128
						}
						goto l126
					l128:
						position, tokenIndex = position126, tokenIndex126
						if !_rules[ruleblockComment]() {
							goto l125
						}
					}
				l126:
					goto l124
				l125:
					position, tokenIndex = position125, tokenIndex125
				}
				add(rule_, position123)
			}
			return true
		},
		/* 18 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position129, tokenIndex129 := position, tokenIndex
			{
				position130 := position
				{
					position131, tokenIndex131 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l132
					}
					position++
					goto l131
				l132:
					position, tokenIndex = position131, tokenIndex131
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l133
					}
					position++
					goto l131
				l133:
					position, tokenIndex = position131, tokenIndex131
					if buffer[position] != rune('_') {
						goto l129
					}
					position++
				}
			l131:
				add(ruleLetter, position130)
			}
			return true
		l129:
			position, tokenIndex = position129, tokenIndex129
			return false
		},
		/* 19 Number <- <([0-9] '.'? [0-9]*)> */
		func() bool {
			position134, tokenIndex134 := position, tokenIndex
			{
				position135 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l134
				}
				position++
				{
					position136, tokenIndex136 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l136
					}
					position++
					goto l137
				l136:
					position, tokenIndex = position136, tokenIndex136
				}
			l137:
			l138:
				{
					position139, tokenIndex139 := position, tokenIndex
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l139
					}
					position++
					goto l138
				l139:
					position, tokenIndex = position139, tokenIndex139
				}
				add(ruleNumber, position135)
			}
			return true
		l134:
			position, tokenIndex = position134, tokenIndex134
			return false
		},
		/* 20 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position140, tokenIndex140 := position, tokenIndex
			{
				position141 := position
				{
					position142, tokenIndex142 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l143
					}
					goto l142
				l143:
					position, tokenIndex = position142, tokenIndex142
					if !_rules[ruleNumber]() {
						goto l140
					}
				}
			l142:
				add(ruleAlphaNum, position141)
			}
			return true
		l140:
			position, tokenIndex = position140, tokenIndex140
			return false
		},
		/* 21 Table <- <(<AlphaNum+> Action7)> */
		func() bool {
			position144, tokenIndex144 := position, tokenIndex
			{
				position145 := position
				{
					position146 := position
					if !_rules[ruleAlphaNum]() {
						goto l144
					}
				l147:
					{
						position148, tokenIndex148 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l148
						}
						goto l147
					l148:
						position, tokenIndex = position148, tokenIndex148
					}
					add(rulePegText, position146)
				}
				if !_rules[ruleAction7]() {
					goto l144
				}
				add(ruleTable, position145)
			}
			return true
		l144:
			position, tokenIndex = position144, tokenIndex144
			return false
		},
		/* 23 Action0 <- <{ fmt.Println("I should be first")}> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		nil,
		/* 25 Action1 <- <{ p.captureColumns(buffer[begin:end])  }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 26 Action2 <- <{ p.captureType(buffer[begin:end]); fmt.Println("Is this second?") }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 27 Action3 <- <{ p.captureType(buffer[begin:end]) ; fmt.Println("Is this the second one?");  }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 28 Action4 <- <{ fmt.Println("Bro "); p.captureType("INSERT"); fmt.Println("Bro");   }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 29 Action5 <- <{ p.captureValues(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 30 Action6 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 31 Action7 <- <{ fmt.Println(p.Expression.sType," This is the type I think");  p.captureTableName(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
	}
	p.rules = _rules
}
