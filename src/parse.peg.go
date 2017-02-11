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
	ruleSelectParaColList
	ruleSelectColList
	ruleSelectColName
	ruleInsertParaColList
	ruleInsertColList
	ruleInsertColName
	ruleCreateParaColList
	ruleCreateColList
	ruleCreateColName
	ruleSelectStatement
	ruleInsertStatement
	ruleCreateStatement
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
	ruleSelectTable
	ruleInsertTable
	ruleCreateTable
	rulePegText
	ruleAction0
	ruleAction1
	ruleAction2
	ruleAction3
	ruleAction4
	ruleAction5
	ruleAction6
	ruleAction7
	ruleAction8
	ruleAction9
	ruleAction10
)

var rul3s = [...]string{
	"Unknown",
	"G",
	"Columns",
	"SelectParaColList",
	"SelectColList",
	"SelectColName",
	"InsertParaColList",
	"InsertColList",
	"InsertColName",
	"CreateParaColList",
	"CreateColList",
	"CreateColName",
	"SelectStatement",
	"InsertStatement",
	"CreateStatement",
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
	"SelectTable",
	"InsertTable",
	"CreateTable",
	"PegText",
	"Action0",
	"Action1",
	"Action2",
	"Action3",
	"Action4",
	"Action5",
	"Action6",
	"Action7",
	"Action8",
	"Action9",
	"Action10",
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
	sType
	SelectStatement
	InsertStatement
	CreateStatement

	Buffer string
	buffer []rune
	rules  [44]func() bool
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
			p.InsertStatement.Columns = append(p.InsertStatement.Columns, buffer[begin:end])
		case ruleAction2:
			p.CreateStatement.Columns = append(p.CreateStatement.Columns, buffer[begin:end])
		case ruleAction3:
			p.sType = Select
		case ruleAction4:
			p.validateInsert()
		case ruleAction5:
			p.setPartitionKey(buffer[begin:end])
		case ruleAction6:
			p.captureValues(buffer[begin:end])
		case ruleAction7:
			p.SelectStatement.AllColumns = true
		case ruleAction8:
			p.SelectStatement.TableName = buffer[begin:end]
		case ruleAction9:
			p.InsertStatement.TableName = buffer[begin:end]
		case ruleAction10:
			p.CreateStatement.TableName = buffer[begin:end]

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
		/* 0 G <- <(SelectStatement / InsertStatement / (CreateStatement !.))> */
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
						goto l4
					}
					goto l2
				l4:
					position, tokenIndex = position2, tokenIndex2
					if !_rules[ruleCreateStatement]() {
						goto l0
					}
					{
						position5, tokenIndex5 := position, tokenIndex
						if !matchDot() {
							goto l5
						}
						goto l0
					l5:
						position, tokenIndex = position5, tokenIndex5
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
		/* 1 Columns <- <(SelectParaColList / SelectColList / Asterisk)> */
		func() bool {
			position6, tokenIndex6 := position, tokenIndex
			{
				position7 := position
				{
					position8, tokenIndex8 := position, tokenIndex
					if !_rules[ruleSelectParaColList]() {
						goto l9
					}
					goto l8
				l9:
					position, tokenIndex = position8, tokenIndex8
					if !_rules[ruleSelectColList]() {
						goto l10
					}
					goto l8
				l10:
					position, tokenIndex = position8, tokenIndex8
					if !_rules[ruleAsterisk]() {
						goto l6
					}
				}
			l8:
				add(ruleColumns, position7)
			}
			return true
		l6:
			position, tokenIndex = position6, tokenIndex6
			return false
		},
		/* 2 SelectParaColList <- <('(' _ SelectColList _ ')')> */
		func() bool {
			position11, tokenIndex11 := position, tokenIndex
			{
				position12 := position
				if buffer[position] != rune('(') {
					goto l11
				}
				position++
				if !_rules[rule_]() {
					goto l11
				}
				if !_rules[ruleSelectColList]() {
					goto l11
				}
				if !_rules[rule_]() {
					goto l11
				}
				if buffer[position] != rune(')') {
					goto l11
				}
				position++
				add(ruleSelectParaColList, position12)
			}
			return true
		l11:
			position, tokenIndex = position11, tokenIndex11
			return false
		},
		/* 3 SelectColList <- <((SelectColName _ ',' _)* SelectColName)> */
		func() bool {
			position13, tokenIndex13 := position, tokenIndex
			{
				position14 := position
			l15:
				{
					position16, tokenIndex16 := position, tokenIndex
					if !_rules[ruleSelectColName]() {
						goto l16
					}
					if !_rules[rule_]() {
						goto l16
					}
					if buffer[position] != rune(',') {
						goto l16
					}
					position++
					if !_rules[rule_]() {
						goto l16
					}
					goto l15
				l16:
					position, tokenIndex = position16, tokenIndex16
				}
				if !_rules[ruleSelectColName]() {
					goto l13
				}
				add(ruleSelectColList, position14)
			}
			return true
		l13:
			position, tokenIndex = position13, tokenIndex13
			return false
		},
		/* 4 SelectColName <- <(<AlphaNum+> Action0)> */
		func() bool {
			position17, tokenIndex17 := position, tokenIndex
			{
				position18 := position
				{
					position19 := position
					if !_rules[ruleAlphaNum]() {
						goto l17
					}
				l20:
					{
						position21, tokenIndex21 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l21
						}
						goto l20
					l21:
						position, tokenIndex = position21, tokenIndex21
					}
					add(rulePegText, position19)
				}
				if !_rules[ruleAction0]() {
					goto l17
				}
				add(ruleSelectColName, position18)
			}
			return true
		l17:
			position, tokenIndex = position17, tokenIndex17
			return false
		},
		/* 5 InsertParaColList <- <('(' _ InsertColList _ ')')> */
		func() bool {
			position22, tokenIndex22 := position, tokenIndex
			{
				position23 := position
				if buffer[position] != rune('(') {
					goto l22
				}
				position++
				if !_rules[rule_]() {
					goto l22
				}
				if !_rules[ruleInsertColList]() {
					goto l22
				}
				if !_rules[rule_]() {
					goto l22
				}
				if buffer[position] != rune(')') {
					goto l22
				}
				position++
				add(ruleInsertParaColList, position23)
			}
			return true
		l22:
			position, tokenIndex = position22, tokenIndex22
			return false
		},
		/* 6 InsertColList <- <((_ InsertColName _ ',' _)* InsertColName)> */
		func() bool {
			position24, tokenIndex24 := position, tokenIndex
			{
				position25 := position
			l26:
				{
					position27, tokenIndex27 := position, tokenIndex
					if !_rules[rule_]() {
						goto l27
					}
					if !_rules[ruleInsertColName]() {
						goto l27
					}
					if !_rules[rule_]() {
						goto l27
					}
					if buffer[position] != rune(',') {
						goto l27
					}
					position++
					if !_rules[rule_]() {
						goto l27
					}
					goto l26
				l27:
					position, tokenIndex = position27, tokenIndex27
				}
				if !_rules[ruleInsertColName]() {
					goto l24
				}
				add(ruleInsertColList, position25)
			}
			return true
		l24:
			position, tokenIndex = position24, tokenIndex24
			return false
		},
		/* 7 InsertColName <- <(<AlphaNum+> Action1)> */
		func() bool {
			position28, tokenIndex28 := position, tokenIndex
			{
				position29 := position
				{
					position30 := position
					if !_rules[ruleAlphaNum]() {
						goto l28
					}
				l31:
					{
						position32, tokenIndex32 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l32
						}
						goto l31
					l32:
						position, tokenIndex = position32, tokenIndex32
					}
					add(rulePegText, position30)
				}
				if !_rules[ruleAction1]() {
					goto l28
				}
				add(ruleInsertColName, position29)
			}
			return true
		l28:
			position, tokenIndex = position28, tokenIndex28
			return false
		},
		/* 8 CreateParaColList <- <('(' _ CreateColList _ ')')> */
		func() bool {
			position33, tokenIndex33 := position, tokenIndex
			{
				position34 := position
				if buffer[position] != rune('(') {
					goto l33
				}
				position++
				if !_rules[rule_]() {
					goto l33
				}
				if !_rules[ruleCreateColList]() {
					goto l33
				}
				if !_rules[rule_]() {
					goto l33
				}
				if buffer[position] != rune(')') {
					goto l33
				}
				position++
				add(ruleCreateParaColList, position34)
			}
			return true
		l33:
			position, tokenIndex = position33, tokenIndex33
			return false
		},
		/* 9 CreateColList <- <((_ CreateColName _ ',' _)* CreateColName)> */
		func() bool {
			position35, tokenIndex35 := position, tokenIndex
			{
				position36 := position
			l37:
				{
					position38, tokenIndex38 := position, tokenIndex
					if !_rules[rule_]() {
						goto l38
					}
					if !_rules[ruleCreateColName]() {
						goto l38
					}
					if !_rules[rule_]() {
						goto l38
					}
					if buffer[position] != rune(',') {
						goto l38
					}
					position++
					if !_rules[rule_]() {
						goto l38
					}
					goto l37
				l38:
					position, tokenIndex = position38, tokenIndex38
				}
				if !_rules[ruleCreateColName]() {
					goto l35
				}
				add(ruleCreateColList, position36)
			}
			return true
		l35:
			position, tokenIndex = position35, tokenIndex35
			return false
		},
		/* 10 CreateColName <- <(<AlphaNum+> Action2)> */
		func() bool {
			position39, tokenIndex39 := position, tokenIndex
			{
				position40 := position
				{
					position41 := position
					if !_rules[ruleAlphaNum]() {
						goto l39
					}
				l42:
					{
						position43, tokenIndex43 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l43
						}
						goto l42
					l43:
						position, tokenIndex = position43, tokenIndex43
					}
					add(rulePegText, position41)
				}
				if !_rules[ruleAction2]() {
					goto l39
				}
				add(ruleCreateColName, position40)
			}
			return true
		l39:
			position, tokenIndex = position39, tokenIndex39
			return false
		},
		/* 11 SelectStatement <- <(('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T') _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ SelectTable _ ';' Action3)> */
		func() bool {
			position44, tokenIndex44 := position, tokenIndex
			{
				position45 := position
				{
					position46, tokenIndex46 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l47
					}
					position++
					goto l46
				l47:
					position, tokenIndex = position46, tokenIndex46
					if buffer[position] != rune('S') {
						goto l44
					}
					position++
				}
			l46:
				{
					position48, tokenIndex48 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l49
					}
					position++
					goto l48
				l49:
					position, tokenIndex = position48, tokenIndex48
					if buffer[position] != rune('E') {
						goto l44
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
						goto l44
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
						goto l44
					}
					position++
				}
			l52:
				{
					position54, tokenIndex54 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l55
					}
					position++
					goto l54
				l55:
					position, tokenIndex = position54, tokenIndex54
					if buffer[position] != rune('C') {
						goto l44
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
						goto l44
					}
					position++
				}
			l56:
				if !_rules[rule_]() {
					goto l44
				}
				if !_rules[ruleColumns]() {
					goto l44
				}
				if !_rules[rule_]() {
					goto l44
				}
				{
					position58, tokenIndex58 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l59
					}
					position++
					goto l58
				l59:
					position, tokenIndex = position58, tokenIndex58
					if buffer[position] != rune('F') {
						goto l44
					}
					position++
				}
			l58:
				{
					position60, tokenIndex60 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l61
					}
					position++
					goto l60
				l61:
					position, tokenIndex = position60, tokenIndex60
					if buffer[position] != rune('R') {
						goto l44
					}
					position++
				}
			l60:
				{
					position62, tokenIndex62 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l63
					}
					position++
					goto l62
				l63:
					position, tokenIndex = position62, tokenIndex62
					if buffer[position] != rune('O') {
						goto l44
					}
					position++
				}
			l62:
				{
					position64, tokenIndex64 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l65
					}
					position++
					goto l64
				l65:
					position, tokenIndex = position64, tokenIndex64
					if buffer[position] != rune('M') {
						goto l44
					}
					position++
				}
			l64:
				if !_rules[rule_]() {
					goto l44
				}
				if !_rules[ruleSelectTable]() {
					goto l44
				}
				if !_rules[rule_]() {
					goto l44
				}
				if buffer[position] != rune(';') {
					goto l44
				}
				position++
				if !_rules[ruleAction3]() {
					goto l44
				}
				add(ruleSelectStatement, position45)
			}
			return true
		l44:
			position, tokenIndex = position44, tokenIndex44
			return false
		},
		/* 12 InsertStatement <- <(('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T') _ (('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O')) _ InsertBody Action4)> */
		func() bool {
			position66, tokenIndex66 := position, tokenIndex
			{
				position67 := position
				{
					position68, tokenIndex68 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l69
					}
					position++
					goto l68
				l69:
					position, tokenIndex = position68, tokenIndex68
					if buffer[position] != rune('I') {
						goto l66
					}
					position++
				}
			l68:
				{
					position70, tokenIndex70 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l71
					}
					position++
					goto l70
				l71:
					position, tokenIndex = position70, tokenIndex70
					if buffer[position] != rune('N') {
						goto l66
					}
					position++
				}
			l70:
				{
					position72, tokenIndex72 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l73
					}
					position++
					goto l72
				l73:
					position, tokenIndex = position72, tokenIndex72
					if buffer[position] != rune('S') {
						goto l66
					}
					position++
				}
			l72:
				{
					position74, tokenIndex74 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l75
					}
					position++
					goto l74
				l75:
					position, tokenIndex = position74, tokenIndex74
					if buffer[position] != rune('E') {
						goto l66
					}
					position++
				}
			l74:
				{
					position76, tokenIndex76 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l77
					}
					position++
					goto l76
				l77:
					position, tokenIndex = position76, tokenIndex76
					if buffer[position] != rune('R') {
						goto l66
					}
					position++
				}
			l76:
				{
					position78, tokenIndex78 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l79
					}
					position++
					goto l78
				l79:
					position, tokenIndex = position78, tokenIndex78
					if buffer[position] != rune('T') {
						goto l66
					}
					position++
				}
			l78:
				if !_rules[rule_]() {
					goto l66
				}
				{
					position80, tokenIndex80 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l81
					}
					position++
					goto l80
				l81:
					position, tokenIndex = position80, tokenIndex80
					if buffer[position] != rune('I') {
						goto l66
					}
					position++
				}
			l80:
				{
					position82, tokenIndex82 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l83
					}
					position++
					goto l82
				l83:
					position, tokenIndex = position82, tokenIndex82
					if buffer[position] != rune('N') {
						goto l66
					}
					position++
				}
			l82:
				{
					position84, tokenIndex84 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l85
					}
					position++
					goto l84
				l85:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('T') {
						goto l66
					}
					position++
				}
			l84:
				{
					position86, tokenIndex86 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l87
					}
					position++
					goto l86
				l87:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('O') {
						goto l66
					}
					position++
				}
			l86:
				if !_rules[rule_]() {
					goto l66
				}
				if !_rules[ruleInsertBody]() {
					goto l66
				}
				if !_rules[ruleAction4]() {
					goto l66
				}
				add(ruleInsertStatement, position67)
			}
			return true
		l66:
			position, tokenIndex = position66, tokenIndex66
			return false
		},
		/* 13 CreateStatement <- <(('c' / 'C') ('r' / 'R') ('e' / 'E') ('a' / 'A') ('t' / 'T') ('e' / 'E') _ (('t' / 'T') ('a' / 'A') ('b' / 'B') ('l' / 'L') ('e' / 'E')) _ CreateTable _ CreateParaColList _ ';' Action5)> */
		func() bool {
			position88, tokenIndex88 := position, tokenIndex
			{
				position89 := position
				{
					position90, tokenIndex90 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l91
					}
					position++
					goto l90
				l91:
					position, tokenIndex = position90, tokenIndex90
					if buffer[position] != rune('C') {
						goto l88
					}
					position++
				}
			l90:
				{
					position92, tokenIndex92 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l93
					}
					position++
					goto l92
				l93:
					position, tokenIndex = position92, tokenIndex92
					if buffer[position] != rune('R') {
						goto l88
					}
					position++
				}
			l92:
				{
					position94, tokenIndex94 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l95
					}
					position++
					goto l94
				l95:
					position, tokenIndex = position94, tokenIndex94
					if buffer[position] != rune('E') {
						goto l88
					}
					position++
				}
			l94:
				{
					position96, tokenIndex96 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l97
					}
					position++
					goto l96
				l97:
					position, tokenIndex = position96, tokenIndex96
					if buffer[position] != rune('A') {
						goto l88
					}
					position++
				}
			l96:
				{
					position98, tokenIndex98 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l99
					}
					position++
					goto l98
				l99:
					position, tokenIndex = position98, tokenIndex98
					if buffer[position] != rune('T') {
						goto l88
					}
					position++
				}
			l98:
				{
					position100, tokenIndex100 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l101
					}
					position++
					goto l100
				l101:
					position, tokenIndex = position100, tokenIndex100
					if buffer[position] != rune('E') {
						goto l88
					}
					position++
				}
			l100:
				if !_rules[rule_]() {
					goto l88
				}
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
						goto l88
					}
					position++
				}
			l102:
				{
					position104, tokenIndex104 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l105
					}
					position++
					goto l104
				l105:
					position, tokenIndex = position104, tokenIndex104
					if buffer[position] != rune('A') {
						goto l88
					}
					position++
				}
			l104:
				{
					position106, tokenIndex106 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l107
					}
					position++
					goto l106
				l107:
					position, tokenIndex = position106, tokenIndex106
					if buffer[position] != rune('B') {
						goto l88
					}
					position++
				}
			l106:
				{
					position108, tokenIndex108 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l109
					}
					position++
					goto l108
				l109:
					position, tokenIndex = position108, tokenIndex108
					if buffer[position] != rune('L') {
						goto l88
					}
					position++
				}
			l108:
				{
					position110, tokenIndex110 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l111
					}
					position++
					goto l110
				l111:
					position, tokenIndex = position110, tokenIndex110
					if buffer[position] != rune('E') {
						goto l88
					}
					position++
				}
			l110:
				if !_rules[rule_]() {
					goto l88
				}
				if !_rules[ruleCreateTable]() {
					goto l88
				}
				if !_rules[rule_]() {
					goto l88
				}
				if !_rules[ruleCreateParaColList]() {
					goto l88
				}
				if !_rules[rule_]() {
					goto l88
				}
				if buffer[position] != rune(';') {
					goto l88
				}
				position++
				if !_rules[ruleAction5]() {
					goto l88
				}
				add(ruleCreateStatement, position89)
			}
			return true
		l88:
			position, tokenIndex = position88, tokenIndex88
			return false
		},
		/* 14 InsertBody <- <(InsertTable _ Values _ ';')> */
		func() bool {
			position112, tokenIndex112 := position, tokenIndex
			{
				position113 := position
				if !_rules[ruleInsertTable]() {
					goto l112
				}
				if !_rules[rule_]() {
					goto l112
				}
				if !_rules[ruleValues]() {
					goto l112
				}
				if !_rules[rule_]() {
					goto l112
				}
				if buffer[position] != rune(';') {
					goto l112
				}
				position++
				add(ruleInsertBody, position113)
			}
			return true
		l112:
			position, tokenIndex = position112, tokenIndex112
			return false
		},
		/* 15 Values <- <(InsertParaColList? _ (('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S')) _ ValuesBody)> */
		func() bool {
			position114, tokenIndex114 := position, tokenIndex
			{
				position115 := position
				{
					position116, tokenIndex116 := position, tokenIndex
					if !_rules[ruleInsertParaColList]() {
						goto l116
					}
					goto l117
				l116:
					position, tokenIndex = position116, tokenIndex116
				}
			l117:
				if !_rules[rule_]() {
					goto l114
				}
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
						goto l114
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
						goto l114
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
						goto l114
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
						goto l114
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
						goto l114
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
						goto l114
					}
					position++
				}
			l128:
				if !_rules[rule_]() {
					goto l114
				}
				if !_rules[ruleValuesBody]() {
					goto l114
				}
				add(ruleValues, position115)
			}
			return true
		l114:
			position, tokenIndex = position114, tokenIndex114
			return false
		},
		/* 16 ValuesBody <- <('(' _ ValList _ ')')> */
		func() bool {
			position130, tokenIndex130 := position, tokenIndex
			{
				position131 := position
				if buffer[position] != rune('(') {
					goto l130
				}
				position++
				if !_rules[rule_]() {
					goto l130
				}
				if !_rules[ruleValList]() {
					goto l130
				}
				if !_rules[rule_]() {
					goto l130
				}
				if buffer[position] != rune(')') {
					goto l130
				}
				position++
				add(ruleValuesBody, position131)
			}
			return true
		l130:
			position, tokenIndex = position130, tokenIndex130
			return false
		},
		/* 17 ValList <- <((_ InsertValue _ ',' _)* _ InsertValue)> */
		func() bool {
			position132, tokenIndex132 := position, tokenIndex
			{
				position133 := position
			l134:
				{
					position135, tokenIndex135 := position, tokenIndex
					if !_rules[rule_]() {
						goto l135
					}
					if !_rules[ruleInsertValue]() {
						goto l135
					}
					if !_rules[rule_]() {
						goto l135
					}
					if buffer[position] != rune(',') {
						goto l135
					}
					position++
					if !_rules[rule_]() {
						goto l135
					}
					goto l134
				l135:
					position, tokenIndex = position135, tokenIndex135
				}
				if !_rules[rule_]() {
					goto l132
				}
				if !_rules[ruleInsertValue]() {
					goto l132
				}
				add(ruleValList, position133)
			}
			return true
		l132:
			position, tokenIndex = position132, tokenIndex132
			return false
		},
		/* 18 InsertValue <- <('"'? EachValue '"'?)> */
		func() bool {
			position136, tokenIndex136 := position, tokenIndex
			{
				position137 := position
				{
					position138, tokenIndex138 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l138
					}
					position++
					goto l139
				l138:
					position, tokenIndex = position138, tokenIndex138
				}
			l139:
				if !_rules[ruleEachValue]() {
					goto l136
				}
				{
					position140, tokenIndex140 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l140
					}
					position++
					goto l141
				l140:
					position, tokenIndex = position140, tokenIndex140
				}
			l141:
				add(ruleInsertValue, position137)
			}
			return true
		l136:
			position, tokenIndex = position136, tokenIndex136
			return false
		},
		/* 19 EachValue <- <(<AlphaNum+> Action6)> */
		func() bool {
			position142, tokenIndex142 := position, tokenIndex
			{
				position143 := position
				{
					position144 := position
					if !_rules[ruleAlphaNum]() {
						goto l142
					}
				l145:
					{
						position146, tokenIndex146 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l146
						}
						goto l145
					l146:
						position, tokenIndex = position146, tokenIndex146
					}
					add(rulePegText, position144)
				}
				if !_rules[ruleAction6]() {
					goto l142
				}
				add(ruleEachValue, position143)
			}
			return true
		l142:
			position, tokenIndex = position142, tokenIndex142
			return false
		},
		/* 20 Asterisk <- <('*' Action7)> */
		func() bool {
			position147, tokenIndex147 := position, tokenIndex
			{
				position148 := position
				if buffer[position] != rune('*') {
					goto l147
				}
				position++
				if !_rules[ruleAction7]() {
					goto l147
				}
				add(ruleAsterisk, position148)
			}
			return true
		l147:
			position, tokenIndex = position147, tokenIndex147
			return false
		},
		/* 21 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position149, tokenIndex149 := position, tokenIndex
			{
				position150 := position
				if buffer[position] != rune('/') {
					goto l149
				}
				position++
				if buffer[position] != rune('/') {
					goto l149
				}
				position++
			l151:
				{
					position152, tokenIndex152 := position, tokenIndex
					{
						position153, tokenIndex153 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l153
						}
						position++
						goto l152
					l153:
						position, tokenIndex = position153, tokenIndex153
					}
					if !matchDot() {
						goto l152
					}
					goto l151
				l152:
					position, tokenIndex = position152, tokenIndex152
				}
				add(rulelineComment, position150)
			}
			return true
		l149:
			position, tokenIndex = position149, tokenIndex149
			return false
		},
		/* 22 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position154, tokenIndex154 := position, tokenIndex
			{
				position155 := position
				if buffer[position] != rune('/') {
					goto l154
				}
				position++
				if buffer[position] != rune('*') {
					goto l154
				}
				position++
			l156:
				{
					position157, tokenIndex157 := position, tokenIndex
					{
						position158, tokenIndex158 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l158
						}
						position++
						if buffer[position] != rune('/') {
							goto l158
						}
						position++
						goto l157
					l158:
						position, tokenIndex = position158, tokenIndex158
					}
					{
						position159, tokenIndex159 := position, tokenIndex
						if !matchDot() {
							goto l160
						}
						goto l159
					l160:
						position, tokenIndex = position159, tokenIndex159
						if buffer[position] != rune('\n') {
							goto l157
						}
						position++
					}
				l159:
					goto l156
				l157:
					position, tokenIndex = position157, tokenIndex157
				}
				if buffer[position] != rune('*') {
					goto l154
				}
				position++
				if buffer[position] != rune('/') {
					goto l154
				}
				position++
				add(ruleblockComment, position155)
			}
			return true
		l154:
			position, tokenIndex = position154, tokenIndex154
			return false
		},
		/* 23 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position161, tokenIndex161 := position, tokenIndex
			{
				position162 := position
				{
					position163, tokenIndex163 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l164
					}
					position++
					goto l163
				l164:
					position, tokenIndex = position163, tokenIndex163
					if buffer[position] != rune('\t') {
						goto l165
					}
					position++
					goto l163
				l165:
					position, tokenIndex = position163, tokenIndex163
					if buffer[position] != rune('\n') {
						goto l166
					}
					position++
					goto l163
				l166:
					position, tokenIndex = position163, tokenIndex163
					if buffer[position] != rune('\r') {
						goto l161
					}
					position++
				}
			l163:
				add(rulews, position162)
			}
			return true
		l161:
			position, tokenIndex = position161, tokenIndex161
			return false
		},
		/* 24 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position168 := position
			l169:
				{
					position170, tokenIndex170 := position, tokenIndex
					{
						position171, tokenIndex171 := position, tokenIndex
						if !_rules[rulews]() {
							goto l172
						}
						goto l171
					l172:
						position, tokenIndex = position171, tokenIndex171
						if !_rules[rulelineComment]() {
							goto l173
						}
						goto l171
					l173:
						position, tokenIndex = position171, tokenIndex171
						if !_rules[ruleblockComment]() {
							goto l170
						}
					}
				l171:
					goto l169
				l170:
					position, tokenIndex = position170, tokenIndex170
				}
				add(rule_, position168)
			}
			return true
		},
		/* 25 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position174, tokenIndex174 := position, tokenIndex
			{
				position175 := position
				{
					position176, tokenIndex176 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l177
					}
					position++
					goto l176
				l177:
					position, tokenIndex = position176, tokenIndex176
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l178
					}
					position++
					goto l176
				l178:
					position, tokenIndex = position176, tokenIndex176
					if buffer[position] != rune('_') {
						goto l174
					}
					position++
				}
			l176:
				add(ruleLetter, position175)
			}
			return true
		l174:
			position, tokenIndex = position174, tokenIndex174
			return false
		},
		/* 26 Number <- <([0-9] ('.' [0-9])*)> */
		func() bool {
			position179, tokenIndex179 := position, tokenIndex
			{
				position180 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l179
				}
				position++
			l181:
				{
					position182, tokenIndex182 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l182
					}
					position++
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l182
					}
					position++
					goto l181
				l182:
					position, tokenIndex = position182, tokenIndex182
				}
				add(ruleNumber, position180)
			}
			return true
		l179:
			position, tokenIndex = position179, tokenIndex179
			return false
		},
		/* 27 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position183, tokenIndex183 := position, tokenIndex
			{
				position184 := position
				{
					position185, tokenIndex185 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l186
					}
					goto l185
				l186:
					position, tokenIndex = position185, tokenIndex185
					if !_rules[ruleNumber]() {
						goto l183
					}
				}
			l185:
				add(ruleAlphaNum, position184)
			}
			return true
		l183:
			position, tokenIndex = position183, tokenIndex183
			return false
		},
		/* 28 SelectTable <- <(<AlphaNum+> Action8)> */
		func() bool {
			position187, tokenIndex187 := position, tokenIndex
			{
				position188 := position
				{
					position189 := position
					if !_rules[ruleAlphaNum]() {
						goto l187
					}
				l190:
					{
						position191, tokenIndex191 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l191
						}
						goto l190
					l191:
						position, tokenIndex = position191, tokenIndex191
					}
					add(rulePegText, position189)
				}
				if !_rules[ruleAction8]() {
					goto l187
				}
				add(ruleSelectTable, position188)
			}
			return true
		l187:
			position, tokenIndex = position187, tokenIndex187
			return false
		},
		/* 29 InsertTable <- <(<AlphaNum+> Action9)> */
		func() bool {
			position192, tokenIndex192 := position, tokenIndex
			{
				position193 := position
				{
					position194 := position
					if !_rules[ruleAlphaNum]() {
						goto l192
					}
				l195:
					{
						position196, tokenIndex196 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l196
						}
						goto l195
					l196:
						position, tokenIndex = position196, tokenIndex196
					}
					add(rulePegText, position194)
				}
				if !_rules[ruleAction9]() {
					goto l192
				}
				add(ruleInsertTable, position193)
			}
			return true
		l192:
			position, tokenIndex = position192, tokenIndex192
			return false
		},
		/* 30 CreateTable <- <(<AlphaNum+> Action10)> */
		func() bool {
			position197, tokenIndex197 := position, tokenIndex
			{
				position198 := position
				{
					position199 := position
					if !_rules[ruleAlphaNum]() {
						goto l197
					}
				l200:
					{
						position201, tokenIndex201 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l201
						}
						goto l200
					l201:
						position, tokenIndex = position201, tokenIndex201
					}
					add(rulePegText, position199)
				}
				if !_rules[ruleAction10]() {
					goto l197
				}
				add(ruleCreateTable, position198)
			}
			return true
		l197:
			position, tokenIndex = position197, tokenIndex197
			return false
		},
		nil,
		/* 33 Action0 <- <{ p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 34 Action1 <- <{ p.InsertStatement.Columns = append(p.InsertStatement.Columns,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 35 Action2 <- <{ p.CreateStatement.Columns = append(p.CreateStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 36 Action3 <- <{ p.sType = Select  }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 37 Action4 <- <{ p.validateInsert() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 38 Action5 <- <{ p.setPartitionKey(buffer[begin:end])}> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 39 Action6 <- <{ p.captureValues(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 40 Action7 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 41 Action8 <- <{ p.SelectStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 42 Action9 <- <{ p.InsertStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 43 Action10 <- <{ p.CreateStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
	}
	p.rules = _rules
}
