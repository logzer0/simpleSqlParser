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
	ruleSelectTable
	ruleInsertTable
	rulePegText
	ruleAction0
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
	"SelectParaColList",
	"SelectColList",
	"SelectColName",
	"InsertParaColList",
	"InsertColList",
	"InsertColName",
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
	"SelectTable",
	"InsertTable",
	"PegText",
	"Action0",
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
	sType
	SelectStatement
	InsertStatement

	Buffer string
	buffer []rune
	rules  [36]func() bool
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
			p.sType = Select
		case ruleAction3:
			p.validateInsert()
		case ruleAction4:
			p.captureValues(buffer[begin:end])
		case ruleAction5:
			p.SelectStatement.AllColumns = true
		case ruleAction6:
			p.SelectStatement.TableName = buffer[begin:end]
		case ruleAction7:
			p.InsertStatement.TableName = buffer[begin:end]

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
		/* 1 Columns <- <(SelectParaColList / SelectColList / Asterisk)> */
		func() bool {
			position5, tokenIndex5 := position, tokenIndex
			{
				position6 := position
				{
					position7, tokenIndex7 := position, tokenIndex
					if !_rules[ruleSelectParaColList]() {
						goto l8
					}
					goto l7
				l8:
					position, tokenIndex = position7, tokenIndex7
					if !_rules[ruleSelectColList]() {
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
		/* 2 SelectParaColList <- <('(' _ SelectColList _ ')')> */
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
				if !_rules[ruleSelectColList]() {
					goto l10
				}
				if !_rules[rule_]() {
					goto l10
				}
				if buffer[position] != rune(')') {
					goto l10
				}
				position++
				add(ruleSelectParaColList, position11)
			}
			return true
		l10:
			position, tokenIndex = position10, tokenIndex10
			return false
		},
		/* 3 SelectColList <- <((SelectColName _ ',' _)* SelectColName)> */
		func() bool {
			position12, tokenIndex12 := position, tokenIndex
			{
				position13 := position
			l14:
				{
					position15, tokenIndex15 := position, tokenIndex
					if !_rules[ruleSelectColName]() {
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
				if !_rules[ruleSelectColName]() {
					goto l12
				}
				add(ruleSelectColList, position13)
			}
			return true
		l12:
			position, tokenIndex = position12, tokenIndex12
			return false
		},
		/* 4 SelectColName <- <(<AlphaNum+> Action0)> */
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
				if !_rules[ruleAction0]() {
					goto l16
				}
				add(ruleSelectColName, position17)
			}
			return true
		l16:
			position, tokenIndex = position16, tokenIndex16
			return false
		},
		/* 5 InsertParaColList <- <('(' _ InsertColList _ ')')> */
		func() bool {
			position21, tokenIndex21 := position, tokenIndex
			{
				position22 := position
				if buffer[position] != rune('(') {
					goto l21
				}
				position++
				if !_rules[rule_]() {
					goto l21
				}
				if !_rules[ruleInsertColList]() {
					goto l21
				}
				if !_rules[rule_]() {
					goto l21
				}
				if buffer[position] != rune(')') {
					goto l21
				}
				position++
				add(ruleInsertParaColList, position22)
			}
			return true
		l21:
			position, tokenIndex = position21, tokenIndex21
			return false
		},
		/* 6 InsertColList <- <((InsertColName _ ',' _)* InsertColName)> */
		func() bool {
			position23, tokenIndex23 := position, tokenIndex
			{
				position24 := position
			l25:
				{
					position26, tokenIndex26 := position, tokenIndex
					if !_rules[ruleInsertColName]() {
						goto l26
					}
					if !_rules[rule_]() {
						goto l26
					}
					if buffer[position] != rune(',') {
						goto l26
					}
					position++
					if !_rules[rule_]() {
						goto l26
					}
					goto l25
				l26:
					position, tokenIndex = position26, tokenIndex26
				}
				if !_rules[ruleInsertColName]() {
					goto l23
				}
				add(ruleInsertColList, position24)
			}
			return true
		l23:
			position, tokenIndex = position23, tokenIndex23
			return false
		},
		/* 7 InsertColName <- <(<AlphaNum+> Action1)> */
		func() bool {
			position27, tokenIndex27 := position, tokenIndex
			{
				position28 := position
				{
					position29 := position
					if !_rules[ruleAlphaNum]() {
						goto l27
					}
				l30:
					{
						position31, tokenIndex31 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l31
						}
						goto l30
					l31:
						position, tokenIndex = position31, tokenIndex31
					}
					add(rulePegText, position29)
				}
				if !_rules[ruleAction1]() {
					goto l27
				}
				add(ruleInsertColName, position28)
			}
			return true
		l27:
			position, tokenIndex = position27, tokenIndex27
			return false
		},
		/* 8 SelectStatement <- <(('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T') _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ SelectTable _ ';' Action2)> */
		func() bool {
			position32, tokenIndex32 := position, tokenIndex
			{
				position33 := position
				{
					position34, tokenIndex34 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l35
					}
					position++
					goto l34
				l35:
					position, tokenIndex = position34, tokenIndex34
					if buffer[position] != rune('S') {
						goto l32
					}
					position++
				}
			l34:
				{
					position36, tokenIndex36 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l37
					}
					position++
					goto l36
				l37:
					position, tokenIndex = position36, tokenIndex36
					if buffer[position] != rune('E') {
						goto l32
					}
					position++
				}
			l36:
				{
					position38, tokenIndex38 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l39
					}
					position++
					goto l38
				l39:
					position, tokenIndex = position38, tokenIndex38
					if buffer[position] != rune('L') {
						goto l32
					}
					position++
				}
			l38:
				{
					position40, tokenIndex40 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l41
					}
					position++
					goto l40
				l41:
					position, tokenIndex = position40, tokenIndex40
					if buffer[position] != rune('E') {
						goto l32
					}
					position++
				}
			l40:
				{
					position42, tokenIndex42 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l43
					}
					position++
					goto l42
				l43:
					position, tokenIndex = position42, tokenIndex42
					if buffer[position] != rune('C') {
						goto l32
					}
					position++
				}
			l42:
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
						goto l32
					}
					position++
				}
			l44:
				if !_rules[rule_]() {
					goto l32
				}
				if !_rules[ruleColumns]() {
					goto l32
				}
				if !_rules[rule_]() {
					goto l32
				}
				{
					position46, tokenIndex46 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l47
					}
					position++
					goto l46
				l47:
					position, tokenIndex = position46, tokenIndex46
					if buffer[position] != rune('F') {
						goto l32
					}
					position++
				}
			l46:
				{
					position48, tokenIndex48 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l49
					}
					position++
					goto l48
				l49:
					position, tokenIndex = position48, tokenIndex48
					if buffer[position] != rune('R') {
						goto l32
					}
					position++
				}
			l48:
				{
					position50, tokenIndex50 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l51
					}
					position++
					goto l50
				l51:
					position, tokenIndex = position50, tokenIndex50
					if buffer[position] != rune('O') {
						goto l32
					}
					position++
				}
			l50:
				{
					position52, tokenIndex52 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l53
					}
					position++
					goto l52
				l53:
					position, tokenIndex = position52, tokenIndex52
					if buffer[position] != rune('M') {
						goto l32
					}
					position++
				}
			l52:
				if !_rules[rule_]() {
					goto l32
				}
				if !_rules[ruleSelectTable]() {
					goto l32
				}
				if !_rules[rule_]() {
					goto l32
				}
				if buffer[position] != rune(';') {
					goto l32
				}
				position++
				if !_rules[ruleAction2]() {
					goto l32
				}
				add(ruleSelectStatement, position33)
			}
			return true
		l32:
			position, tokenIndex = position32, tokenIndex32
			return false
		},
		/* 9 InsertStatement <- <(('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T') _ (('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O')) _ InsertBody Action3)> */
		func() bool {
			position54, tokenIndex54 := position, tokenIndex
			{
				position55 := position
				{
					position56, tokenIndex56 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l57
					}
					position++
					goto l56
				l57:
					position, tokenIndex = position56, tokenIndex56
					if buffer[position] != rune('I') {
						goto l54
					}
					position++
				}
			l56:
				{
					position58, tokenIndex58 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l59
					}
					position++
					goto l58
				l59:
					position, tokenIndex = position58, tokenIndex58
					if buffer[position] != rune('N') {
						goto l54
					}
					position++
				}
			l58:
				{
					position60, tokenIndex60 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l61
					}
					position++
					goto l60
				l61:
					position, tokenIndex = position60, tokenIndex60
					if buffer[position] != rune('S') {
						goto l54
					}
					position++
				}
			l60:
				{
					position62, tokenIndex62 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l63
					}
					position++
					goto l62
				l63:
					position, tokenIndex = position62, tokenIndex62
					if buffer[position] != rune('E') {
						goto l54
					}
					position++
				}
			l62:
				{
					position64, tokenIndex64 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l65
					}
					position++
					goto l64
				l65:
					position, tokenIndex = position64, tokenIndex64
					if buffer[position] != rune('R') {
						goto l54
					}
					position++
				}
			l64:
				{
					position66, tokenIndex66 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l67
					}
					position++
					goto l66
				l67:
					position, tokenIndex = position66, tokenIndex66
					if buffer[position] != rune('T') {
						goto l54
					}
					position++
				}
			l66:
				if !_rules[rule_]() {
					goto l54
				}
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
						goto l54
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
						goto l54
					}
					position++
				}
			l70:
				{
					position72, tokenIndex72 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l73
					}
					position++
					goto l72
				l73:
					position, tokenIndex = position72, tokenIndex72
					if buffer[position] != rune('T') {
						goto l54
					}
					position++
				}
			l72:
				{
					position74, tokenIndex74 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l75
					}
					position++
					goto l74
				l75:
					position, tokenIndex = position74, tokenIndex74
					if buffer[position] != rune('O') {
						goto l54
					}
					position++
				}
			l74:
				if !_rules[rule_]() {
					goto l54
				}
				if !_rules[ruleInsertBody]() {
					goto l54
				}
				if !_rules[ruleAction3]() {
					goto l54
				}
				add(ruleInsertStatement, position55)
			}
			return true
		l54:
			position, tokenIndex = position54, tokenIndex54
			return false
		},
		/* 10 InsertBody <- <(InsertTable _ Values _ ';')> */
		func() bool {
			position76, tokenIndex76 := position, tokenIndex
			{
				position77 := position
				if !_rules[ruleInsertTable]() {
					goto l76
				}
				if !_rules[rule_]() {
					goto l76
				}
				if !_rules[ruleValues]() {
					goto l76
				}
				if !_rules[rule_]() {
					goto l76
				}
				if buffer[position] != rune(';') {
					goto l76
				}
				position++
				add(ruleInsertBody, position77)
			}
			return true
		l76:
			position, tokenIndex = position76, tokenIndex76
			return false
		},
		/* 11 Values <- <(InsertParaColList? _ (('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S')) _ ValuesBody)> */
		func() bool {
			position78, tokenIndex78 := position, tokenIndex
			{
				position79 := position
				{
					position80, tokenIndex80 := position, tokenIndex
					if !_rules[ruleInsertParaColList]() {
						goto l80
					}
					goto l81
				l80:
					position, tokenIndex = position80, tokenIndex80
				}
			l81:
				if !_rules[rule_]() {
					goto l78
				}
				{
					position82, tokenIndex82 := position, tokenIndex
					if buffer[position] != rune('v') {
						goto l83
					}
					position++
					goto l82
				l83:
					position, tokenIndex = position82, tokenIndex82
					if buffer[position] != rune('V') {
						goto l78
					}
					position++
				}
			l82:
				{
					position84, tokenIndex84 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l85
					}
					position++
					goto l84
				l85:
					position, tokenIndex = position84, tokenIndex84
					if buffer[position] != rune('A') {
						goto l78
					}
					position++
				}
			l84:
				{
					position86, tokenIndex86 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l87
					}
					position++
					goto l86
				l87:
					position, tokenIndex = position86, tokenIndex86
					if buffer[position] != rune('L') {
						goto l78
					}
					position++
				}
			l86:
				{
					position88, tokenIndex88 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l89
					}
					position++
					goto l88
				l89:
					position, tokenIndex = position88, tokenIndex88
					if buffer[position] != rune('U') {
						goto l78
					}
					position++
				}
			l88:
				{
					position90, tokenIndex90 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l91
					}
					position++
					goto l90
				l91:
					position, tokenIndex = position90, tokenIndex90
					if buffer[position] != rune('E') {
						goto l78
					}
					position++
				}
			l90:
				{
					position92, tokenIndex92 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l93
					}
					position++
					goto l92
				l93:
					position, tokenIndex = position92, tokenIndex92
					if buffer[position] != rune('S') {
						goto l78
					}
					position++
				}
			l92:
				if !_rules[rule_]() {
					goto l78
				}
				if !_rules[ruleValuesBody]() {
					goto l78
				}
				add(ruleValues, position79)
			}
			return true
		l78:
			position, tokenIndex = position78, tokenIndex78
			return false
		},
		/* 12 ValuesBody <- <('(' _ ValList _ ')')> */
		func() bool {
			position94, tokenIndex94 := position, tokenIndex
			{
				position95 := position
				if buffer[position] != rune('(') {
					goto l94
				}
				position++
				if !_rules[rule_]() {
					goto l94
				}
				if !_rules[ruleValList]() {
					goto l94
				}
				if !_rules[rule_]() {
					goto l94
				}
				if buffer[position] != rune(')') {
					goto l94
				}
				position++
				add(ruleValuesBody, position95)
			}
			return true
		l94:
			position, tokenIndex = position94, tokenIndex94
			return false
		},
		/* 13 ValList <- <((_ InsertValue _ ',' _)* _ InsertValue)> */
		func() bool {
			position96, tokenIndex96 := position, tokenIndex
			{
				position97 := position
			l98:
				{
					position99, tokenIndex99 := position, tokenIndex
					if !_rules[rule_]() {
						goto l99
					}
					if !_rules[ruleInsertValue]() {
						goto l99
					}
					if !_rules[rule_]() {
						goto l99
					}
					if buffer[position] != rune(',') {
						goto l99
					}
					position++
					if !_rules[rule_]() {
						goto l99
					}
					goto l98
				l99:
					position, tokenIndex = position99, tokenIndex99
				}
				if !_rules[rule_]() {
					goto l96
				}
				if !_rules[ruleInsertValue]() {
					goto l96
				}
				add(ruleValList, position97)
			}
			return true
		l96:
			position, tokenIndex = position96, tokenIndex96
			return false
		},
		/* 14 InsertValue <- <('"'? EachValue '"'?)> */
		func() bool {
			position100, tokenIndex100 := position, tokenIndex
			{
				position101 := position
				{
					position102, tokenIndex102 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l102
					}
					position++
					goto l103
				l102:
					position, tokenIndex = position102, tokenIndex102
				}
			l103:
				if !_rules[ruleEachValue]() {
					goto l100
				}
				{
					position104, tokenIndex104 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l104
					}
					position++
					goto l105
				l104:
					position, tokenIndex = position104, tokenIndex104
				}
			l105:
				add(ruleInsertValue, position101)
			}
			return true
		l100:
			position, tokenIndex = position100, tokenIndex100
			return false
		},
		/* 15 EachValue <- <(<AlphaNum+> Action4)> */
		func() bool {
			position106, tokenIndex106 := position, tokenIndex
			{
				position107 := position
				{
					position108 := position
					if !_rules[ruleAlphaNum]() {
						goto l106
					}
				l109:
					{
						position110, tokenIndex110 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l110
						}
						goto l109
					l110:
						position, tokenIndex = position110, tokenIndex110
					}
					add(rulePegText, position108)
				}
				if !_rules[ruleAction4]() {
					goto l106
				}
				add(ruleEachValue, position107)
			}
			return true
		l106:
			position, tokenIndex = position106, tokenIndex106
			return false
		},
		/* 16 Asterisk <- <('*' Action5)> */
		func() bool {
			position111, tokenIndex111 := position, tokenIndex
			{
				position112 := position
				if buffer[position] != rune('*') {
					goto l111
				}
				position++
				if !_rules[ruleAction5]() {
					goto l111
				}
				add(ruleAsterisk, position112)
			}
			return true
		l111:
			position, tokenIndex = position111, tokenIndex111
			return false
		},
		/* 17 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position113, tokenIndex113 := position, tokenIndex
			{
				position114 := position
				if buffer[position] != rune('/') {
					goto l113
				}
				position++
				if buffer[position] != rune('/') {
					goto l113
				}
				position++
			l115:
				{
					position116, tokenIndex116 := position, tokenIndex
					{
						position117, tokenIndex117 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l117
						}
						position++
						goto l116
					l117:
						position, tokenIndex = position117, tokenIndex117
					}
					if !matchDot() {
						goto l116
					}
					goto l115
				l116:
					position, tokenIndex = position116, tokenIndex116
				}
				add(rulelineComment, position114)
			}
			return true
		l113:
			position, tokenIndex = position113, tokenIndex113
			return false
		},
		/* 18 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position118, tokenIndex118 := position, tokenIndex
			{
				position119 := position
				if buffer[position] != rune('/') {
					goto l118
				}
				position++
				if buffer[position] != rune('*') {
					goto l118
				}
				position++
			l120:
				{
					position121, tokenIndex121 := position, tokenIndex
					{
						position122, tokenIndex122 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l122
						}
						position++
						if buffer[position] != rune('/') {
							goto l122
						}
						position++
						goto l121
					l122:
						position, tokenIndex = position122, tokenIndex122
					}
					{
						position123, tokenIndex123 := position, tokenIndex
						if !matchDot() {
							goto l124
						}
						goto l123
					l124:
						position, tokenIndex = position123, tokenIndex123
						if buffer[position] != rune('\n') {
							goto l121
						}
						position++
					}
				l123:
					goto l120
				l121:
					position, tokenIndex = position121, tokenIndex121
				}
				if buffer[position] != rune('*') {
					goto l118
				}
				position++
				if buffer[position] != rune('/') {
					goto l118
				}
				position++
				add(ruleblockComment, position119)
			}
			return true
		l118:
			position, tokenIndex = position118, tokenIndex118
			return false
		},
		/* 19 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position125, tokenIndex125 := position, tokenIndex
			{
				position126 := position
				{
					position127, tokenIndex127 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l128
					}
					position++
					goto l127
				l128:
					position, tokenIndex = position127, tokenIndex127
					if buffer[position] != rune('\t') {
						goto l129
					}
					position++
					goto l127
				l129:
					position, tokenIndex = position127, tokenIndex127
					if buffer[position] != rune('\n') {
						goto l130
					}
					position++
					goto l127
				l130:
					position, tokenIndex = position127, tokenIndex127
					if buffer[position] != rune('\r') {
						goto l125
					}
					position++
				}
			l127:
				add(rulews, position126)
			}
			return true
		l125:
			position, tokenIndex = position125, tokenIndex125
			return false
		},
		/* 20 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position132 := position
			l133:
				{
					position134, tokenIndex134 := position, tokenIndex
					{
						position135, tokenIndex135 := position, tokenIndex
						if !_rules[rulews]() {
							goto l136
						}
						goto l135
					l136:
						position, tokenIndex = position135, tokenIndex135
						if !_rules[rulelineComment]() {
							goto l137
						}
						goto l135
					l137:
						position, tokenIndex = position135, tokenIndex135
						if !_rules[ruleblockComment]() {
							goto l134
						}
					}
				l135:
					goto l133
				l134:
					position, tokenIndex = position134, tokenIndex134
				}
				add(rule_, position132)
			}
			return true
		},
		/* 21 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position138, tokenIndex138 := position, tokenIndex
			{
				position139 := position
				{
					position140, tokenIndex140 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l141
					}
					position++
					goto l140
				l141:
					position, tokenIndex = position140, tokenIndex140
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l142
					}
					position++
					goto l140
				l142:
					position, tokenIndex = position140, tokenIndex140
					if buffer[position] != rune('_') {
						goto l138
					}
					position++
				}
			l140:
				add(ruleLetter, position139)
			}
			return true
		l138:
			position, tokenIndex = position138, tokenIndex138
			return false
		},
		/* 22 Number <- <([0-9] ('.' [0-9])*)> */
		func() bool {
			position143, tokenIndex143 := position, tokenIndex
			{
				position144 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l143
				}
				position++
			l145:
				{
					position146, tokenIndex146 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l146
					}
					position++
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l146
					}
					position++
					goto l145
				l146:
					position, tokenIndex = position146, tokenIndex146
				}
				add(ruleNumber, position144)
			}
			return true
		l143:
			position, tokenIndex = position143, tokenIndex143
			return false
		},
		/* 23 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position147, tokenIndex147 := position, tokenIndex
			{
				position148 := position
				{
					position149, tokenIndex149 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l150
					}
					goto l149
				l150:
					position, tokenIndex = position149, tokenIndex149
					if !_rules[ruleNumber]() {
						goto l147
					}
				}
			l149:
				add(ruleAlphaNum, position148)
			}
			return true
		l147:
			position, tokenIndex = position147, tokenIndex147
			return false
		},
		/* 24 SelectTable <- <(<AlphaNum+> Action6)> */
		func() bool {
			position151, tokenIndex151 := position, tokenIndex
			{
				position152 := position
				{
					position153 := position
					if !_rules[ruleAlphaNum]() {
						goto l151
					}
				l154:
					{
						position155, tokenIndex155 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l155
						}
						goto l154
					l155:
						position, tokenIndex = position155, tokenIndex155
					}
					add(rulePegText, position153)
				}
				if !_rules[ruleAction6]() {
					goto l151
				}
				add(ruleSelectTable, position152)
			}
			return true
		l151:
			position, tokenIndex = position151, tokenIndex151
			return false
		},
		/* 25 InsertTable <- <(<AlphaNum+> Action7)> */
		func() bool {
			position156, tokenIndex156 := position, tokenIndex
			{
				position157 := position
				{
					position158 := position
					if !_rules[ruleAlphaNum]() {
						goto l156
					}
				l159:
					{
						position160, tokenIndex160 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l160
						}
						goto l159
					l160:
						position, tokenIndex = position160, tokenIndex160
					}
					add(rulePegText, position158)
				}
				if !_rules[ruleAction7]() {
					goto l156
				}
				add(ruleInsertTable, position157)
			}
			return true
		l156:
			position, tokenIndex = position156, tokenIndex156
			return false
		},
		nil,
		/* 28 Action0 <- <{ p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 29 Action1 <- <{ p.InsertStatement.Columns = append(p.InsertStatement.Columns,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 30 Action2 <- <{ p.sType = Select  }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 31 Action3 <- <{ p.validateInsert() }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 32 Action4 <- <{ p.captureValues(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 33 Action5 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 34 Action6 <- <{ p.SelectStatement.TableName = buffer [begin:end] }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 35 Action7 <- <{ p.InsertStatement.TableName = buffer[begin:end]  }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
	}
	p.rules = _rules
}
