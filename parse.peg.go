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
	ruleDropStatement
	ruleSelectStatement
	ruleInsertStatement
	ruleCreateStatement
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
	ruleSelectKeyspace
	ruleSelectTableName
	ruleInsertTable
	ruleInsertKeyspace
	ruleInsertTableName
	ruleCreateTable
	ruleCreateKeyspace
	ruleCreateTableName
	ruleDropTable
	ruleDropTableKeyspace
	ruleDropTableName
	ruleWHERE
	ruleFILTERS
	ruleFILTER
	ruleOperation
	ruleFilterColumn
	ruleFilterValue
	ruleLIMIT
	ruleLimitVal
	ruleAction0
	ruleAction1
	rulePegText
	ruleAction2
	ruleAction3
	ruleAction4
	ruleAction5
	ruleAction6
	ruleAction7
	ruleAction8
	ruleAction9
	ruleAction10
	ruleAction11
	ruleAction12
	ruleAction13
	ruleAction14
	ruleAction15
	ruleAction16
	ruleAction17
	ruleAction18
	ruleAction19
)

var rul3s = [...]string{
	"Unknown",
	"G",
	"DropStatement",
	"SelectStatement",
	"InsertStatement",
	"CreateStatement",
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
	"SelectKeyspace",
	"SelectTableName",
	"InsertTable",
	"InsertKeyspace",
	"InsertTableName",
	"CreateTable",
	"CreateKeyspace",
	"CreateTableName",
	"DropTable",
	"DropTableKeyspace",
	"DropTableName",
	"WHERE",
	"FILTERS",
	"FILTER",
	"Operation",
	"FilterColumn",
	"FilterValue",
	"LIMIT",
	"LimitVal",
	"Action0",
	"Action1",
	"PegText",
	"Action2",
	"Action3",
	"Action4",
	"Action5",
	"Action6",
	"Action7",
	"Action8",
	"Action9",
	"Action10",
	"Action11",
	"Action12",
	"Action13",
	"Action14",
	"Action15",
	"Action16",
	"Action17",
	"Action18",
	"Action19",
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
	DropStatement

	Buffer string
	buffer []rune
	rules  [71]func() bool
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
			p.validateInsert()
		case ruleAction1:
			p.setPartitionKey(buffer[begin:end])
		case ruleAction2:
			p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end])
		case ruleAction3:
			p.InsertStatement.Columns = append(p.InsertStatement.Columns, buffer[begin:end])
		case ruleAction4:
			p.CreateStatement.Columns = append(p.CreateStatement.Columns, buffer[begin:end])
		case ruleAction5:
			p.captureValues(buffer[begin:end])
		case ruleAction6:
			p.SelectStatement.AllColumns = true
		case ruleAction7:
			p.SelectStatement.Keyspace = buffer[begin:end]
		case ruleAction8:
			p.SelectStatement.TableName = buffer[begin:end]
		case ruleAction9:
			p.InsertStatement.Keyspace = buffer[begin:end]
		case ruleAction10:
			p.InsertStatement.TableName = buffer[begin:end]
		case ruleAction11:
			p.CreateStatement.Keyspace = buffer[begin:end]
		case ruleAction12:
			p.CreateStatement.TableName = buffer[begin:end]
		case ruleAction13:
			p.DropStatement.Keyspace = buffer[begin:end]
		case ruleAction14:
			p.DropStatement.TableName = buffer[begin:end]
		case ruleAction15:
			p.finalizeSelect()
		case ruleAction16:
			p.SelectStatement.Operators = append(p.SelectStatement.Operators, buffer[begin:end])
		case ruleAction17:
			p.SelectStatement.WhereColumns = append(p.SelectStatement.WhereColumns, buffer[begin:end])
		case ruleAction18:
			p.SelectStatement.WhereValues = append(p.SelectStatement.WhereValues, buffer[begin:end])
		case ruleAction19:
			var err error
			p.SelectStatement.Limit, err = strconv.Atoi(buffer[begin:end])
			if err != nil {
				p.SelectStatement.Limit = -1
			}

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
		/* 0 G <- <(SelectStatement / InsertStatement / CreateStatement / (DropStatement !.))> */
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
						goto l5
					}
					goto l2
				l5:
					position, tokenIndex = position2, tokenIndex2
					if !_rules[ruleDropStatement]() {
						goto l0
					}
					{
						position6, tokenIndex6 := position, tokenIndex
						if !matchDot() {
							goto l6
						}
						goto l0
					l6:
						position, tokenIndex = position6, tokenIndex6
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
		/* 1 DropStatement <- <(_ (('d' / 'D') ('r' / 'R') ('o' / 'O') ('p' / 'P')) _ DropTable _ ';' _)> */
		func() bool {
			position7, tokenIndex7 := position, tokenIndex
			{
				position8 := position
				if !_rules[rule_]() {
					goto l7
				}
				{
					position9, tokenIndex9 := position, tokenIndex
					if buffer[position] != rune('d') {
						goto l10
					}
					position++
					goto l9
				l10:
					position, tokenIndex = position9, tokenIndex9
					if buffer[position] != rune('D') {
						goto l7
					}
					position++
				}
			l9:
				{
					position11, tokenIndex11 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l12
					}
					position++
					goto l11
				l12:
					position, tokenIndex = position11, tokenIndex11
					if buffer[position] != rune('R') {
						goto l7
					}
					position++
				}
			l11:
				{
					position13, tokenIndex13 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l14
					}
					position++
					goto l13
				l14:
					position, tokenIndex = position13, tokenIndex13
					if buffer[position] != rune('O') {
						goto l7
					}
					position++
				}
			l13:
				{
					position15, tokenIndex15 := position, tokenIndex
					if buffer[position] != rune('p') {
						goto l16
					}
					position++
					goto l15
				l16:
					position, tokenIndex = position15, tokenIndex15
					if buffer[position] != rune('P') {
						goto l7
					}
					position++
				}
			l15:
				if !_rules[rule_]() {
					goto l7
				}
				if !_rules[ruleDropTable]() {
					goto l7
				}
				if !_rules[rule_]() {
					goto l7
				}
				if buffer[position] != rune(';') {
					goto l7
				}
				position++
				if !_rules[rule_]() {
					goto l7
				}
				add(ruleDropStatement, position8)
			}
			return true
		l7:
			position, tokenIndex = position7, tokenIndex7
			return false
		},
		/* 2 SelectStatement <- <(_ (('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T')) _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ SelectTable _ WHERE* _ LIMIT* _ ';' _)> */
		func() bool {
			position17, tokenIndex17 := position, tokenIndex
			{
				position18 := position
				if !_rules[rule_]() {
					goto l17
				}
				{
					position19, tokenIndex19 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l20
					}
					position++
					goto l19
				l20:
					position, tokenIndex = position19, tokenIndex19
					if buffer[position] != rune('S') {
						goto l17
					}
					position++
				}
			l19:
				{
					position21, tokenIndex21 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l22
					}
					position++
					goto l21
				l22:
					position, tokenIndex = position21, tokenIndex21
					if buffer[position] != rune('E') {
						goto l17
					}
					position++
				}
			l21:
				{
					position23, tokenIndex23 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l24
					}
					position++
					goto l23
				l24:
					position, tokenIndex = position23, tokenIndex23
					if buffer[position] != rune('L') {
						goto l17
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
						goto l17
					}
					position++
				}
			l25:
				{
					position27, tokenIndex27 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l28
					}
					position++
					goto l27
				l28:
					position, tokenIndex = position27, tokenIndex27
					if buffer[position] != rune('C') {
						goto l17
					}
					position++
				}
			l27:
				{
					position29, tokenIndex29 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l30
					}
					position++
					goto l29
				l30:
					position, tokenIndex = position29, tokenIndex29
					if buffer[position] != rune('T') {
						goto l17
					}
					position++
				}
			l29:
				if !_rules[rule_]() {
					goto l17
				}
				if !_rules[ruleColumns]() {
					goto l17
				}
				if !_rules[rule_]() {
					goto l17
				}
				{
					position31, tokenIndex31 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l32
					}
					position++
					goto l31
				l32:
					position, tokenIndex = position31, tokenIndex31
					if buffer[position] != rune('F') {
						goto l17
					}
					position++
				}
			l31:
				{
					position33, tokenIndex33 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l34
					}
					position++
					goto l33
				l34:
					position, tokenIndex = position33, tokenIndex33
					if buffer[position] != rune('R') {
						goto l17
					}
					position++
				}
			l33:
				{
					position35, tokenIndex35 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l36
					}
					position++
					goto l35
				l36:
					position, tokenIndex = position35, tokenIndex35
					if buffer[position] != rune('O') {
						goto l17
					}
					position++
				}
			l35:
				{
					position37, tokenIndex37 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l38
					}
					position++
					goto l37
				l38:
					position, tokenIndex = position37, tokenIndex37
					if buffer[position] != rune('M') {
						goto l17
					}
					position++
				}
			l37:
				if !_rules[rule_]() {
					goto l17
				}
				if !_rules[ruleSelectTable]() {
					goto l17
				}
				if !_rules[rule_]() {
					goto l17
				}
			l39:
				{
					position40, tokenIndex40 := position, tokenIndex
					if !_rules[ruleWHERE]() {
						goto l40
					}
					goto l39
				l40:
					position, tokenIndex = position40, tokenIndex40
				}
				if !_rules[rule_]() {
					goto l17
				}
			l41:
				{
					position42, tokenIndex42 := position, tokenIndex
					if !_rules[ruleLIMIT]() {
						goto l42
					}
					goto l41
				l42:
					position, tokenIndex = position42, tokenIndex42
				}
				if !_rules[rule_]() {
					goto l17
				}
				if buffer[position] != rune(';') {
					goto l17
				}
				position++
				if !_rules[rule_]() {
					goto l17
				}
				add(ruleSelectStatement, position18)
			}
			return true
		l17:
			position, tokenIndex = position17, tokenIndex17
			return false
		},
		/* 3 InsertStatement <- <(_ (('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T')) _ (('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O')) _ InsertBody Action0)> */
		func() bool {
			position43, tokenIndex43 := position, tokenIndex
			{
				position44 := position
				if !_rules[rule_]() {
					goto l43
				}
				{
					position45, tokenIndex45 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l46
					}
					position++
					goto l45
				l46:
					position, tokenIndex = position45, tokenIndex45
					if buffer[position] != rune('I') {
						goto l43
					}
					position++
				}
			l45:
				{
					position47, tokenIndex47 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l48
					}
					position++
					goto l47
				l48:
					position, tokenIndex = position47, tokenIndex47
					if buffer[position] != rune('N') {
						goto l43
					}
					position++
				}
			l47:
				{
					position49, tokenIndex49 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l50
					}
					position++
					goto l49
				l50:
					position, tokenIndex = position49, tokenIndex49
					if buffer[position] != rune('S') {
						goto l43
					}
					position++
				}
			l49:
				{
					position51, tokenIndex51 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l52
					}
					position++
					goto l51
				l52:
					position, tokenIndex = position51, tokenIndex51
					if buffer[position] != rune('E') {
						goto l43
					}
					position++
				}
			l51:
				{
					position53, tokenIndex53 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l54
					}
					position++
					goto l53
				l54:
					position, tokenIndex = position53, tokenIndex53
					if buffer[position] != rune('R') {
						goto l43
					}
					position++
				}
			l53:
				{
					position55, tokenIndex55 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l56
					}
					position++
					goto l55
				l56:
					position, tokenIndex = position55, tokenIndex55
					if buffer[position] != rune('T') {
						goto l43
					}
					position++
				}
			l55:
				if !_rules[rule_]() {
					goto l43
				}
				{
					position57, tokenIndex57 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l58
					}
					position++
					goto l57
				l58:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('I') {
						goto l43
					}
					position++
				}
			l57:
				{
					position59, tokenIndex59 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l60
					}
					position++
					goto l59
				l60:
					position, tokenIndex = position59, tokenIndex59
					if buffer[position] != rune('N') {
						goto l43
					}
					position++
				}
			l59:
				{
					position61, tokenIndex61 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l62
					}
					position++
					goto l61
				l62:
					position, tokenIndex = position61, tokenIndex61
					if buffer[position] != rune('T') {
						goto l43
					}
					position++
				}
			l61:
				{
					position63, tokenIndex63 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l64
					}
					position++
					goto l63
				l64:
					position, tokenIndex = position63, tokenIndex63
					if buffer[position] != rune('O') {
						goto l43
					}
					position++
				}
			l63:
				if !_rules[rule_]() {
					goto l43
				}
				if !_rules[ruleInsertBody]() {
					goto l43
				}
				if !_rules[ruleAction0]() {
					goto l43
				}
				add(ruleInsertStatement, position44)
			}
			return true
		l43:
			position, tokenIndex = position43, tokenIndex43
			return false
		},
		/* 4 CreateStatement <- <(_ (('c' / 'C') ('r' / 'R') ('e' / 'E') ('a' / 'A') ('t' / 'T') ('e' / 'E')) _ (('t' / 'T') ('a' / 'A') ('b' / 'B') ('l' / 'L') ('e' / 'E')) _ CreateTable _ CreateParaColList _ ';' Action1)> */
		func() bool {
			position65, tokenIndex65 := position, tokenIndex
			{
				position66 := position
				if !_rules[rule_]() {
					goto l65
				}
				{
					position67, tokenIndex67 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l68
					}
					position++
					goto l67
				l68:
					position, tokenIndex = position67, tokenIndex67
					if buffer[position] != rune('C') {
						goto l65
					}
					position++
				}
			l67:
				{
					position69, tokenIndex69 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l70
					}
					position++
					goto l69
				l70:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('R') {
						goto l65
					}
					position++
				}
			l69:
				{
					position71, tokenIndex71 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l72
					}
					position++
					goto l71
				l72:
					position, tokenIndex = position71, tokenIndex71
					if buffer[position] != rune('E') {
						goto l65
					}
					position++
				}
			l71:
				{
					position73, tokenIndex73 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l74
					}
					position++
					goto l73
				l74:
					position, tokenIndex = position73, tokenIndex73
					if buffer[position] != rune('A') {
						goto l65
					}
					position++
				}
			l73:
				{
					position75, tokenIndex75 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l76
					}
					position++
					goto l75
				l76:
					position, tokenIndex = position75, tokenIndex75
					if buffer[position] != rune('T') {
						goto l65
					}
					position++
				}
			l75:
				{
					position77, tokenIndex77 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l78
					}
					position++
					goto l77
				l78:
					position, tokenIndex = position77, tokenIndex77
					if buffer[position] != rune('E') {
						goto l65
					}
					position++
				}
			l77:
				if !_rules[rule_]() {
					goto l65
				}
				{
					position79, tokenIndex79 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l80
					}
					position++
					goto l79
				l80:
					position, tokenIndex = position79, tokenIndex79
					if buffer[position] != rune('T') {
						goto l65
					}
					position++
				}
			l79:
				{
					position81, tokenIndex81 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l82
					}
					position++
					goto l81
				l82:
					position, tokenIndex = position81, tokenIndex81
					if buffer[position] != rune('A') {
						goto l65
					}
					position++
				}
			l81:
				{
					position83, tokenIndex83 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l84
					}
					position++
					goto l83
				l84:
					position, tokenIndex = position83, tokenIndex83
					if buffer[position] != rune('B') {
						goto l65
					}
					position++
				}
			l83:
				{
					position85, tokenIndex85 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l86
					}
					position++
					goto l85
				l86:
					position, tokenIndex = position85, tokenIndex85
					if buffer[position] != rune('L') {
						goto l65
					}
					position++
				}
			l85:
				{
					position87, tokenIndex87 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l88
					}
					position++
					goto l87
				l88:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('E') {
						goto l65
					}
					position++
				}
			l87:
				if !_rules[rule_]() {
					goto l65
				}
				if !_rules[ruleCreateTable]() {
					goto l65
				}
				if !_rules[rule_]() {
					goto l65
				}
				if !_rules[ruleCreateParaColList]() {
					goto l65
				}
				if !_rules[rule_]() {
					goto l65
				}
				if buffer[position] != rune(';') {
					goto l65
				}
				position++
				if !_rules[ruleAction1]() {
					goto l65
				}
				add(ruleCreateStatement, position66)
			}
			return true
		l65:
			position, tokenIndex = position65, tokenIndex65
			return false
		},
		/* 5 Columns <- <(SelectParaColList / SelectColList / Asterisk)> */
		func() bool {
			position89, tokenIndex89 := position, tokenIndex
			{
				position90 := position
				{
					position91, tokenIndex91 := position, tokenIndex
					if !_rules[ruleSelectParaColList]() {
						goto l92
					}
					goto l91
				l92:
					position, tokenIndex = position91, tokenIndex91
					if !_rules[ruleSelectColList]() {
						goto l93
					}
					goto l91
				l93:
					position, tokenIndex = position91, tokenIndex91
					if !_rules[ruleAsterisk]() {
						goto l89
					}
				}
			l91:
				add(ruleColumns, position90)
			}
			return true
		l89:
			position, tokenIndex = position89, tokenIndex89
			return false
		},
		/* 6 SelectParaColList <- <('(' _ SelectColList _ ')')> */
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
				if !_rules[ruleSelectColList]() {
					goto l94
				}
				if !_rules[rule_]() {
					goto l94
				}
				if buffer[position] != rune(')') {
					goto l94
				}
				position++
				add(ruleSelectParaColList, position95)
			}
			return true
		l94:
			position, tokenIndex = position94, tokenIndex94
			return false
		},
		/* 7 SelectColList <- <((SelectColName _ ',' _)* SelectColName)> */
		func() bool {
			position96, tokenIndex96 := position, tokenIndex
			{
				position97 := position
			l98:
				{
					position99, tokenIndex99 := position, tokenIndex
					if !_rules[ruleSelectColName]() {
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
				if !_rules[ruleSelectColName]() {
					goto l96
				}
				add(ruleSelectColList, position97)
			}
			return true
		l96:
			position, tokenIndex = position96, tokenIndex96
			return false
		},
		/* 8 SelectColName <- <(<AlphaNum+> Action2)> */
		func() bool {
			position100, tokenIndex100 := position, tokenIndex
			{
				position101 := position
				{
					position102 := position
					if !_rules[ruleAlphaNum]() {
						goto l100
					}
				l103:
					{
						position104, tokenIndex104 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l104
						}
						goto l103
					l104:
						position, tokenIndex = position104, tokenIndex104
					}
					add(rulePegText, position102)
				}
				if !_rules[ruleAction2]() {
					goto l100
				}
				add(ruleSelectColName, position101)
			}
			return true
		l100:
			position, tokenIndex = position100, tokenIndex100
			return false
		},
		/* 9 InsertParaColList <- <('(' _ InsertColList _ ')')> */
		func() bool {
			position105, tokenIndex105 := position, tokenIndex
			{
				position106 := position
				if buffer[position] != rune('(') {
					goto l105
				}
				position++
				if !_rules[rule_]() {
					goto l105
				}
				if !_rules[ruleInsertColList]() {
					goto l105
				}
				if !_rules[rule_]() {
					goto l105
				}
				if buffer[position] != rune(')') {
					goto l105
				}
				position++
				add(ruleInsertParaColList, position106)
			}
			return true
		l105:
			position, tokenIndex = position105, tokenIndex105
			return false
		},
		/* 10 InsertColList <- <((_ InsertColName _ ',' _)* InsertColName)> */
		func() bool {
			position107, tokenIndex107 := position, tokenIndex
			{
				position108 := position
			l109:
				{
					position110, tokenIndex110 := position, tokenIndex
					if !_rules[rule_]() {
						goto l110
					}
					if !_rules[ruleInsertColName]() {
						goto l110
					}
					if !_rules[rule_]() {
						goto l110
					}
					if buffer[position] != rune(',') {
						goto l110
					}
					position++
					if !_rules[rule_]() {
						goto l110
					}
					goto l109
				l110:
					position, tokenIndex = position110, tokenIndex110
				}
				if !_rules[ruleInsertColName]() {
					goto l107
				}
				add(ruleInsertColList, position108)
			}
			return true
		l107:
			position, tokenIndex = position107, tokenIndex107
			return false
		},
		/* 11 InsertColName <- <(<AlphaNum+> Action3)> */
		func() bool {
			position111, tokenIndex111 := position, tokenIndex
			{
				position112 := position
				{
					position113 := position
					if !_rules[ruleAlphaNum]() {
						goto l111
					}
				l114:
					{
						position115, tokenIndex115 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l115
						}
						goto l114
					l115:
						position, tokenIndex = position115, tokenIndex115
					}
					add(rulePegText, position113)
				}
				if !_rules[ruleAction3]() {
					goto l111
				}
				add(ruleInsertColName, position112)
			}
			return true
		l111:
			position, tokenIndex = position111, tokenIndex111
			return false
		},
		/* 12 CreateParaColList <- <('(' _ CreateColList _ ')')> */
		func() bool {
			position116, tokenIndex116 := position, tokenIndex
			{
				position117 := position
				if buffer[position] != rune('(') {
					goto l116
				}
				position++
				if !_rules[rule_]() {
					goto l116
				}
				if !_rules[ruleCreateColList]() {
					goto l116
				}
				if !_rules[rule_]() {
					goto l116
				}
				if buffer[position] != rune(')') {
					goto l116
				}
				position++
				add(ruleCreateParaColList, position117)
			}
			return true
		l116:
			position, tokenIndex = position116, tokenIndex116
			return false
		},
		/* 13 CreateColList <- <((_ CreateColName _ ',' _)* CreateColName)> */
		func() bool {
			position118, tokenIndex118 := position, tokenIndex
			{
				position119 := position
			l120:
				{
					position121, tokenIndex121 := position, tokenIndex
					if !_rules[rule_]() {
						goto l121
					}
					if !_rules[ruleCreateColName]() {
						goto l121
					}
					if !_rules[rule_]() {
						goto l121
					}
					if buffer[position] != rune(',') {
						goto l121
					}
					position++
					if !_rules[rule_]() {
						goto l121
					}
					goto l120
				l121:
					position, tokenIndex = position121, tokenIndex121
				}
				if !_rules[ruleCreateColName]() {
					goto l118
				}
				add(ruleCreateColList, position119)
			}
			return true
		l118:
			position, tokenIndex = position118, tokenIndex118
			return false
		},
		/* 14 CreateColName <- <(<AlphaNum+> Action4)> */
		func() bool {
			position122, tokenIndex122 := position, tokenIndex
			{
				position123 := position
				{
					position124 := position
					if !_rules[ruleAlphaNum]() {
						goto l122
					}
				l125:
					{
						position126, tokenIndex126 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l126
						}
						goto l125
					l126:
						position, tokenIndex = position126, tokenIndex126
					}
					add(rulePegText, position124)
				}
				if !_rules[ruleAction4]() {
					goto l122
				}
				add(ruleCreateColName, position123)
			}
			return true
		l122:
			position, tokenIndex = position122, tokenIndex122
			return false
		},
		/* 15 InsertBody <- <(InsertTable _ Values _ ';')> */
		func() bool {
			position127, tokenIndex127 := position, tokenIndex
			{
				position128 := position
				if !_rules[ruleInsertTable]() {
					goto l127
				}
				if !_rules[rule_]() {
					goto l127
				}
				if !_rules[ruleValues]() {
					goto l127
				}
				if !_rules[rule_]() {
					goto l127
				}
				if buffer[position] != rune(';') {
					goto l127
				}
				position++
				add(ruleInsertBody, position128)
			}
			return true
		l127:
			position, tokenIndex = position127, tokenIndex127
			return false
		},
		/* 16 Values <- <(InsertParaColList? _ (('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S')) _ ValuesBody)> */
		func() bool {
			position129, tokenIndex129 := position, tokenIndex
			{
				position130 := position
				{
					position131, tokenIndex131 := position, tokenIndex
					if !_rules[ruleInsertParaColList]() {
						goto l131
					}
					goto l132
				l131:
					position, tokenIndex = position131, tokenIndex131
				}
			l132:
				if !_rules[rule_]() {
					goto l129
				}
				{
					position133, tokenIndex133 := position, tokenIndex
					if buffer[position] != rune('v') {
						goto l134
					}
					position++
					goto l133
				l134:
					position, tokenIndex = position133, tokenIndex133
					if buffer[position] != rune('V') {
						goto l129
					}
					position++
				}
			l133:
				{
					position135, tokenIndex135 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l136
					}
					position++
					goto l135
				l136:
					position, tokenIndex = position135, tokenIndex135
					if buffer[position] != rune('A') {
						goto l129
					}
					position++
				}
			l135:
				{
					position137, tokenIndex137 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l138
					}
					position++
					goto l137
				l138:
					position, tokenIndex = position137, tokenIndex137
					if buffer[position] != rune('L') {
						goto l129
					}
					position++
				}
			l137:
				{
					position139, tokenIndex139 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l140
					}
					position++
					goto l139
				l140:
					position, tokenIndex = position139, tokenIndex139
					if buffer[position] != rune('U') {
						goto l129
					}
					position++
				}
			l139:
				{
					position141, tokenIndex141 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l142
					}
					position++
					goto l141
				l142:
					position, tokenIndex = position141, tokenIndex141
					if buffer[position] != rune('E') {
						goto l129
					}
					position++
				}
			l141:
				{
					position143, tokenIndex143 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l144
					}
					position++
					goto l143
				l144:
					position, tokenIndex = position143, tokenIndex143
					if buffer[position] != rune('S') {
						goto l129
					}
					position++
				}
			l143:
				if !_rules[rule_]() {
					goto l129
				}
				if !_rules[ruleValuesBody]() {
					goto l129
				}
				add(ruleValues, position130)
			}
			return true
		l129:
			position, tokenIndex = position129, tokenIndex129
			return false
		},
		/* 17 ValuesBody <- <('(' _ ValList _ ')')> */
		func() bool {
			position145, tokenIndex145 := position, tokenIndex
			{
				position146 := position
				if buffer[position] != rune('(') {
					goto l145
				}
				position++
				if !_rules[rule_]() {
					goto l145
				}
				if !_rules[ruleValList]() {
					goto l145
				}
				if !_rules[rule_]() {
					goto l145
				}
				if buffer[position] != rune(')') {
					goto l145
				}
				position++
				add(ruleValuesBody, position146)
			}
			return true
		l145:
			position, tokenIndex = position145, tokenIndex145
			return false
		},
		/* 18 ValList <- <((_ InsertValue _ ',' _)* _ InsertValue)> */
		func() bool {
			position147, tokenIndex147 := position, tokenIndex
			{
				position148 := position
			l149:
				{
					position150, tokenIndex150 := position, tokenIndex
					if !_rules[rule_]() {
						goto l150
					}
					if !_rules[ruleInsertValue]() {
						goto l150
					}
					if !_rules[rule_]() {
						goto l150
					}
					if buffer[position] != rune(',') {
						goto l150
					}
					position++
					if !_rules[rule_]() {
						goto l150
					}
					goto l149
				l150:
					position, tokenIndex = position150, tokenIndex150
				}
				if !_rules[rule_]() {
					goto l147
				}
				if !_rules[ruleInsertValue]() {
					goto l147
				}
				add(ruleValList, position148)
			}
			return true
		l147:
			position, tokenIndex = position147, tokenIndex147
			return false
		},
		/* 19 InsertValue <- <('"'? EachValue '"'?)> */
		func() bool {
			position151, tokenIndex151 := position, tokenIndex
			{
				position152 := position
				{
					position153, tokenIndex153 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l153
					}
					position++
					goto l154
				l153:
					position, tokenIndex = position153, tokenIndex153
				}
			l154:
				if !_rules[ruleEachValue]() {
					goto l151
				}
				{
					position155, tokenIndex155 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l155
					}
					position++
					goto l156
				l155:
					position, tokenIndex = position155, tokenIndex155
				}
			l156:
				add(ruleInsertValue, position152)
			}
			return true
		l151:
			position, tokenIndex = position151, tokenIndex151
			return false
		},
		/* 20 EachValue <- <(<AlphaNum+> Action5)> */
		func() bool {
			position157, tokenIndex157 := position, tokenIndex
			{
				position158 := position
				{
					position159 := position
					if !_rules[ruleAlphaNum]() {
						goto l157
					}
				l160:
					{
						position161, tokenIndex161 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l161
						}
						goto l160
					l161:
						position, tokenIndex = position161, tokenIndex161
					}
					add(rulePegText, position159)
				}
				if !_rules[ruleAction5]() {
					goto l157
				}
				add(ruleEachValue, position158)
			}
			return true
		l157:
			position, tokenIndex = position157, tokenIndex157
			return false
		},
		/* 21 Asterisk <- <('*' Action6)> */
		func() bool {
			position162, tokenIndex162 := position, tokenIndex
			{
				position163 := position
				if buffer[position] != rune('*') {
					goto l162
				}
				position++
				if !_rules[ruleAction6]() {
					goto l162
				}
				add(ruleAsterisk, position163)
			}
			return true
		l162:
			position, tokenIndex = position162, tokenIndex162
			return false
		},
		/* 22 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position164, tokenIndex164 := position, tokenIndex
			{
				position165 := position
				if buffer[position] != rune('/') {
					goto l164
				}
				position++
				if buffer[position] != rune('/') {
					goto l164
				}
				position++
			l166:
				{
					position167, tokenIndex167 := position, tokenIndex
					{
						position168, tokenIndex168 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l168
						}
						position++
						goto l167
					l168:
						position, tokenIndex = position168, tokenIndex168
					}
					if !matchDot() {
						goto l167
					}
					goto l166
				l167:
					position, tokenIndex = position167, tokenIndex167
				}
				add(rulelineComment, position165)
			}
			return true
		l164:
			position, tokenIndex = position164, tokenIndex164
			return false
		},
		/* 23 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position169, tokenIndex169 := position, tokenIndex
			{
				position170 := position
				if buffer[position] != rune('/') {
					goto l169
				}
				position++
				if buffer[position] != rune('*') {
					goto l169
				}
				position++
			l171:
				{
					position172, tokenIndex172 := position, tokenIndex
					{
						position173, tokenIndex173 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l173
						}
						position++
						if buffer[position] != rune('/') {
							goto l173
						}
						position++
						goto l172
					l173:
						position, tokenIndex = position173, tokenIndex173
					}
					{
						position174, tokenIndex174 := position, tokenIndex
						if !matchDot() {
							goto l175
						}
						goto l174
					l175:
						position, tokenIndex = position174, tokenIndex174
						if buffer[position] != rune('\n') {
							goto l172
						}
						position++
					}
				l174:
					goto l171
				l172:
					position, tokenIndex = position172, tokenIndex172
				}
				if buffer[position] != rune('*') {
					goto l169
				}
				position++
				if buffer[position] != rune('/') {
					goto l169
				}
				position++
				add(ruleblockComment, position170)
			}
			return true
		l169:
			position, tokenIndex = position169, tokenIndex169
			return false
		},
		/* 24 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position176, tokenIndex176 := position, tokenIndex
			{
				position177 := position
				{
					position178, tokenIndex178 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l179
					}
					position++
					goto l178
				l179:
					position, tokenIndex = position178, tokenIndex178
					if buffer[position] != rune('\t') {
						goto l180
					}
					position++
					goto l178
				l180:
					position, tokenIndex = position178, tokenIndex178
					if buffer[position] != rune('\n') {
						goto l181
					}
					position++
					goto l178
				l181:
					position, tokenIndex = position178, tokenIndex178
					if buffer[position] != rune('\r') {
						goto l176
					}
					position++
				}
			l178:
				add(rulews, position177)
			}
			return true
		l176:
			position, tokenIndex = position176, tokenIndex176
			return false
		},
		/* 25 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position183 := position
			l184:
				{
					position185, tokenIndex185 := position, tokenIndex
					{
						position186, tokenIndex186 := position, tokenIndex
						if !_rules[rulews]() {
							goto l187
						}
						goto l186
					l187:
						position, tokenIndex = position186, tokenIndex186
						if !_rules[rulelineComment]() {
							goto l188
						}
						goto l186
					l188:
						position, tokenIndex = position186, tokenIndex186
						if !_rules[ruleblockComment]() {
							goto l185
						}
					}
				l186:
					goto l184
				l185:
					position, tokenIndex = position185, tokenIndex185
				}
				add(rule_, position183)
			}
			return true
		},
		/* 26 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position189, tokenIndex189 := position, tokenIndex
			{
				position190 := position
				{
					position191, tokenIndex191 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l192
					}
					position++
					goto l191
				l192:
					position, tokenIndex = position191, tokenIndex191
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l193
					}
					position++
					goto l191
				l193:
					position, tokenIndex = position191, tokenIndex191
					if buffer[position] != rune('_') {
						goto l189
					}
					position++
				}
			l191:
				add(ruleLetter, position190)
			}
			return true
		l189:
			position, tokenIndex = position189, tokenIndex189
			return false
		},
		/* 27 Number <- <([0-9] ('.' [0-9])*)> */
		func() bool {
			position194, tokenIndex194 := position, tokenIndex
			{
				position195 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l194
				}
				position++
			l196:
				{
					position197, tokenIndex197 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l197
					}
					position++
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l197
					}
					position++
					goto l196
				l197:
					position, tokenIndex = position197, tokenIndex197
				}
				add(ruleNumber, position195)
			}
			return true
		l194:
			position, tokenIndex = position194, tokenIndex194
			return false
		},
		/* 28 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position198, tokenIndex198 := position, tokenIndex
			{
				position199 := position
				{
					position200, tokenIndex200 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l201
					}
					goto l200
				l201:
					position, tokenIndex = position200, tokenIndex200
					if !_rules[ruleNumber]() {
						goto l198
					}
				}
			l200:
				add(ruleAlphaNum, position199)
			}
			return true
		l198:
			position, tokenIndex = position198, tokenIndex198
			return false
		},
		/* 29 SelectTable <- <(SelectKeyspace '.' SelectTableName)> */
		func() bool {
			position202, tokenIndex202 := position, tokenIndex
			{
				position203 := position
				if !_rules[ruleSelectKeyspace]() {
					goto l202
				}
				if buffer[position] != rune('.') {
					goto l202
				}
				position++
				if !_rules[ruleSelectTableName]() {
					goto l202
				}
				add(ruleSelectTable, position203)
			}
			return true
		l202:
			position, tokenIndex = position202, tokenIndex202
			return false
		},
		/* 30 SelectKeyspace <- <(<AlphaNum+> Action7)> */
		func() bool {
			position204, tokenIndex204 := position, tokenIndex
			{
				position205 := position
				{
					position206 := position
					if !_rules[ruleAlphaNum]() {
						goto l204
					}
				l207:
					{
						position208, tokenIndex208 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l208
						}
						goto l207
					l208:
						position, tokenIndex = position208, tokenIndex208
					}
					add(rulePegText, position206)
				}
				if !_rules[ruleAction7]() {
					goto l204
				}
				add(ruleSelectKeyspace, position205)
			}
			return true
		l204:
			position, tokenIndex = position204, tokenIndex204
			return false
		},
		/* 31 SelectTableName <- <(<AlphaNum+> Action8)> */
		func() bool {
			position209, tokenIndex209 := position, tokenIndex
			{
				position210 := position
				{
					position211 := position
					if !_rules[ruleAlphaNum]() {
						goto l209
					}
				l212:
					{
						position213, tokenIndex213 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l213
						}
						goto l212
					l213:
						position, tokenIndex = position213, tokenIndex213
					}
					add(rulePegText, position211)
				}
				if !_rules[ruleAction8]() {
					goto l209
				}
				add(ruleSelectTableName, position210)
			}
			return true
		l209:
			position, tokenIndex = position209, tokenIndex209
			return false
		},
		/* 32 InsertTable <- <(InsertKeyspace '.' InsertTableName)> */
		func() bool {
			position214, tokenIndex214 := position, tokenIndex
			{
				position215 := position
				if !_rules[ruleInsertKeyspace]() {
					goto l214
				}
				if buffer[position] != rune('.') {
					goto l214
				}
				position++
				if !_rules[ruleInsertTableName]() {
					goto l214
				}
				add(ruleInsertTable, position215)
			}
			return true
		l214:
			position, tokenIndex = position214, tokenIndex214
			return false
		},
		/* 33 InsertKeyspace <- <(<AlphaNum+> Action9)> */
		func() bool {
			position216, tokenIndex216 := position, tokenIndex
			{
				position217 := position
				{
					position218 := position
					if !_rules[ruleAlphaNum]() {
						goto l216
					}
				l219:
					{
						position220, tokenIndex220 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l220
						}
						goto l219
					l220:
						position, tokenIndex = position220, tokenIndex220
					}
					add(rulePegText, position218)
				}
				if !_rules[ruleAction9]() {
					goto l216
				}
				add(ruleInsertKeyspace, position217)
			}
			return true
		l216:
			position, tokenIndex = position216, tokenIndex216
			return false
		},
		/* 34 InsertTableName <- <(<AlphaNum+> Action10)> */
		func() bool {
			position221, tokenIndex221 := position, tokenIndex
			{
				position222 := position
				{
					position223 := position
					if !_rules[ruleAlphaNum]() {
						goto l221
					}
				l224:
					{
						position225, tokenIndex225 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l225
						}
						goto l224
					l225:
						position, tokenIndex = position225, tokenIndex225
					}
					add(rulePegText, position223)
				}
				if !_rules[ruleAction10]() {
					goto l221
				}
				add(ruleInsertTableName, position222)
			}
			return true
		l221:
			position, tokenIndex = position221, tokenIndex221
			return false
		},
		/* 35 CreateTable <- <(CreateKeyspace '.' CreateTableName)> */
		func() bool {
			position226, tokenIndex226 := position, tokenIndex
			{
				position227 := position
				if !_rules[ruleCreateKeyspace]() {
					goto l226
				}
				if buffer[position] != rune('.') {
					goto l226
				}
				position++
				if !_rules[ruleCreateTableName]() {
					goto l226
				}
				add(ruleCreateTable, position227)
			}
			return true
		l226:
			position, tokenIndex = position226, tokenIndex226
			return false
		},
		/* 36 CreateKeyspace <- <(<AlphaNum+> Action11)> */
		func() bool {
			position228, tokenIndex228 := position, tokenIndex
			{
				position229 := position
				{
					position230 := position
					if !_rules[ruleAlphaNum]() {
						goto l228
					}
				l231:
					{
						position232, tokenIndex232 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l232
						}
						goto l231
					l232:
						position, tokenIndex = position232, tokenIndex232
					}
					add(rulePegText, position230)
				}
				if !_rules[ruleAction11]() {
					goto l228
				}
				add(ruleCreateKeyspace, position229)
			}
			return true
		l228:
			position, tokenIndex = position228, tokenIndex228
			return false
		},
		/* 37 CreateTableName <- <(<AlphaNum+> Action12)> */
		func() bool {
			position233, tokenIndex233 := position, tokenIndex
			{
				position234 := position
				{
					position235 := position
					if !_rules[ruleAlphaNum]() {
						goto l233
					}
				l236:
					{
						position237, tokenIndex237 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l237
						}
						goto l236
					l237:
						position, tokenIndex = position237, tokenIndex237
					}
					add(rulePegText, position235)
				}
				if !_rules[ruleAction12]() {
					goto l233
				}
				add(ruleCreateTableName, position234)
			}
			return true
		l233:
			position, tokenIndex = position233, tokenIndex233
			return false
		},
		/* 38 DropTable <- <(DropTableKeyspace '.' DropTableName)> */
		func() bool {
			position238, tokenIndex238 := position, tokenIndex
			{
				position239 := position
				if !_rules[ruleDropTableKeyspace]() {
					goto l238
				}
				if buffer[position] != rune('.') {
					goto l238
				}
				position++
				if !_rules[ruleDropTableName]() {
					goto l238
				}
				add(ruleDropTable, position239)
			}
			return true
		l238:
			position, tokenIndex = position238, tokenIndex238
			return false
		},
		/* 39 DropTableKeyspace <- <(<AlphaNum+> Action13)> */
		func() bool {
			position240, tokenIndex240 := position, tokenIndex
			{
				position241 := position
				{
					position242 := position
					if !_rules[ruleAlphaNum]() {
						goto l240
					}
				l243:
					{
						position244, tokenIndex244 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l244
						}
						goto l243
					l244:
						position, tokenIndex = position244, tokenIndex244
					}
					add(rulePegText, position242)
				}
				if !_rules[ruleAction13]() {
					goto l240
				}
				add(ruleDropTableKeyspace, position241)
			}
			return true
		l240:
			position, tokenIndex = position240, tokenIndex240
			return false
		},
		/* 40 DropTableName <- <(<AlphaNum+> Action14)> */
		func() bool {
			position245, tokenIndex245 := position, tokenIndex
			{
				position246 := position
				{
					position247 := position
					if !_rules[ruleAlphaNum]() {
						goto l245
					}
				l248:
					{
						position249, tokenIndex249 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l249
						}
						goto l248
					l249:
						position, tokenIndex = position249, tokenIndex249
					}
					add(rulePegText, position247)
				}
				if !_rules[ruleAction14]() {
					goto l245
				}
				add(ruleDropTableName, position246)
			}
			return true
		l245:
			position, tokenIndex = position245, tokenIndex245
			return false
		},
		/* 41 WHERE <- <(('w' / 'W') ('h' / 'H') ('e' / 'E') ('r' / 'R') ('e' / 'E') _ FILTERS+ Action15)> */
		func() bool {
			position250, tokenIndex250 := position, tokenIndex
			{
				position251 := position
				{
					position252, tokenIndex252 := position, tokenIndex
					if buffer[position] != rune('w') {
						goto l253
					}
					position++
					goto l252
				l253:
					position, tokenIndex = position252, tokenIndex252
					if buffer[position] != rune('W') {
						goto l250
					}
					position++
				}
			l252:
				{
					position254, tokenIndex254 := position, tokenIndex
					if buffer[position] != rune('h') {
						goto l255
					}
					position++
					goto l254
				l255:
					position, tokenIndex = position254, tokenIndex254
					if buffer[position] != rune('H') {
						goto l250
					}
					position++
				}
			l254:
				{
					position256, tokenIndex256 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l257
					}
					position++
					goto l256
				l257:
					position, tokenIndex = position256, tokenIndex256
					if buffer[position] != rune('E') {
						goto l250
					}
					position++
				}
			l256:
				{
					position258, tokenIndex258 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l259
					}
					position++
					goto l258
				l259:
					position, tokenIndex = position258, tokenIndex258
					if buffer[position] != rune('R') {
						goto l250
					}
					position++
				}
			l258:
				{
					position260, tokenIndex260 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l261
					}
					position++
					goto l260
				l261:
					position, tokenIndex = position260, tokenIndex260
					if buffer[position] != rune('E') {
						goto l250
					}
					position++
				}
			l260:
				if !_rules[rule_]() {
					goto l250
				}
				if !_rules[ruleFILTERS]() {
					goto l250
				}
			l262:
				{
					position263, tokenIndex263 := position, tokenIndex
					if !_rules[ruleFILTERS]() {
						goto l263
					}
					goto l262
				l263:
					position, tokenIndex = position263, tokenIndex263
				}
				if !_rules[ruleAction15]() {
					goto l250
				}
				add(ruleWHERE, position251)
			}
			return true
		l250:
			position, tokenIndex = position250, tokenIndex250
			return false
		},
		/* 42 FILTERS <- <((FILTER _ (('a' / 'A') ('n' / 'N') ('d' / 'D')))* FILTER)> */
		func() bool {
			position264, tokenIndex264 := position, tokenIndex
			{
				position265 := position
			l266:
				{
					position267, tokenIndex267 := position, tokenIndex
					if !_rules[ruleFILTER]() {
						goto l267
					}
					if !_rules[rule_]() {
						goto l267
					}
					{
						position268, tokenIndex268 := position, tokenIndex
						if buffer[position] != rune('a') {
							goto l269
						}
						position++
						goto l268
					l269:
						position, tokenIndex = position268, tokenIndex268
						if buffer[position] != rune('A') {
							goto l267
						}
						position++
					}
				l268:
					{
						position270, tokenIndex270 := position, tokenIndex
						if buffer[position] != rune('n') {
							goto l271
						}
						position++
						goto l270
					l271:
						position, tokenIndex = position270, tokenIndex270
						if buffer[position] != rune('N') {
							goto l267
						}
						position++
					}
				l270:
					{
						position272, tokenIndex272 := position, tokenIndex
						if buffer[position] != rune('d') {
							goto l273
						}
						position++
						goto l272
					l273:
						position, tokenIndex = position272, tokenIndex272
						if buffer[position] != rune('D') {
							goto l267
						}
						position++
					}
				l272:
					goto l266
				l267:
					position, tokenIndex = position267, tokenIndex267
				}
				if !_rules[ruleFILTER]() {
					goto l264
				}
				add(ruleFILTERS, position265)
			}
			return true
		l264:
			position, tokenIndex = position264, tokenIndex264
			return false
		},
		/* 43 FILTER <- <(_ FilterColumn _ Operation _ FilterValue _)> */
		func() bool {
			position274, tokenIndex274 := position, tokenIndex
			{
				position275 := position
				if !_rules[rule_]() {
					goto l274
				}
				if !_rules[ruleFilterColumn]() {
					goto l274
				}
				if !_rules[rule_]() {
					goto l274
				}
				if !_rules[ruleOperation]() {
					goto l274
				}
				if !_rules[rule_]() {
					goto l274
				}
				if !_rules[ruleFilterValue]() {
					goto l274
				}
				if !_rules[rule_]() {
					goto l274
				}
				add(ruleFILTER, position275)
			}
			return true
		l274:
			position, tokenIndex = position274, tokenIndex274
			return false
		},
		/* 44 Operation <- <(<('=' / '>' / '<')> Action16)> */
		func() bool {
			position276, tokenIndex276 := position, tokenIndex
			{
				position277 := position
				{
					position278 := position
					{
						position279, tokenIndex279 := position, tokenIndex
						if buffer[position] != rune('=') {
							goto l280
						}
						position++
						goto l279
					l280:
						position, tokenIndex = position279, tokenIndex279
						if buffer[position] != rune('>') {
							goto l281
						}
						position++
						goto l279
					l281:
						position, tokenIndex = position279, tokenIndex279
						if buffer[position] != rune('<') {
							goto l276
						}
						position++
					}
				l279:
					add(rulePegText, position278)
				}
				if !_rules[ruleAction16]() {
					goto l276
				}
				add(ruleOperation, position277)
			}
			return true
		l276:
			position, tokenIndex = position276, tokenIndex276
			return false
		},
		/* 45 FilterColumn <- <(<AlphaNum+> Action17)> */
		func() bool {
			position282, tokenIndex282 := position, tokenIndex
			{
				position283 := position
				{
					position284 := position
					if !_rules[ruleAlphaNum]() {
						goto l282
					}
				l285:
					{
						position286, tokenIndex286 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l286
						}
						goto l285
					l286:
						position, tokenIndex = position286, tokenIndex286
					}
					add(rulePegText, position284)
				}
				if !_rules[ruleAction17]() {
					goto l282
				}
				add(ruleFilterColumn, position283)
			}
			return true
		l282:
			position, tokenIndex = position282, tokenIndex282
			return false
		},
		/* 46 FilterValue <- <(<AlphaNum+> Action18)> */
		func() bool {
			position287, tokenIndex287 := position, tokenIndex
			{
				position288 := position
				{
					position289 := position
					if !_rules[ruleAlphaNum]() {
						goto l287
					}
				l290:
					{
						position291, tokenIndex291 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l291
						}
						goto l290
					l291:
						position, tokenIndex = position291, tokenIndex291
					}
					add(rulePegText, position289)
				}
				if !_rules[ruleAction18]() {
					goto l287
				}
				add(ruleFilterValue, position288)
			}
			return true
		l287:
			position, tokenIndex = position287, tokenIndex287
			return false
		},
		/* 47 LIMIT <- <(('l' / 'L') ('i' / 'I') ('m' / 'M') ('i' / 'I') ('t' / 'T') _ LimitVal)> */
		func() bool {
			position292, tokenIndex292 := position, tokenIndex
			{
				position293 := position
				{
					position294, tokenIndex294 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l295
					}
					position++
					goto l294
				l295:
					position, tokenIndex = position294, tokenIndex294
					if buffer[position] != rune('L') {
						goto l292
					}
					position++
				}
			l294:
				{
					position296, tokenIndex296 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l297
					}
					position++
					goto l296
				l297:
					position, tokenIndex = position296, tokenIndex296
					if buffer[position] != rune('I') {
						goto l292
					}
					position++
				}
			l296:
				{
					position298, tokenIndex298 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l299
					}
					position++
					goto l298
				l299:
					position, tokenIndex = position298, tokenIndex298
					if buffer[position] != rune('M') {
						goto l292
					}
					position++
				}
			l298:
				{
					position300, tokenIndex300 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l301
					}
					position++
					goto l300
				l301:
					position, tokenIndex = position300, tokenIndex300
					if buffer[position] != rune('I') {
						goto l292
					}
					position++
				}
			l300:
				{
					position302, tokenIndex302 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l303
					}
					position++
					goto l302
				l303:
					position, tokenIndex = position302, tokenIndex302
					if buffer[position] != rune('T') {
						goto l292
					}
					position++
				}
			l302:
				if !_rules[rule_]() {
					goto l292
				}
				if !_rules[ruleLimitVal]() {
					goto l292
				}
				add(ruleLIMIT, position293)
			}
			return true
		l292:
			position, tokenIndex = position292, tokenIndex292
			return false
		},
		/* 48 LimitVal <- <(<Number+> Action19)> */
		func() bool {
			position304, tokenIndex304 := position, tokenIndex
			{
				position305 := position
				{
					position306 := position
					if !_rules[ruleNumber]() {
						goto l304
					}
				l307:
					{
						position308, tokenIndex308 := position, tokenIndex
						if !_rules[ruleNumber]() {
							goto l308
						}
						goto l307
					l308:
						position, tokenIndex = position308, tokenIndex308
					}
					add(rulePegText, position306)
				}
				if !_rules[ruleAction19]() {
					goto l304
				}
				add(ruleLimitVal, position305)
			}
			return true
		l304:
			position, tokenIndex = position304, tokenIndex304
			return false
		},
		/* 50 Action0 <- <{ p.validateInsert() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 51 Action1 <- <{ p.setPartitionKey(buffer[begin:end])}> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		nil,
		/* 53 Action2 <- <{ p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 54 Action3 <- <{ p.InsertStatement.Columns = append(p.InsertStatement.Columns,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 55 Action4 <- <{ p.CreateStatement.Columns = append(p.CreateStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 56 Action5 <- <{ p.captureValues(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 57 Action6 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 58 Action7 <- <{p.SelectStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 59 Action8 <- <{ p.SelectStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 60 Action9 <- <{ p.InsertStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 61 Action10 <- <{ p.InsertStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 62 Action11 <- <{ p.CreateStatement.Keyspace =buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 63 Action12 <- <{ p.CreateStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 64 Action13 <- <{ p.DropStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 65 Action14 <- <{p.DropStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 66 Action15 <- <{  p.finalizeSelect();  }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 67 Action16 <- <{ p.SelectStatement.Operators= append(p.SelectStatement.Operators, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 68 Action17 <- <{ p.SelectStatement.WhereColumns = append(p.SelectStatement.WhereColumns, buffer[begin:end])  }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 69 Action18 <- <{ p.SelectStatement.WhereValues = append(p.SelectStatement.WhereValues, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 70 Action19 <- <{ var err error;  p.SelectStatement.Limit,err = strconv.Atoi(buffer[begin:end]); if err!=nil{ p.SelectStatement.Limit = -1  };   }> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
	}
	p.rules = _rules
}
