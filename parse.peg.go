package simpleSqlParser

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
	ruleDeleteStatement
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
	ruleDeleteTable
	ruleDeleteKeyspace
	ruleDeleteTableName
	ruleWHERESELECT
	ruleSELECTFILTERS
	ruleSELECTFILTER
	ruleSelectOperation
	ruleSelectEquality
	ruleSelectLessThan
	ruleSelectGreaterThan
	ruleSelectFilterColumn
	ruleSelectFilterValue
	ruleWHEREDELETE
	ruleDELETEFILTER
	ruleDeleteOperation
	ruleDeleteEquality
	ruleDeleteLessThan
	ruleDeleteGreaterThan
	ruleDeleteFilterColumn
	ruleDeleteFilterValue
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
	ruleAction20
	ruleAction21
	ruleAction22
	ruleAction23
	ruleAction24
	ruleAction25
	ruleAction26
	ruleAction27
)

var rul3s = [...]string{
	"Unknown",
	"G",
	"DropStatement",
	"SelectStatement",
	"DeleteStatement",
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
	"DeleteTable",
	"DeleteKeyspace",
	"DeleteTableName",
	"WHERESELECT",
	"SELECTFILTERS",
	"SELECTFILTER",
	"SelectOperation",
	"SelectEquality",
	"SelectLessThan",
	"SelectGreaterThan",
	"SelectFilterColumn",
	"SelectFilterValue",
	"WHEREDELETE",
	"DELETEFILTER",
	"DeleteOperation",
	"DeleteEquality",
	"DeleteLessThan",
	"DeleteGreaterThan",
	"DeleteFilterColumn",
	"DeleteFilterValue",
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
	"Action20",
	"Action21",
	"Action22",
	"Action23",
	"Action24",
	"Action25",
	"Action26",
	"Action27",
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
	DeleteStatement

	Buffer string
	buffer []rune
	rules  [94]func() bool
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
			p.DeleteStatement.Keyspace = buffer[begin:end]
		case ruleAction16:
			p.DeleteStatement.TableName = buffer[begin:end]
		case ruleAction17:
			p.SelectStatement.Operators = append(p.SelectStatement.Operators, buffer[begin:end])
		case ruleAction18:
			p.SelectStatement.Operators = append(p.SelectStatement.Operators, buffer[begin:end])
		case ruleAction19:
			p.SelectStatement.Operators = append(p.SelectStatement.Operators, buffer[begin:end])
		case ruleAction20:
			p.SelectStatement.WhereColumns = append(p.SelectStatement.WhereColumns, buffer[begin:end])
		case ruleAction21:
			p.SelectStatement.WhereValues = append(p.SelectStatement.WhereValues, buffer[begin:end])
		case ruleAction22:
			p.DeleteStatement.Operator = buffer[begin:end]
		case ruleAction23:
			p.DeleteStatement.Operator = buffer[begin:end]
		case ruleAction24:
			p.DeleteStatement.Operator = buffer[begin:end]
		case ruleAction25:
			p.DeleteStatement.WhereColumn = buffer[begin:end]
		case ruleAction26:
			p.DeleteStatement.WhereValue = buffer[begin:end]
		case ruleAction27:
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
		/* 0 G <- <(SelectStatement / InsertStatement / CreateStatement / DropStatement / (DeleteStatement !.))> */
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
						goto l6
					}
					goto l2
				l6:
					position, tokenIndex = position2, tokenIndex2
					if !_rules[ruleDeleteStatement]() {
						goto l0
					}
					{
						position7, tokenIndex7 := position, tokenIndex
						if !matchDot() {
							goto l7
						}
						goto l0
					l7:
						position, tokenIndex = position7, tokenIndex7
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
			position8, tokenIndex8 := position, tokenIndex
			{
				position9 := position
				if !_rules[rule_]() {
					goto l8
				}
				{
					position10, tokenIndex10 := position, tokenIndex
					if buffer[position] != rune('d') {
						goto l11
					}
					position++
					goto l10
				l11:
					position, tokenIndex = position10, tokenIndex10
					if buffer[position] != rune('D') {
						goto l8
					}
					position++
				}
			l10:
				{
					position12, tokenIndex12 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l13
					}
					position++
					goto l12
				l13:
					position, tokenIndex = position12, tokenIndex12
					if buffer[position] != rune('R') {
						goto l8
					}
					position++
				}
			l12:
				{
					position14, tokenIndex14 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l15
					}
					position++
					goto l14
				l15:
					position, tokenIndex = position14, tokenIndex14
					if buffer[position] != rune('O') {
						goto l8
					}
					position++
				}
			l14:
				{
					position16, tokenIndex16 := position, tokenIndex
					if buffer[position] != rune('p') {
						goto l17
					}
					position++
					goto l16
				l17:
					position, tokenIndex = position16, tokenIndex16
					if buffer[position] != rune('P') {
						goto l8
					}
					position++
				}
			l16:
				if !_rules[rule_]() {
					goto l8
				}
				if !_rules[ruleDropTable]() {
					goto l8
				}
				if !_rules[rule_]() {
					goto l8
				}
				if buffer[position] != rune(';') {
					goto l8
				}
				position++
				if !_rules[rule_]() {
					goto l8
				}
				add(ruleDropStatement, position9)
			}
			return true
		l8:
			position, tokenIndex = position8, tokenIndex8
			return false
		},
		/* 2 SelectStatement <- <(_ (('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T')) _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ SelectTable _ WHERESELECT* _ LIMIT* _ ';' _)> */
		func() bool {
			position18, tokenIndex18 := position, tokenIndex
			{
				position19 := position
				if !_rules[rule_]() {
					goto l18
				}
				{
					position20, tokenIndex20 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l21
					}
					position++
					goto l20
				l21:
					position, tokenIndex = position20, tokenIndex20
					if buffer[position] != rune('S') {
						goto l18
					}
					position++
				}
			l20:
				{
					position22, tokenIndex22 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l23
					}
					position++
					goto l22
				l23:
					position, tokenIndex = position22, tokenIndex22
					if buffer[position] != rune('E') {
						goto l18
					}
					position++
				}
			l22:
				{
					position24, tokenIndex24 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l25
					}
					position++
					goto l24
				l25:
					position, tokenIndex = position24, tokenIndex24
					if buffer[position] != rune('L') {
						goto l18
					}
					position++
				}
			l24:
				{
					position26, tokenIndex26 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l27
					}
					position++
					goto l26
				l27:
					position, tokenIndex = position26, tokenIndex26
					if buffer[position] != rune('E') {
						goto l18
					}
					position++
				}
			l26:
				{
					position28, tokenIndex28 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l29
					}
					position++
					goto l28
				l29:
					position, tokenIndex = position28, tokenIndex28
					if buffer[position] != rune('C') {
						goto l18
					}
					position++
				}
			l28:
				{
					position30, tokenIndex30 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l31
					}
					position++
					goto l30
				l31:
					position, tokenIndex = position30, tokenIndex30
					if buffer[position] != rune('T') {
						goto l18
					}
					position++
				}
			l30:
				if !_rules[rule_]() {
					goto l18
				}
				if !_rules[ruleColumns]() {
					goto l18
				}
				if !_rules[rule_]() {
					goto l18
				}
				{
					position32, tokenIndex32 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l33
					}
					position++
					goto l32
				l33:
					position, tokenIndex = position32, tokenIndex32
					if buffer[position] != rune('F') {
						goto l18
					}
					position++
				}
			l32:
				{
					position34, tokenIndex34 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l35
					}
					position++
					goto l34
				l35:
					position, tokenIndex = position34, tokenIndex34
					if buffer[position] != rune('R') {
						goto l18
					}
					position++
				}
			l34:
				{
					position36, tokenIndex36 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l37
					}
					position++
					goto l36
				l37:
					position, tokenIndex = position36, tokenIndex36
					if buffer[position] != rune('O') {
						goto l18
					}
					position++
				}
			l36:
				{
					position38, tokenIndex38 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l39
					}
					position++
					goto l38
				l39:
					position, tokenIndex = position38, tokenIndex38
					if buffer[position] != rune('M') {
						goto l18
					}
					position++
				}
			l38:
				if !_rules[rule_]() {
					goto l18
				}
				if !_rules[ruleSelectTable]() {
					goto l18
				}
				if !_rules[rule_]() {
					goto l18
				}
			l40:
				{
					position41, tokenIndex41 := position, tokenIndex
					if !_rules[ruleWHERESELECT]() {
						goto l41
					}
					goto l40
				l41:
					position, tokenIndex = position41, tokenIndex41
				}
				if !_rules[rule_]() {
					goto l18
				}
			l42:
				{
					position43, tokenIndex43 := position, tokenIndex
					if !_rules[ruleLIMIT]() {
						goto l43
					}
					goto l42
				l43:
					position, tokenIndex = position43, tokenIndex43
				}
				if !_rules[rule_]() {
					goto l18
				}
				if buffer[position] != rune(';') {
					goto l18
				}
				position++
				if !_rules[rule_]() {
					goto l18
				}
				add(ruleSelectStatement, position19)
			}
			return true
		l18:
			position, tokenIndex = position18, tokenIndex18
			return false
		},
		/* 3 DeleteStatement <- <(('d' / 'D') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('t' / 'T') ('e' / 'E') _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ DeleteTable _ WHEREDELETE _ ';' _)> */
		func() bool {
			position44, tokenIndex44 := position, tokenIndex
			{
				position45 := position
				{
					position46, tokenIndex46 := position, tokenIndex
					if buffer[position] != rune('d') {
						goto l47
					}
					position++
					goto l46
				l47:
					position, tokenIndex = position46, tokenIndex46
					if buffer[position] != rune('D') {
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
					if buffer[position] != rune('t') {
						goto l55
					}
					position++
					goto l54
				l55:
					position, tokenIndex = position54, tokenIndex54
					if buffer[position] != rune('T') {
						goto l44
					}
					position++
				}
			l54:
				{
					position56, tokenIndex56 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l57
					}
					position++
					goto l56
				l57:
					position, tokenIndex = position56, tokenIndex56
					if buffer[position] != rune('E') {
						goto l44
					}
					position++
				}
			l56:
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
				if !_rules[ruleDeleteTable]() {
					goto l44
				}
				if !_rules[rule_]() {
					goto l44
				}
				if !_rules[ruleWHEREDELETE]() {
					goto l44
				}
				if !_rules[rule_]() {
					goto l44
				}
				if buffer[position] != rune(';') {
					goto l44
				}
				position++
				if !_rules[rule_]() {
					goto l44
				}
				add(ruleDeleteStatement, position45)
			}
			return true
		l44:
			position, tokenIndex = position44, tokenIndex44
			return false
		},
		/* 4 InsertStatement <- <(_ (('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T')) _ (('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O')) _ InsertBody Action0)> */
		func() bool {
			position66, tokenIndex66 := position, tokenIndex
			{
				position67 := position
				if !_rules[rule_]() {
					goto l66
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
				if !_rules[ruleAction0]() {
					goto l66
				}
				add(ruleInsertStatement, position67)
			}
			return true
		l66:
			position, tokenIndex = position66, tokenIndex66
			return false
		},
		/* 5 CreateStatement <- <(_ (('c' / 'C') ('r' / 'R') ('e' / 'E') ('a' / 'A') ('t' / 'T') ('e' / 'E')) _ (('t' / 'T') ('a' / 'A') ('b' / 'B') ('l' / 'L') ('e' / 'E')) _ CreateTable _ CreateParaColList _ ';' Action1)> */
		func() bool {
			position88, tokenIndex88 := position, tokenIndex
			{
				position89 := position
				if !_rules[rule_]() {
					goto l88
				}
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
				if !_rules[ruleAction1]() {
					goto l88
				}
				add(ruleCreateStatement, position89)
			}
			return true
		l88:
			position, tokenIndex = position88, tokenIndex88
			return false
		},
		/* 6 Columns <- <(SelectParaColList / SelectColList / Asterisk)> */
		func() bool {
			position112, tokenIndex112 := position, tokenIndex
			{
				position113 := position
				{
					position114, tokenIndex114 := position, tokenIndex
					if !_rules[ruleSelectParaColList]() {
						goto l115
					}
					goto l114
				l115:
					position, tokenIndex = position114, tokenIndex114
					if !_rules[ruleSelectColList]() {
						goto l116
					}
					goto l114
				l116:
					position, tokenIndex = position114, tokenIndex114
					if !_rules[ruleAsterisk]() {
						goto l112
					}
				}
			l114:
				add(ruleColumns, position113)
			}
			return true
		l112:
			position, tokenIndex = position112, tokenIndex112
			return false
		},
		/* 7 SelectParaColList <- <('(' _ SelectColList _ ')')> */
		func() bool {
			position117, tokenIndex117 := position, tokenIndex
			{
				position118 := position
				if buffer[position] != rune('(') {
					goto l117
				}
				position++
				if !_rules[rule_]() {
					goto l117
				}
				if !_rules[ruleSelectColList]() {
					goto l117
				}
				if !_rules[rule_]() {
					goto l117
				}
				if buffer[position] != rune(')') {
					goto l117
				}
				position++
				add(ruleSelectParaColList, position118)
			}
			return true
		l117:
			position, tokenIndex = position117, tokenIndex117
			return false
		},
		/* 8 SelectColList <- <((SelectColName _ ',' _)* SelectColName)> */
		func() bool {
			position119, tokenIndex119 := position, tokenIndex
			{
				position120 := position
			l121:
				{
					position122, tokenIndex122 := position, tokenIndex
					if !_rules[ruleSelectColName]() {
						goto l122
					}
					if !_rules[rule_]() {
						goto l122
					}
					if buffer[position] != rune(',') {
						goto l122
					}
					position++
					if !_rules[rule_]() {
						goto l122
					}
					goto l121
				l122:
					position, tokenIndex = position122, tokenIndex122
				}
				if !_rules[ruleSelectColName]() {
					goto l119
				}
				add(ruleSelectColList, position120)
			}
			return true
		l119:
			position, tokenIndex = position119, tokenIndex119
			return false
		},
		/* 9 SelectColName <- <(<AlphaNum+> Action2)> */
		func() bool {
			position123, tokenIndex123 := position, tokenIndex
			{
				position124 := position
				{
					position125 := position
					if !_rules[ruleAlphaNum]() {
						goto l123
					}
				l126:
					{
						position127, tokenIndex127 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l127
						}
						goto l126
					l127:
						position, tokenIndex = position127, tokenIndex127
					}
					add(rulePegText, position125)
				}
				if !_rules[ruleAction2]() {
					goto l123
				}
				add(ruleSelectColName, position124)
			}
			return true
		l123:
			position, tokenIndex = position123, tokenIndex123
			return false
		},
		/* 10 InsertParaColList <- <('(' _ InsertColList _ ')')> */
		func() bool {
			position128, tokenIndex128 := position, tokenIndex
			{
				position129 := position
				if buffer[position] != rune('(') {
					goto l128
				}
				position++
				if !_rules[rule_]() {
					goto l128
				}
				if !_rules[ruleInsertColList]() {
					goto l128
				}
				if !_rules[rule_]() {
					goto l128
				}
				if buffer[position] != rune(')') {
					goto l128
				}
				position++
				add(ruleInsertParaColList, position129)
			}
			return true
		l128:
			position, tokenIndex = position128, tokenIndex128
			return false
		},
		/* 11 InsertColList <- <((_ InsertColName _ ',' _)* InsertColName)> */
		func() bool {
			position130, tokenIndex130 := position, tokenIndex
			{
				position131 := position
			l132:
				{
					position133, tokenIndex133 := position, tokenIndex
					if !_rules[rule_]() {
						goto l133
					}
					if !_rules[ruleInsertColName]() {
						goto l133
					}
					if !_rules[rule_]() {
						goto l133
					}
					if buffer[position] != rune(',') {
						goto l133
					}
					position++
					if !_rules[rule_]() {
						goto l133
					}
					goto l132
				l133:
					position, tokenIndex = position133, tokenIndex133
				}
				if !_rules[ruleInsertColName]() {
					goto l130
				}
				add(ruleInsertColList, position131)
			}
			return true
		l130:
			position, tokenIndex = position130, tokenIndex130
			return false
		},
		/* 12 InsertColName <- <(<AlphaNum+> Action3)> */
		func() bool {
			position134, tokenIndex134 := position, tokenIndex
			{
				position135 := position
				{
					position136 := position
					if !_rules[ruleAlphaNum]() {
						goto l134
					}
				l137:
					{
						position138, tokenIndex138 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l138
						}
						goto l137
					l138:
						position, tokenIndex = position138, tokenIndex138
					}
					add(rulePegText, position136)
				}
				if !_rules[ruleAction3]() {
					goto l134
				}
				add(ruleInsertColName, position135)
			}
			return true
		l134:
			position, tokenIndex = position134, tokenIndex134
			return false
		},
		/* 13 CreateParaColList <- <('(' _ CreateColList _ ')')> */
		func() bool {
			position139, tokenIndex139 := position, tokenIndex
			{
				position140 := position
				if buffer[position] != rune('(') {
					goto l139
				}
				position++
				if !_rules[rule_]() {
					goto l139
				}
				if !_rules[ruleCreateColList]() {
					goto l139
				}
				if !_rules[rule_]() {
					goto l139
				}
				if buffer[position] != rune(')') {
					goto l139
				}
				position++
				add(ruleCreateParaColList, position140)
			}
			return true
		l139:
			position, tokenIndex = position139, tokenIndex139
			return false
		},
		/* 14 CreateColList <- <((_ CreateColName _ ',' _)* CreateColName)> */
		func() bool {
			position141, tokenIndex141 := position, tokenIndex
			{
				position142 := position
			l143:
				{
					position144, tokenIndex144 := position, tokenIndex
					if !_rules[rule_]() {
						goto l144
					}
					if !_rules[ruleCreateColName]() {
						goto l144
					}
					if !_rules[rule_]() {
						goto l144
					}
					if buffer[position] != rune(',') {
						goto l144
					}
					position++
					if !_rules[rule_]() {
						goto l144
					}
					goto l143
				l144:
					position, tokenIndex = position144, tokenIndex144
				}
				if !_rules[ruleCreateColName]() {
					goto l141
				}
				add(ruleCreateColList, position142)
			}
			return true
		l141:
			position, tokenIndex = position141, tokenIndex141
			return false
		},
		/* 15 CreateColName <- <(<AlphaNum+> Action4)> */
		func() bool {
			position145, tokenIndex145 := position, tokenIndex
			{
				position146 := position
				{
					position147 := position
					if !_rules[ruleAlphaNum]() {
						goto l145
					}
				l148:
					{
						position149, tokenIndex149 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l149
						}
						goto l148
					l149:
						position, tokenIndex = position149, tokenIndex149
					}
					add(rulePegText, position147)
				}
				if !_rules[ruleAction4]() {
					goto l145
				}
				add(ruleCreateColName, position146)
			}
			return true
		l145:
			position, tokenIndex = position145, tokenIndex145
			return false
		},
		/* 16 InsertBody <- <(InsertTable _ Values _ ';')> */
		func() bool {
			position150, tokenIndex150 := position, tokenIndex
			{
				position151 := position
				if !_rules[ruleInsertTable]() {
					goto l150
				}
				if !_rules[rule_]() {
					goto l150
				}
				if !_rules[ruleValues]() {
					goto l150
				}
				if !_rules[rule_]() {
					goto l150
				}
				if buffer[position] != rune(';') {
					goto l150
				}
				position++
				add(ruleInsertBody, position151)
			}
			return true
		l150:
			position, tokenIndex = position150, tokenIndex150
			return false
		},
		/* 17 Values <- <(InsertParaColList? _ (('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S')) _ ValuesBody)> */
		func() bool {
			position152, tokenIndex152 := position, tokenIndex
			{
				position153 := position
				{
					position154, tokenIndex154 := position, tokenIndex
					if !_rules[ruleInsertParaColList]() {
						goto l154
					}
					goto l155
				l154:
					position, tokenIndex = position154, tokenIndex154
				}
			l155:
				if !_rules[rule_]() {
					goto l152
				}
				{
					position156, tokenIndex156 := position, tokenIndex
					if buffer[position] != rune('v') {
						goto l157
					}
					position++
					goto l156
				l157:
					position, tokenIndex = position156, tokenIndex156
					if buffer[position] != rune('V') {
						goto l152
					}
					position++
				}
			l156:
				{
					position158, tokenIndex158 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l159
					}
					position++
					goto l158
				l159:
					position, tokenIndex = position158, tokenIndex158
					if buffer[position] != rune('A') {
						goto l152
					}
					position++
				}
			l158:
				{
					position160, tokenIndex160 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l161
					}
					position++
					goto l160
				l161:
					position, tokenIndex = position160, tokenIndex160
					if buffer[position] != rune('L') {
						goto l152
					}
					position++
				}
			l160:
				{
					position162, tokenIndex162 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l163
					}
					position++
					goto l162
				l163:
					position, tokenIndex = position162, tokenIndex162
					if buffer[position] != rune('U') {
						goto l152
					}
					position++
				}
			l162:
				{
					position164, tokenIndex164 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l165
					}
					position++
					goto l164
				l165:
					position, tokenIndex = position164, tokenIndex164
					if buffer[position] != rune('E') {
						goto l152
					}
					position++
				}
			l164:
				{
					position166, tokenIndex166 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l167
					}
					position++
					goto l166
				l167:
					position, tokenIndex = position166, tokenIndex166
					if buffer[position] != rune('S') {
						goto l152
					}
					position++
				}
			l166:
				if !_rules[rule_]() {
					goto l152
				}
				if !_rules[ruleValuesBody]() {
					goto l152
				}
				add(ruleValues, position153)
			}
			return true
		l152:
			position, tokenIndex = position152, tokenIndex152
			return false
		},
		/* 18 ValuesBody <- <('(' _ ValList _ ')')> */
		func() bool {
			position168, tokenIndex168 := position, tokenIndex
			{
				position169 := position
				if buffer[position] != rune('(') {
					goto l168
				}
				position++
				if !_rules[rule_]() {
					goto l168
				}
				if !_rules[ruleValList]() {
					goto l168
				}
				if !_rules[rule_]() {
					goto l168
				}
				if buffer[position] != rune(')') {
					goto l168
				}
				position++
				add(ruleValuesBody, position169)
			}
			return true
		l168:
			position, tokenIndex = position168, tokenIndex168
			return false
		},
		/* 19 ValList <- <((_ InsertValue _ ',' _)* _ InsertValue)> */
		func() bool {
			position170, tokenIndex170 := position, tokenIndex
			{
				position171 := position
			l172:
				{
					position173, tokenIndex173 := position, tokenIndex
					if !_rules[rule_]() {
						goto l173
					}
					if !_rules[ruleInsertValue]() {
						goto l173
					}
					if !_rules[rule_]() {
						goto l173
					}
					if buffer[position] != rune(',') {
						goto l173
					}
					position++
					if !_rules[rule_]() {
						goto l173
					}
					goto l172
				l173:
					position, tokenIndex = position173, tokenIndex173
				}
				if !_rules[rule_]() {
					goto l170
				}
				if !_rules[ruleInsertValue]() {
					goto l170
				}
				add(ruleValList, position171)
			}
			return true
		l170:
			position, tokenIndex = position170, tokenIndex170
			return false
		},
		/* 20 InsertValue <- <('"'? EachValue '"'?)> */
		func() bool {
			position174, tokenIndex174 := position, tokenIndex
			{
				position175 := position
				{
					position176, tokenIndex176 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l176
					}
					position++
					goto l177
				l176:
					position, tokenIndex = position176, tokenIndex176
				}
			l177:
				if !_rules[ruleEachValue]() {
					goto l174
				}
				{
					position178, tokenIndex178 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l178
					}
					position++
					goto l179
				l178:
					position, tokenIndex = position178, tokenIndex178
				}
			l179:
				add(ruleInsertValue, position175)
			}
			return true
		l174:
			position, tokenIndex = position174, tokenIndex174
			return false
		},
		/* 21 EachValue <- <(<AlphaNum+> Action5)> */
		func() bool {
			position180, tokenIndex180 := position, tokenIndex
			{
				position181 := position
				{
					position182 := position
					if !_rules[ruleAlphaNum]() {
						goto l180
					}
				l183:
					{
						position184, tokenIndex184 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l184
						}
						goto l183
					l184:
						position, tokenIndex = position184, tokenIndex184
					}
					add(rulePegText, position182)
				}
				if !_rules[ruleAction5]() {
					goto l180
				}
				add(ruleEachValue, position181)
			}
			return true
		l180:
			position, tokenIndex = position180, tokenIndex180
			return false
		},
		/* 22 Asterisk <- <('*' Action6)> */
		func() bool {
			position185, tokenIndex185 := position, tokenIndex
			{
				position186 := position
				if buffer[position] != rune('*') {
					goto l185
				}
				position++
				if !_rules[ruleAction6]() {
					goto l185
				}
				add(ruleAsterisk, position186)
			}
			return true
		l185:
			position, tokenIndex = position185, tokenIndex185
			return false
		},
		/* 23 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position187, tokenIndex187 := position, tokenIndex
			{
				position188 := position
				if buffer[position] != rune('/') {
					goto l187
				}
				position++
				if buffer[position] != rune('/') {
					goto l187
				}
				position++
			l189:
				{
					position190, tokenIndex190 := position, tokenIndex
					{
						position191, tokenIndex191 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l191
						}
						position++
						goto l190
					l191:
						position, tokenIndex = position191, tokenIndex191
					}
					if !matchDot() {
						goto l190
					}
					goto l189
				l190:
					position, tokenIndex = position190, tokenIndex190
				}
				add(rulelineComment, position188)
			}
			return true
		l187:
			position, tokenIndex = position187, tokenIndex187
			return false
		},
		/* 24 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position192, tokenIndex192 := position, tokenIndex
			{
				position193 := position
				if buffer[position] != rune('/') {
					goto l192
				}
				position++
				if buffer[position] != rune('*') {
					goto l192
				}
				position++
			l194:
				{
					position195, tokenIndex195 := position, tokenIndex
					{
						position196, tokenIndex196 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l196
						}
						position++
						if buffer[position] != rune('/') {
							goto l196
						}
						position++
						goto l195
					l196:
						position, tokenIndex = position196, tokenIndex196
					}
					{
						position197, tokenIndex197 := position, tokenIndex
						if !matchDot() {
							goto l198
						}
						goto l197
					l198:
						position, tokenIndex = position197, tokenIndex197
						if buffer[position] != rune('\n') {
							goto l195
						}
						position++
					}
				l197:
					goto l194
				l195:
					position, tokenIndex = position195, tokenIndex195
				}
				if buffer[position] != rune('*') {
					goto l192
				}
				position++
				if buffer[position] != rune('/') {
					goto l192
				}
				position++
				add(ruleblockComment, position193)
			}
			return true
		l192:
			position, tokenIndex = position192, tokenIndex192
			return false
		},
		/* 25 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position199, tokenIndex199 := position, tokenIndex
			{
				position200 := position
				{
					position201, tokenIndex201 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l202
					}
					position++
					goto l201
				l202:
					position, tokenIndex = position201, tokenIndex201
					if buffer[position] != rune('\t') {
						goto l203
					}
					position++
					goto l201
				l203:
					position, tokenIndex = position201, tokenIndex201
					if buffer[position] != rune('\n') {
						goto l204
					}
					position++
					goto l201
				l204:
					position, tokenIndex = position201, tokenIndex201
					if buffer[position] != rune('\r') {
						goto l199
					}
					position++
				}
			l201:
				add(rulews, position200)
			}
			return true
		l199:
			position, tokenIndex = position199, tokenIndex199
			return false
		},
		/* 26 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position206 := position
			l207:
				{
					position208, tokenIndex208 := position, tokenIndex
					{
						position209, tokenIndex209 := position, tokenIndex
						if !_rules[rulews]() {
							goto l210
						}
						goto l209
					l210:
						position, tokenIndex = position209, tokenIndex209
						if !_rules[rulelineComment]() {
							goto l211
						}
						goto l209
					l211:
						position, tokenIndex = position209, tokenIndex209
						if !_rules[ruleblockComment]() {
							goto l208
						}
					}
				l209:
					goto l207
				l208:
					position, tokenIndex = position208, tokenIndex208
				}
				add(rule_, position206)
			}
			return true
		},
		/* 27 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position212, tokenIndex212 := position, tokenIndex
			{
				position213 := position
				{
					position214, tokenIndex214 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l215
					}
					position++
					goto l214
				l215:
					position, tokenIndex = position214, tokenIndex214
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l216
					}
					position++
					goto l214
				l216:
					position, tokenIndex = position214, tokenIndex214
					if buffer[position] != rune('_') {
						goto l212
					}
					position++
				}
			l214:
				add(ruleLetter, position213)
			}
			return true
		l212:
			position, tokenIndex = position212, tokenIndex212
			return false
		},
		/* 28 Number <- <([0-9] ('.' [0-9])*)> */
		func() bool {
			position217, tokenIndex217 := position, tokenIndex
			{
				position218 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l217
				}
				position++
			l219:
				{
					position220, tokenIndex220 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l220
					}
					position++
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l220
					}
					position++
					goto l219
				l220:
					position, tokenIndex = position220, tokenIndex220
				}
				add(ruleNumber, position218)
			}
			return true
		l217:
			position, tokenIndex = position217, tokenIndex217
			return false
		},
		/* 29 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position221, tokenIndex221 := position, tokenIndex
			{
				position222 := position
				{
					position223, tokenIndex223 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l224
					}
					goto l223
				l224:
					position, tokenIndex = position223, tokenIndex223
					if !_rules[ruleNumber]() {
						goto l221
					}
				}
			l223:
				add(ruleAlphaNum, position222)
			}
			return true
		l221:
			position, tokenIndex = position221, tokenIndex221
			return false
		},
		/* 30 SelectTable <- <(SelectKeyspace '.' SelectTableName)> */
		func() bool {
			position225, tokenIndex225 := position, tokenIndex
			{
				position226 := position
				if !_rules[ruleSelectKeyspace]() {
					goto l225
				}
				if buffer[position] != rune('.') {
					goto l225
				}
				position++
				if !_rules[ruleSelectTableName]() {
					goto l225
				}
				add(ruleSelectTable, position226)
			}
			return true
		l225:
			position, tokenIndex = position225, tokenIndex225
			return false
		},
		/* 31 SelectKeyspace <- <(<AlphaNum+> Action7)> */
		func() bool {
			position227, tokenIndex227 := position, tokenIndex
			{
				position228 := position
				{
					position229 := position
					if !_rules[ruleAlphaNum]() {
						goto l227
					}
				l230:
					{
						position231, tokenIndex231 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l231
						}
						goto l230
					l231:
						position, tokenIndex = position231, tokenIndex231
					}
					add(rulePegText, position229)
				}
				if !_rules[ruleAction7]() {
					goto l227
				}
				add(ruleSelectKeyspace, position228)
			}
			return true
		l227:
			position, tokenIndex = position227, tokenIndex227
			return false
		},
		/* 32 SelectTableName <- <(<AlphaNum+> Action8)> */
		func() bool {
			position232, tokenIndex232 := position, tokenIndex
			{
				position233 := position
				{
					position234 := position
					if !_rules[ruleAlphaNum]() {
						goto l232
					}
				l235:
					{
						position236, tokenIndex236 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l236
						}
						goto l235
					l236:
						position, tokenIndex = position236, tokenIndex236
					}
					add(rulePegText, position234)
				}
				if !_rules[ruleAction8]() {
					goto l232
				}
				add(ruleSelectTableName, position233)
			}
			return true
		l232:
			position, tokenIndex = position232, tokenIndex232
			return false
		},
		/* 33 InsertTable <- <(InsertKeyspace '.' InsertTableName)> */
		func() bool {
			position237, tokenIndex237 := position, tokenIndex
			{
				position238 := position
				if !_rules[ruleInsertKeyspace]() {
					goto l237
				}
				if buffer[position] != rune('.') {
					goto l237
				}
				position++
				if !_rules[ruleInsertTableName]() {
					goto l237
				}
				add(ruleInsertTable, position238)
			}
			return true
		l237:
			position, tokenIndex = position237, tokenIndex237
			return false
		},
		/* 34 InsertKeyspace <- <(<AlphaNum+> Action9)> */
		func() bool {
			position239, tokenIndex239 := position, tokenIndex
			{
				position240 := position
				{
					position241 := position
					if !_rules[ruleAlphaNum]() {
						goto l239
					}
				l242:
					{
						position243, tokenIndex243 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l243
						}
						goto l242
					l243:
						position, tokenIndex = position243, tokenIndex243
					}
					add(rulePegText, position241)
				}
				if !_rules[ruleAction9]() {
					goto l239
				}
				add(ruleInsertKeyspace, position240)
			}
			return true
		l239:
			position, tokenIndex = position239, tokenIndex239
			return false
		},
		/* 35 InsertTableName <- <(<AlphaNum+> Action10)> */
		func() bool {
			position244, tokenIndex244 := position, tokenIndex
			{
				position245 := position
				{
					position246 := position
					if !_rules[ruleAlphaNum]() {
						goto l244
					}
				l247:
					{
						position248, tokenIndex248 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l248
						}
						goto l247
					l248:
						position, tokenIndex = position248, tokenIndex248
					}
					add(rulePegText, position246)
				}
				if !_rules[ruleAction10]() {
					goto l244
				}
				add(ruleInsertTableName, position245)
			}
			return true
		l244:
			position, tokenIndex = position244, tokenIndex244
			return false
		},
		/* 36 CreateTable <- <(CreateKeyspace '.' CreateTableName)> */
		func() bool {
			position249, tokenIndex249 := position, tokenIndex
			{
				position250 := position
				if !_rules[ruleCreateKeyspace]() {
					goto l249
				}
				if buffer[position] != rune('.') {
					goto l249
				}
				position++
				if !_rules[ruleCreateTableName]() {
					goto l249
				}
				add(ruleCreateTable, position250)
			}
			return true
		l249:
			position, tokenIndex = position249, tokenIndex249
			return false
		},
		/* 37 CreateKeyspace <- <(<AlphaNum+> Action11)> */
		func() bool {
			position251, tokenIndex251 := position, tokenIndex
			{
				position252 := position
				{
					position253 := position
					if !_rules[ruleAlphaNum]() {
						goto l251
					}
				l254:
					{
						position255, tokenIndex255 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l255
						}
						goto l254
					l255:
						position, tokenIndex = position255, tokenIndex255
					}
					add(rulePegText, position253)
				}
				if !_rules[ruleAction11]() {
					goto l251
				}
				add(ruleCreateKeyspace, position252)
			}
			return true
		l251:
			position, tokenIndex = position251, tokenIndex251
			return false
		},
		/* 38 CreateTableName <- <(<AlphaNum+> Action12)> */
		func() bool {
			position256, tokenIndex256 := position, tokenIndex
			{
				position257 := position
				{
					position258 := position
					if !_rules[ruleAlphaNum]() {
						goto l256
					}
				l259:
					{
						position260, tokenIndex260 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l260
						}
						goto l259
					l260:
						position, tokenIndex = position260, tokenIndex260
					}
					add(rulePegText, position258)
				}
				if !_rules[ruleAction12]() {
					goto l256
				}
				add(ruleCreateTableName, position257)
			}
			return true
		l256:
			position, tokenIndex = position256, tokenIndex256
			return false
		},
		/* 39 DropTable <- <(DropTableKeyspace '.' DropTableName)> */
		func() bool {
			position261, tokenIndex261 := position, tokenIndex
			{
				position262 := position
				if !_rules[ruleDropTableKeyspace]() {
					goto l261
				}
				if buffer[position] != rune('.') {
					goto l261
				}
				position++
				if !_rules[ruleDropTableName]() {
					goto l261
				}
				add(ruleDropTable, position262)
			}
			return true
		l261:
			position, tokenIndex = position261, tokenIndex261
			return false
		},
		/* 40 DropTableKeyspace <- <(<AlphaNum+> Action13)> */
		func() bool {
			position263, tokenIndex263 := position, tokenIndex
			{
				position264 := position
				{
					position265 := position
					if !_rules[ruleAlphaNum]() {
						goto l263
					}
				l266:
					{
						position267, tokenIndex267 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l267
						}
						goto l266
					l267:
						position, tokenIndex = position267, tokenIndex267
					}
					add(rulePegText, position265)
				}
				if !_rules[ruleAction13]() {
					goto l263
				}
				add(ruleDropTableKeyspace, position264)
			}
			return true
		l263:
			position, tokenIndex = position263, tokenIndex263
			return false
		},
		/* 41 DropTableName <- <(<AlphaNum+> Action14)> */
		func() bool {
			position268, tokenIndex268 := position, tokenIndex
			{
				position269 := position
				{
					position270 := position
					if !_rules[ruleAlphaNum]() {
						goto l268
					}
				l271:
					{
						position272, tokenIndex272 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l272
						}
						goto l271
					l272:
						position, tokenIndex = position272, tokenIndex272
					}
					add(rulePegText, position270)
				}
				if !_rules[ruleAction14]() {
					goto l268
				}
				add(ruleDropTableName, position269)
			}
			return true
		l268:
			position, tokenIndex = position268, tokenIndex268
			return false
		},
		/* 42 DeleteTable <- <(DeleteKeyspace '.' DeleteTableName)> */
		func() bool {
			position273, tokenIndex273 := position, tokenIndex
			{
				position274 := position
				if !_rules[ruleDeleteKeyspace]() {
					goto l273
				}
				if buffer[position] != rune('.') {
					goto l273
				}
				position++
				if !_rules[ruleDeleteTableName]() {
					goto l273
				}
				add(ruleDeleteTable, position274)
			}
			return true
		l273:
			position, tokenIndex = position273, tokenIndex273
			return false
		},
		/* 43 DeleteKeyspace <- <(<AlphaNum+> Action15)> */
		func() bool {
			position275, tokenIndex275 := position, tokenIndex
			{
				position276 := position
				{
					position277 := position
					if !_rules[ruleAlphaNum]() {
						goto l275
					}
				l278:
					{
						position279, tokenIndex279 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l279
						}
						goto l278
					l279:
						position, tokenIndex = position279, tokenIndex279
					}
					add(rulePegText, position277)
				}
				if !_rules[ruleAction15]() {
					goto l275
				}
				add(ruleDeleteKeyspace, position276)
			}
			return true
		l275:
			position, tokenIndex = position275, tokenIndex275
			return false
		},
		/* 44 DeleteTableName <- <(<AlphaNum+> Action16)> */
		func() bool {
			position280, tokenIndex280 := position, tokenIndex
			{
				position281 := position
				{
					position282 := position
					if !_rules[ruleAlphaNum]() {
						goto l280
					}
				l283:
					{
						position284, tokenIndex284 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l284
						}
						goto l283
					l284:
						position, tokenIndex = position284, tokenIndex284
					}
					add(rulePegText, position282)
				}
				if !_rules[ruleAction16]() {
					goto l280
				}
				add(ruleDeleteTableName, position281)
			}
			return true
		l280:
			position, tokenIndex = position280, tokenIndex280
			return false
		},
		/* 45 WHERESELECT <- <(('w' / 'W') ('h' / 'H') ('e' / 'E') ('r' / 'R') ('e' / 'E') _ SELECTFILTERS+)> */
		func() bool {
			position285, tokenIndex285 := position, tokenIndex
			{
				position286 := position
				{
					position287, tokenIndex287 := position, tokenIndex
					if buffer[position] != rune('w') {
						goto l288
					}
					position++
					goto l287
				l288:
					position, tokenIndex = position287, tokenIndex287
					if buffer[position] != rune('W') {
						goto l285
					}
					position++
				}
			l287:
				{
					position289, tokenIndex289 := position, tokenIndex
					if buffer[position] != rune('h') {
						goto l290
					}
					position++
					goto l289
				l290:
					position, tokenIndex = position289, tokenIndex289
					if buffer[position] != rune('H') {
						goto l285
					}
					position++
				}
			l289:
				{
					position291, tokenIndex291 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l292
					}
					position++
					goto l291
				l292:
					position, tokenIndex = position291, tokenIndex291
					if buffer[position] != rune('E') {
						goto l285
					}
					position++
				}
			l291:
				{
					position293, tokenIndex293 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l294
					}
					position++
					goto l293
				l294:
					position, tokenIndex = position293, tokenIndex293
					if buffer[position] != rune('R') {
						goto l285
					}
					position++
				}
			l293:
				{
					position295, tokenIndex295 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l296
					}
					position++
					goto l295
				l296:
					position, tokenIndex = position295, tokenIndex295
					if buffer[position] != rune('E') {
						goto l285
					}
					position++
				}
			l295:
				if !_rules[rule_]() {
					goto l285
				}
				if !_rules[ruleSELECTFILTERS]() {
					goto l285
				}
			l297:
				{
					position298, tokenIndex298 := position, tokenIndex
					if !_rules[ruleSELECTFILTERS]() {
						goto l298
					}
					goto l297
				l298:
					position, tokenIndex = position298, tokenIndex298
				}
				add(ruleWHERESELECT, position286)
			}
			return true
		l285:
			position, tokenIndex = position285, tokenIndex285
			return false
		},
		/* 46 SELECTFILTERS <- <((SELECTFILTER _ (('a' / 'A') ('n' / 'N') ('d' / 'D')))* SELECTFILTER)> */
		func() bool {
			position299, tokenIndex299 := position, tokenIndex
			{
				position300 := position
			l301:
				{
					position302, tokenIndex302 := position, tokenIndex
					if !_rules[ruleSELECTFILTER]() {
						goto l302
					}
					if !_rules[rule_]() {
						goto l302
					}
					{
						position303, tokenIndex303 := position, tokenIndex
						if buffer[position] != rune('a') {
							goto l304
						}
						position++
						goto l303
					l304:
						position, tokenIndex = position303, tokenIndex303
						if buffer[position] != rune('A') {
							goto l302
						}
						position++
					}
				l303:
					{
						position305, tokenIndex305 := position, tokenIndex
						if buffer[position] != rune('n') {
							goto l306
						}
						position++
						goto l305
					l306:
						position, tokenIndex = position305, tokenIndex305
						if buffer[position] != rune('N') {
							goto l302
						}
						position++
					}
				l305:
					{
						position307, tokenIndex307 := position, tokenIndex
						if buffer[position] != rune('d') {
							goto l308
						}
						position++
						goto l307
					l308:
						position, tokenIndex = position307, tokenIndex307
						if buffer[position] != rune('D') {
							goto l302
						}
						position++
					}
				l307:
					goto l301
				l302:
					position, tokenIndex = position302, tokenIndex302
				}
				if !_rules[ruleSELECTFILTER]() {
					goto l299
				}
				add(ruleSELECTFILTERS, position300)
			}
			return true
		l299:
			position, tokenIndex = position299, tokenIndex299
			return false
		},
		/* 47 SELECTFILTER <- <(_ SelectFilterColumn _ SelectOperation _ SelectFilterValue _)> */
		func() bool {
			position309, tokenIndex309 := position, tokenIndex
			{
				position310 := position
				if !_rules[rule_]() {
					goto l309
				}
				if !_rules[ruleSelectFilterColumn]() {
					goto l309
				}
				if !_rules[rule_]() {
					goto l309
				}
				if !_rules[ruleSelectOperation]() {
					goto l309
				}
				if !_rules[rule_]() {
					goto l309
				}
				if !_rules[ruleSelectFilterValue]() {
					goto l309
				}
				if !_rules[rule_]() {
					goto l309
				}
				add(ruleSELECTFILTER, position310)
			}
			return true
		l309:
			position, tokenIndex = position309, tokenIndex309
			return false
		},
		/* 48 SelectOperation <- <(SelectEquality / SelectLessThan / SelectGreaterThan)> */
		func() bool {
			position311, tokenIndex311 := position, tokenIndex
			{
				position312 := position
				{
					position313, tokenIndex313 := position, tokenIndex
					if !_rules[ruleSelectEquality]() {
						goto l314
					}
					goto l313
				l314:
					position, tokenIndex = position313, tokenIndex313
					if !_rules[ruleSelectLessThan]() {
						goto l315
					}
					goto l313
				l315:
					position, tokenIndex = position313, tokenIndex313
					if !_rules[ruleSelectGreaterThan]() {
						goto l311
					}
				}
			l313:
				add(ruleSelectOperation, position312)
			}
			return true
		l311:
			position, tokenIndex = position311, tokenIndex311
			return false
		},
		/* 49 SelectEquality <- <(<'='> Action17)> */
		func() bool {
			position316, tokenIndex316 := position, tokenIndex
			{
				position317 := position
				{
					position318 := position
					if buffer[position] != rune('=') {
						goto l316
					}
					position++
					add(rulePegText, position318)
				}
				if !_rules[ruleAction17]() {
					goto l316
				}
				add(ruleSelectEquality, position317)
			}
			return true
		l316:
			position, tokenIndex = position316, tokenIndex316
			return false
		},
		/* 50 SelectLessThan <- <(<('<' '='?)> Action18)> */
		func() bool {
			position319, tokenIndex319 := position, tokenIndex
			{
				position320 := position
				{
					position321 := position
					if buffer[position] != rune('<') {
						goto l319
					}
					position++
					{
						position322, tokenIndex322 := position, tokenIndex
						if buffer[position] != rune('=') {
							goto l322
						}
						position++
						goto l323
					l322:
						position, tokenIndex = position322, tokenIndex322
					}
				l323:
					add(rulePegText, position321)
				}
				if !_rules[ruleAction18]() {
					goto l319
				}
				add(ruleSelectLessThan, position320)
			}
			return true
		l319:
			position, tokenIndex = position319, tokenIndex319
			return false
		},
		/* 51 SelectGreaterThan <- <(<('>' '='?)> Action19)> */
		func() bool {
			position324, tokenIndex324 := position, tokenIndex
			{
				position325 := position
				{
					position326 := position
					if buffer[position] != rune('>') {
						goto l324
					}
					position++
					{
						position327, tokenIndex327 := position, tokenIndex
						if buffer[position] != rune('=') {
							goto l327
						}
						position++
						goto l328
					l327:
						position, tokenIndex = position327, tokenIndex327
					}
				l328:
					add(rulePegText, position326)
				}
				if !_rules[ruleAction19]() {
					goto l324
				}
				add(ruleSelectGreaterThan, position325)
			}
			return true
		l324:
			position, tokenIndex = position324, tokenIndex324
			return false
		},
		/* 52 SelectFilterColumn <- <(<AlphaNum+> Action20)> */
		func() bool {
			position329, tokenIndex329 := position, tokenIndex
			{
				position330 := position
				{
					position331 := position
					if !_rules[ruleAlphaNum]() {
						goto l329
					}
				l332:
					{
						position333, tokenIndex333 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l333
						}
						goto l332
					l333:
						position, tokenIndex = position333, tokenIndex333
					}
					add(rulePegText, position331)
				}
				if !_rules[ruleAction20]() {
					goto l329
				}
				add(ruleSelectFilterColumn, position330)
			}
			return true
		l329:
			position, tokenIndex = position329, tokenIndex329
			return false
		},
		/* 53 SelectFilterValue <- <(<AlphaNum+> Action21)> */
		func() bool {
			position334, tokenIndex334 := position, tokenIndex
			{
				position335 := position
				{
					position336 := position
					if !_rules[ruleAlphaNum]() {
						goto l334
					}
				l337:
					{
						position338, tokenIndex338 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l338
						}
						goto l337
					l338:
						position, tokenIndex = position338, tokenIndex338
					}
					add(rulePegText, position336)
				}
				if !_rules[ruleAction21]() {
					goto l334
				}
				add(ruleSelectFilterValue, position335)
			}
			return true
		l334:
			position, tokenIndex = position334, tokenIndex334
			return false
		},
		/* 54 WHEREDELETE <- <(('w' / 'W') ('h' / 'H') ('e' / 'E') ('r' / 'R') ('e' / 'E') _ DELETEFILTER)> */
		func() bool {
			position339, tokenIndex339 := position, tokenIndex
			{
				position340 := position
				{
					position341, tokenIndex341 := position, tokenIndex
					if buffer[position] != rune('w') {
						goto l342
					}
					position++
					goto l341
				l342:
					position, tokenIndex = position341, tokenIndex341
					if buffer[position] != rune('W') {
						goto l339
					}
					position++
				}
			l341:
				{
					position343, tokenIndex343 := position, tokenIndex
					if buffer[position] != rune('h') {
						goto l344
					}
					position++
					goto l343
				l344:
					position, tokenIndex = position343, tokenIndex343
					if buffer[position] != rune('H') {
						goto l339
					}
					position++
				}
			l343:
				{
					position345, tokenIndex345 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l346
					}
					position++
					goto l345
				l346:
					position, tokenIndex = position345, tokenIndex345
					if buffer[position] != rune('E') {
						goto l339
					}
					position++
				}
			l345:
				{
					position347, tokenIndex347 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l348
					}
					position++
					goto l347
				l348:
					position, tokenIndex = position347, tokenIndex347
					if buffer[position] != rune('R') {
						goto l339
					}
					position++
				}
			l347:
				{
					position349, tokenIndex349 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l350
					}
					position++
					goto l349
				l350:
					position, tokenIndex = position349, tokenIndex349
					if buffer[position] != rune('E') {
						goto l339
					}
					position++
				}
			l349:
				if !_rules[rule_]() {
					goto l339
				}
				if !_rules[ruleDELETEFILTER]() {
					goto l339
				}
				add(ruleWHEREDELETE, position340)
			}
			return true
		l339:
			position, tokenIndex = position339, tokenIndex339
			return false
		},
		/* 55 DELETEFILTER <- <(_ DeleteFilterColumn _ DeleteOperation _ DeleteFilterValue _)> */
		func() bool {
			position351, tokenIndex351 := position, tokenIndex
			{
				position352 := position
				if !_rules[rule_]() {
					goto l351
				}
				if !_rules[ruleDeleteFilterColumn]() {
					goto l351
				}
				if !_rules[rule_]() {
					goto l351
				}
				if !_rules[ruleDeleteOperation]() {
					goto l351
				}
				if !_rules[rule_]() {
					goto l351
				}
				if !_rules[ruleDeleteFilterValue]() {
					goto l351
				}
				if !_rules[rule_]() {
					goto l351
				}
				add(ruleDELETEFILTER, position352)
			}
			return true
		l351:
			position, tokenIndex = position351, tokenIndex351
			return false
		},
		/* 56 DeleteOperation <- <(DeleteEquality / DeleteLessThan / DeleteGreaterThan)> */
		func() bool {
			position353, tokenIndex353 := position, tokenIndex
			{
				position354 := position
				{
					position355, tokenIndex355 := position, tokenIndex
					if !_rules[ruleDeleteEquality]() {
						goto l356
					}
					goto l355
				l356:
					position, tokenIndex = position355, tokenIndex355
					if !_rules[ruleDeleteLessThan]() {
						goto l357
					}
					goto l355
				l357:
					position, tokenIndex = position355, tokenIndex355
					if !_rules[ruleDeleteGreaterThan]() {
						goto l353
					}
				}
			l355:
				add(ruleDeleteOperation, position354)
			}
			return true
		l353:
			position, tokenIndex = position353, tokenIndex353
			return false
		},
		/* 57 DeleteEquality <- <(<'='> Action22)> */
		func() bool {
			position358, tokenIndex358 := position, tokenIndex
			{
				position359 := position
				{
					position360 := position
					if buffer[position] != rune('=') {
						goto l358
					}
					position++
					add(rulePegText, position360)
				}
				if !_rules[ruleAction22]() {
					goto l358
				}
				add(ruleDeleteEquality, position359)
			}
			return true
		l358:
			position, tokenIndex = position358, tokenIndex358
			return false
		},
		/* 58 DeleteLessThan <- <(<('<' '='?)> Action23)> */
		func() bool {
			position361, tokenIndex361 := position, tokenIndex
			{
				position362 := position
				{
					position363 := position
					if buffer[position] != rune('<') {
						goto l361
					}
					position++
					{
						position364, tokenIndex364 := position, tokenIndex
						if buffer[position] != rune('=') {
							goto l364
						}
						position++
						goto l365
					l364:
						position, tokenIndex = position364, tokenIndex364
					}
				l365:
					add(rulePegText, position363)
				}
				if !_rules[ruleAction23]() {
					goto l361
				}
				add(ruleDeleteLessThan, position362)
			}
			return true
		l361:
			position, tokenIndex = position361, tokenIndex361
			return false
		},
		/* 59 DeleteGreaterThan <- <(<('>' '='?)> Action24)> */
		func() bool {
			position366, tokenIndex366 := position, tokenIndex
			{
				position367 := position
				{
					position368 := position
					if buffer[position] != rune('>') {
						goto l366
					}
					position++
					{
						position369, tokenIndex369 := position, tokenIndex
						if buffer[position] != rune('=') {
							goto l369
						}
						position++
						goto l370
					l369:
						position, tokenIndex = position369, tokenIndex369
					}
				l370:
					add(rulePegText, position368)
				}
				if !_rules[ruleAction24]() {
					goto l366
				}
				add(ruleDeleteGreaterThan, position367)
			}
			return true
		l366:
			position, tokenIndex = position366, tokenIndex366
			return false
		},
		/* 60 DeleteFilterColumn <- <(<AlphaNum+> Action25)> */
		func() bool {
			position371, tokenIndex371 := position, tokenIndex
			{
				position372 := position
				{
					position373 := position
					if !_rules[ruleAlphaNum]() {
						goto l371
					}
				l374:
					{
						position375, tokenIndex375 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l375
						}
						goto l374
					l375:
						position, tokenIndex = position375, tokenIndex375
					}
					add(rulePegText, position373)
				}
				if !_rules[ruleAction25]() {
					goto l371
				}
				add(ruleDeleteFilterColumn, position372)
			}
			return true
		l371:
			position, tokenIndex = position371, tokenIndex371
			return false
		},
		/* 61 DeleteFilterValue <- <(<AlphaNum+> Action26)> */
		func() bool {
			position376, tokenIndex376 := position, tokenIndex
			{
				position377 := position
				{
					position378 := position
					if !_rules[ruleAlphaNum]() {
						goto l376
					}
				l379:
					{
						position380, tokenIndex380 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l380
						}
						goto l379
					l380:
						position, tokenIndex = position380, tokenIndex380
					}
					add(rulePegText, position378)
				}
				if !_rules[ruleAction26]() {
					goto l376
				}
				add(ruleDeleteFilterValue, position377)
			}
			return true
		l376:
			position, tokenIndex = position376, tokenIndex376
			return false
		},
		/* 62 LIMIT <- <(('l' / 'L') ('i' / 'I') ('m' / 'M') ('i' / 'I') ('t' / 'T') _ LimitVal)> */
		func() bool {
			position381, tokenIndex381 := position, tokenIndex
			{
				position382 := position
				{
					position383, tokenIndex383 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l384
					}
					position++
					goto l383
				l384:
					position, tokenIndex = position383, tokenIndex383
					if buffer[position] != rune('L') {
						goto l381
					}
					position++
				}
			l383:
				{
					position385, tokenIndex385 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l386
					}
					position++
					goto l385
				l386:
					position, tokenIndex = position385, tokenIndex385
					if buffer[position] != rune('I') {
						goto l381
					}
					position++
				}
			l385:
				{
					position387, tokenIndex387 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l388
					}
					position++
					goto l387
				l388:
					position, tokenIndex = position387, tokenIndex387
					if buffer[position] != rune('M') {
						goto l381
					}
					position++
				}
			l387:
				{
					position389, tokenIndex389 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l390
					}
					position++
					goto l389
				l390:
					position, tokenIndex = position389, tokenIndex389
					if buffer[position] != rune('I') {
						goto l381
					}
					position++
				}
			l389:
				{
					position391, tokenIndex391 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l392
					}
					position++
					goto l391
				l392:
					position, tokenIndex = position391, tokenIndex391
					if buffer[position] != rune('T') {
						goto l381
					}
					position++
				}
			l391:
				if !_rules[rule_]() {
					goto l381
				}
				if !_rules[ruleLimitVal]() {
					goto l381
				}
				add(ruleLIMIT, position382)
			}
			return true
		l381:
			position, tokenIndex = position381, tokenIndex381
			return false
		},
		/* 63 LimitVal <- <(<Number+> Action27)> */
		func() bool {
			position393, tokenIndex393 := position, tokenIndex
			{
				position394 := position
				{
					position395 := position
					if !_rules[ruleNumber]() {
						goto l393
					}
				l396:
					{
						position397, tokenIndex397 := position, tokenIndex
						if !_rules[ruleNumber]() {
							goto l397
						}
						goto l396
					l397:
						position, tokenIndex = position397, tokenIndex397
					}
					add(rulePegText, position395)
				}
				if !_rules[ruleAction27]() {
					goto l393
				}
				add(ruleLimitVal, position394)
			}
			return true
		l393:
			position, tokenIndex = position393, tokenIndex393
			return false
		},
		/* 65 Action0 <- <{ p.validateInsert() }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 66 Action1 <- <{ p.setPartitionKey(buffer[begin:end])}> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		nil,
		/* 68 Action2 <- <{ p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 69 Action3 <- <{ p.InsertStatement.Columns = append(p.InsertStatement.Columns,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 70 Action4 <- <{ p.CreateStatement.Columns = append(p.CreateStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 71 Action5 <- <{ p.captureValues(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 72 Action6 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 73 Action7 <- <{p.SelectStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 74 Action8 <- <{ p.SelectStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 75 Action9 <- <{ p.InsertStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 76 Action10 <- <{ p.InsertStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 77 Action11 <- <{ p.CreateStatement.Keyspace =buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 78 Action12 <- <{ p.CreateStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 79 Action13 <- <{ p.DropStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 80 Action14 <- <{p.DropStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 81 Action15 <- <{p.DeleteStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
		/* 82 Action16 <- <{ p.DeleteStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction16, position)
			}
			return true
		},
		/* 83 Action17 <- <{ p.SelectStatement.Operators= append(p.SelectStatement.Operators, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction17, position)
			}
			return true
		},
		/* 84 Action18 <- <{ p.SelectStatement.Operators= append(p.SelectStatement.Operators, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction18, position)
			}
			return true
		},
		/* 85 Action19 <- <{ p.SelectStatement.Operators= append(p.SelectStatement.Operators, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction19, position)
			}
			return true
		},
		/* 86 Action20 <- <{ p.SelectStatement.WhereColumns = append(p.SelectStatement.WhereColumns, buffer[begin:end])  }> */
		func() bool {
			{
				add(ruleAction20, position)
			}
			return true
		},
		/* 87 Action21 <- <{ p.SelectStatement.WhereValues = append(p.SelectStatement.WhereValues, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction21, position)
			}
			return true
		},
		/* 88 Action22 <- <{ p.DeleteStatement.Operator=  buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction22, position)
			}
			return true
		},
		/* 89 Action23 <- <{ p.DeleteStatement.Operator=  buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction23, position)
			}
			return true
		},
		/* 90 Action24 <- <{ p.DeleteStatement.Operator=  buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction24, position)
			}
			return true
		},
		/* 91 Action25 <- <{ p.DeleteStatement.WhereColumn =  buffer[begin:end]  }> */
		func() bool {
			{
				add(ruleAction25, position)
			}
			return true
		},
		/* 92 Action26 <- <{ p.DeleteStatement.WhereValue =  buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction26, position)
			}
			return true
		},
		/* 93 Action27 <- <{ var err error;  p.SelectStatement.Limit,err = strconv.Atoi(buffer[begin:end]); if err!=nil{ p.SelectStatement.Limit = -1  };   }> */
		func() bool {
			{
				add(ruleAction27, position)
			}
			return true
		},
	}
	p.rules = _rules
}
