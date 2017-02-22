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
	ruleDropStatement
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
	ruleAction11
	ruleAction12
	ruleAction13
	ruleAction14
	ruleAction15
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
	"DropStatement",
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
	"Action11",
	"Action12",
	"Action13",
	"Action14",
	"Action15",
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
	rules  [59]func() bool
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
			p.SelectStatement.Keyspace = buffer[begin:end]
		case ruleAction9:
			p.SelectStatement.TableName = buffer[begin:end]
		case ruleAction10:
			p.InsertStatement.Keyspace = buffer[begin:end]
		case ruleAction11:
			p.InsertStatement.TableName = buffer[begin:end]
		case ruleAction12:
			p.CreateStatement.Keyspace = buffer[begin:end]
		case ruleAction13:
			p.CreateStatement.TableName = buffer[begin:end]
		case ruleAction14:
			p.DropStatement.Keyspace = buffer[begin:end]
		case ruleAction15:
			p.DropStatement.TableName = buffer[begin:end]

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
		/* 1 Columns <- <(SelectParaColList / SelectColList / Asterisk)> */
		func() bool {
			position7, tokenIndex7 := position, tokenIndex
			{
				position8 := position
				{
					position9, tokenIndex9 := position, tokenIndex
					if !_rules[ruleSelectParaColList]() {
						goto l10
					}
					goto l9
				l10:
					position, tokenIndex = position9, tokenIndex9
					if !_rules[ruleSelectColList]() {
						goto l11
					}
					goto l9
				l11:
					position, tokenIndex = position9, tokenIndex9
					if !_rules[ruleAsterisk]() {
						goto l7
					}
				}
			l9:
				add(ruleColumns, position8)
			}
			return true
		l7:
			position, tokenIndex = position7, tokenIndex7
			return false
		},
		/* 2 SelectParaColList <- <('(' _ SelectColList _ ')')> */
		func() bool {
			position12, tokenIndex12 := position, tokenIndex
			{
				position13 := position
				if buffer[position] != rune('(') {
					goto l12
				}
				position++
				if !_rules[rule_]() {
					goto l12
				}
				if !_rules[ruleSelectColList]() {
					goto l12
				}
				if !_rules[rule_]() {
					goto l12
				}
				if buffer[position] != rune(')') {
					goto l12
				}
				position++
				add(ruleSelectParaColList, position13)
			}
			return true
		l12:
			position, tokenIndex = position12, tokenIndex12
			return false
		},
		/* 3 SelectColList <- <((SelectColName _ ',' _)* SelectColName)> */
		func() bool {
			position14, tokenIndex14 := position, tokenIndex
			{
				position15 := position
			l16:
				{
					position17, tokenIndex17 := position, tokenIndex
					if !_rules[ruleSelectColName]() {
						goto l17
					}
					if !_rules[rule_]() {
						goto l17
					}
					if buffer[position] != rune(',') {
						goto l17
					}
					position++
					if !_rules[rule_]() {
						goto l17
					}
					goto l16
				l17:
					position, tokenIndex = position17, tokenIndex17
				}
				if !_rules[ruleSelectColName]() {
					goto l14
				}
				add(ruleSelectColList, position15)
			}
			return true
		l14:
			position, tokenIndex = position14, tokenIndex14
			return false
		},
		/* 4 SelectColName <- <(<AlphaNum+> Action0)> */
		func() bool {
			position18, tokenIndex18 := position, tokenIndex
			{
				position19 := position
				{
					position20 := position
					if !_rules[ruleAlphaNum]() {
						goto l18
					}
				l21:
					{
						position22, tokenIndex22 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l22
						}
						goto l21
					l22:
						position, tokenIndex = position22, tokenIndex22
					}
					add(rulePegText, position20)
				}
				if !_rules[ruleAction0]() {
					goto l18
				}
				add(ruleSelectColName, position19)
			}
			return true
		l18:
			position, tokenIndex = position18, tokenIndex18
			return false
		},
		/* 5 InsertParaColList <- <('(' _ InsertColList _ ')')> */
		func() bool {
			position23, tokenIndex23 := position, tokenIndex
			{
				position24 := position
				if buffer[position] != rune('(') {
					goto l23
				}
				position++
				if !_rules[rule_]() {
					goto l23
				}
				if !_rules[ruleInsertColList]() {
					goto l23
				}
				if !_rules[rule_]() {
					goto l23
				}
				if buffer[position] != rune(')') {
					goto l23
				}
				position++
				add(ruleInsertParaColList, position24)
			}
			return true
		l23:
			position, tokenIndex = position23, tokenIndex23
			return false
		},
		/* 6 InsertColList <- <((_ InsertColName _ ',' _)* InsertColName)> */
		func() bool {
			position25, tokenIndex25 := position, tokenIndex
			{
				position26 := position
			l27:
				{
					position28, tokenIndex28 := position, tokenIndex
					if !_rules[rule_]() {
						goto l28
					}
					if !_rules[ruleInsertColName]() {
						goto l28
					}
					if !_rules[rule_]() {
						goto l28
					}
					if buffer[position] != rune(',') {
						goto l28
					}
					position++
					if !_rules[rule_]() {
						goto l28
					}
					goto l27
				l28:
					position, tokenIndex = position28, tokenIndex28
				}
				if !_rules[ruleInsertColName]() {
					goto l25
				}
				add(ruleInsertColList, position26)
			}
			return true
		l25:
			position, tokenIndex = position25, tokenIndex25
			return false
		},
		/* 7 InsertColName <- <(<AlphaNum+> Action1)> */
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
				if !_rules[ruleAction1]() {
					goto l29
				}
				add(ruleInsertColName, position30)
			}
			return true
		l29:
			position, tokenIndex = position29, tokenIndex29
			return false
		},
		/* 8 CreateParaColList <- <('(' _ CreateColList _ ')')> */
		func() bool {
			position34, tokenIndex34 := position, tokenIndex
			{
				position35 := position
				if buffer[position] != rune('(') {
					goto l34
				}
				position++
				if !_rules[rule_]() {
					goto l34
				}
				if !_rules[ruleCreateColList]() {
					goto l34
				}
				if !_rules[rule_]() {
					goto l34
				}
				if buffer[position] != rune(')') {
					goto l34
				}
				position++
				add(ruleCreateParaColList, position35)
			}
			return true
		l34:
			position, tokenIndex = position34, tokenIndex34
			return false
		},
		/* 9 CreateColList <- <((_ CreateColName _ ',' _)* CreateColName)> */
		func() bool {
			position36, tokenIndex36 := position, tokenIndex
			{
				position37 := position
			l38:
				{
					position39, tokenIndex39 := position, tokenIndex
					if !_rules[rule_]() {
						goto l39
					}
					if !_rules[ruleCreateColName]() {
						goto l39
					}
					if !_rules[rule_]() {
						goto l39
					}
					if buffer[position] != rune(',') {
						goto l39
					}
					position++
					if !_rules[rule_]() {
						goto l39
					}
					goto l38
				l39:
					position, tokenIndex = position39, tokenIndex39
				}
				if !_rules[ruleCreateColName]() {
					goto l36
				}
				add(ruleCreateColList, position37)
			}
			return true
		l36:
			position, tokenIndex = position36, tokenIndex36
			return false
		},
		/* 10 CreateColName <- <(<AlphaNum+> Action2)> */
		func() bool {
			position40, tokenIndex40 := position, tokenIndex
			{
				position41 := position
				{
					position42 := position
					if !_rules[ruleAlphaNum]() {
						goto l40
					}
				l43:
					{
						position44, tokenIndex44 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l44
						}
						goto l43
					l44:
						position, tokenIndex = position44, tokenIndex44
					}
					add(rulePegText, position42)
				}
				if !_rules[ruleAction2]() {
					goto l40
				}
				add(ruleCreateColName, position41)
			}
			return true
		l40:
			position, tokenIndex = position40, tokenIndex40
			return false
		},
		/* 11 DropStatement <- <(_ (('d' / 'D') ('r' / 'R') ('o' / 'O') ('p' / 'P')) _ DropTable _ ';' _)> */
		func() bool {
			position45, tokenIndex45 := position, tokenIndex
			{
				position46 := position
				if !_rules[rule_]() {
					goto l45
				}
				{
					position47, tokenIndex47 := position, tokenIndex
					if buffer[position] != rune('d') {
						goto l48
					}
					position++
					goto l47
				l48:
					position, tokenIndex = position47, tokenIndex47
					if buffer[position] != rune('D') {
						goto l45
					}
					position++
				}
			l47:
				{
					position49, tokenIndex49 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l50
					}
					position++
					goto l49
				l50:
					position, tokenIndex = position49, tokenIndex49
					if buffer[position] != rune('R') {
						goto l45
					}
					position++
				}
			l49:
				{
					position51, tokenIndex51 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l52
					}
					position++
					goto l51
				l52:
					position, tokenIndex = position51, tokenIndex51
					if buffer[position] != rune('O') {
						goto l45
					}
					position++
				}
			l51:
				{
					position53, tokenIndex53 := position, tokenIndex
					if buffer[position] != rune('p') {
						goto l54
					}
					position++
					goto l53
				l54:
					position, tokenIndex = position53, tokenIndex53
					if buffer[position] != rune('P') {
						goto l45
					}
					position++
				}
			l53:
				if !_rules[rule_]() {
					goto l45
				}
				if !_rules[ruleDropTable]() {
					goto l45
				}
				if !_rules[rule_]() {
					goto l45
				}
				if buffer[position] != rune(';') {
					goto l45
				}
				position++
				if !_rules[rule_]() {
					goto l45
				}
				add(ruleDropStatement, position46)
			}
			return true
		l45:
			position, tokenIndex = position45, tokenIndex45
			return false
		},
		/* 12 SelectStatement <- <(_ (('s' / 'S') ('e' / 'E') ('l' / 'L') ('e' / 'E') ('c' / 'C') ('t' / 'T')) _ Columns _ (('f' / 'F') ('r' / 'R') ('o' / 'O') ('m' / 'M')) _ SelectTable _ ';' _ Action3)> */
		func() bool {
			position55, tokenIndex55 := position, tokenIndex
			{
				position56 := position
				if !_rules[rule_]() {
					goto l55
				}
				{
					position57, tokenIndex57 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l58
					}
					position++
					goto l57
				l58:
					position, tokenIndex = position57, tokenIndex57
					if buffer[position] != rune('S') {
						goto l55
					}
					position++
				}
			l57:
				{
					position59, tokenIndex59 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l60
					}
					position++
					goto l59
				l60:
					position, tokenIndex = position59, tokenIndex59
					if buffer[position] != rune('E') {
						goto l55
					}
					position++
				}
			l59:
				{
					position61, tokenIndex61 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l62
					}
					position++
					goto l61
				l62:
					position, tokenIndex = position61, tokenIndex61
					if buffer[position] != rune('L') {
						goto l55
					}
					position++
				}
			l61:
				{
					position63, tokenIndex63 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l64
					}
					position++
					goto l63
				l64:
					position, tokenIndex = position63, tokenIndex63
					if buffer[position] != rune('E') {
						goto l55
					}
					position++
				}
			l63:
				{
					position65, tokenIndex65 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l66
					}
					position++
					goto l65
				l66:
					position, tokenIndex = position65, tokenIndex65
					if buffer[position] != rune('C') {
						goto l55
					}
					position++
				}
			l65:
				{
					position67, tokenIndex67 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l68
					}
					position++
					goto l67
				l68:
					position, tokenIndex = position67, tokenIndex67
					if buffer[position] != rune('T') {
						goto l55
					}
					position++
				}
			l67:
				if !_rules[rule_]() {
					goto l55
				}
				if !_rules[ruleColumns]() {
					goto l55
				}
				if !_rules[rule_]() {
					goto l55
				}
				{
					position69, tokenIndex69 := position, tokenIndex
					if buffer[position] != rune('f') {
						goto l70
					}
					position++
					goto l69
				l70:
					position, tokenIndex = position69, tokenIndex69
					if buffer[position] != rune('F') {
						goto l55
					}
					position++
				}
			l69:
				{
					position71, tokenIndex71 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l72
					}
					position++
					goto l71
				l72:
					position, tokenIndex = position71, tokenIndex71
					if buffer[position] != rune('R') {
						goto l55
					}
					position++
				}
			l71:
				{
					position73, tokenIndex73 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l74
					}
					position++
					goto l73
				l74:
					position, tokenIndex = position73, tokenIndex73
					if buffer[position] != rune('O') {
						goto l55
					}
					position++
				}
			l73:
				{
					position75, tokenIndex75 := position, tokenIndex
					if buffer[position] != rune('m') {
						goto l76
					}
					position++
					goto l75
				l76:
					position, tokenIndex = position75, tokenIndex75
					if buffer[position] != rune('M') {
						goto l55
					}
					position++
				}
			l75:
				if !_rules[rule_]() {
					goto l55
				}
				if !_rules[ruleSelectTable]() {
					goto l55
				}
				if !_rules[rule_]() {
					goto l55
				}
				if buffer[position] != rune(';') {
					goto l55
				}
				position++
				if !_rules[rule_]() {
					goto l55
				}
				if !_rules[ruleAction3]() {
					goto l55
				}
				add(ruleSelectStatement, position56)
			}
			return true
		l55:
			position, tokenIndex = position55, tokenIndex55
			return false
		},
		/* 13 InsertStatement <- <(_ (('i' / 'I') ('n' / 'N') ('s' / 'S') ('e' / 'E') ('r' / 'R') ('t' / 'T')) _ (('i' / 'I') ('n' / 'N') ('t' / 'T') ('o' / 'O')) _ InsertBody Action4)> */
		func() bool {
			position77, tokenIndex77 := position, tokenIndex
			{
				position78 := position
				if !_rules[rule_]() {
					goto l77
				}
				{
					position79, tokenIndex79 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l80
					}
					position++
					goto l79
				l80:
					position, tokenIndex = position79, tokenIndex79
					if buffer[position] != rune('I') {
						goto l77
					}
					position++
				}
			l79:
				{
					position81, tokenIndex81 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l82
					}
					position++
					goto l81
				l82:
					position, tokenIndex = position81, tokenIndex81
					if buffer[position] != rune('N') {
						goto l77
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
						goto l77
					}
					position++
				}
			l83:
				{
					position85, tokenIndex85 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l86
					}
					position++
					goto l85
				l86:
					position, tokenIndex = position85, tokenIndex85
					if buffer[position] != rune('E') {
						goto l77
					}
					position++
				}
			l85:
				{
					position87, tokenIndex87 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l88
					}
					position++
					goto l87
				l88:
					position, tokenIndex = position87, tokenIndex87
					if buffer[position] != rune('R') {
						goto l77
					}
					position++
				}
			l87:
				{
					position89, tokenIndex89 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l90
					}
					position++
					goto l89
				l90:
					position, tokenIndex = position89, tokenIndex89
					if buffer[position] != rune('T') {
						goto l77
					}
					position++
				}
			l89:
				if !_rules[rule_]() {
					goto l77
				}
				{
					position91, tokenIndex91 := position, tokenIndex
					if buffer[position] != rune('i') {
						goto l92
					}
					position++
					goto l91
				l92:
					position, tokenIndex = position91, tokenIndex91
					if buffer[position] != rune('I') {
						goto l77
					}
					position++
				}
			l91:
				{
					position93, tokenIndex93 := position, tokenIndex
					if buffer[position] != rune('n') {
						goto l94
					}
					position++
					goto l93
				l94:
					position, tokenIndex = position93, tokenIndex93
					if buffer[position] != rune('N') {
						goto l77
					}
					position++
				}
			l93:
				{
					position95, tokenIndex95 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l96
					}
					position++
					goto l95
				l96:
					position, tokenIndex = position95, tokenIndex95
					if buffer[position] != rune('T') {
						goto l77
					}
					position++
				}
			l95:
				{
					position97, tokenIndex97 := position, tokenIndex
					if buffer[position] != rune('o') {
						goto l98
					}
					position++
					goto l97
				l98:
					position, tokenIndex = position97, tokenIndex97
					if buffer[position] != rune('O') {
						goto l77
					}
					position++
				}
			l97:
				if !_rules[rule_]() {
					goto l77
				}
				if !_rules[ruleInsertBody]() {
					goto l77
				}
				if !_rules[ruleAction4]() {
					goto l77
				}
				add(ruleInsertStatement, position78)
			}
			return true
		l77:
			position, tokenIndex = position77, tokenIndex77
			return false
		},
		/* 14 CreateStatement <- <(_ (('c' / 'C') ('r' / 'R') ('e' / 'E') ('a' / 'A') ('t' / 'T') ('e' / 'E')) _ (('t' / 'T') ('a' / 'A') ('b' / 'B') ('l' / 'L') ('e' / 'E')) _ CreateTable _ CreateParaColList _ ';' Action5)> */
		func() bool {
			position99, tokenIndex99 := position, tokenIndex
			{
				position100 := position
				if !_rules[rule_]() {
					goto l99
				}
				{
					position101, tokenIndex101 := position, tokenIndex
					if buffer[position] != rune('c') {
						goto l102
					}
					position++
					goto l101
				l102:
					position, tokenIndex = position101, tokenIndex101
					if buffer[position] != rune('C') {
						goto l99
					}
					position++
				}
			l101:
				{
					position103, tokenIndex103 := position, tokenIndex
					if buffer[position] != rune('r') {
						goto l104
					}
					position++
					goto l103
				l104:
					position, tokenIndex = position103, tokenIndex103
					if buffer[position] != rune('R') {
						goto l99
					}
					position++
				}
			l103:
				{
					position105, tokenIndex105 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l106
					}
					position++
					goto l105
				l106:
					position, tokenIndex = position105, tokenIndex105
					if buffer[position] != rune('E') {
						goto l99
					}
					position++
				}
			l105:
				{
					position107, tokenIndex107 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l108
					}
					position++
					goto l107
				l108:
					position, tokenIndex = position107, tokenIndex107
					if buffer[position] != rune('A') {
						goto l99
					}
					position++
				}
			l107:
				{
					position109, tokenIndex109 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l110
					}
					position++
					goto l109
				l110:
					position, tokenIndex = position109, tokenIndex109
					if buffer[position] != rune('T') {
						goto l99
					}
					position++
				}
			l109:
				{
					position111, tokenIndex111 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l112
					}
					position++
					goto l111
				l112:
					position, tokenIndex = position111, tokenIndex111
					if buffer[position] != rune('E') {
						goto l99
					}
					position++
				}
			l111:
				if !_rules[rule_]() {
					goto l99
				}
				{
					position113, tokenIndex113 := position, tokenIndex
					if buffer[position] != rune('t') {
						goto l114
					}
					position++
					goto l113
				l114:
					position, tokenIndex = position113, tokenIndex113
					if buffer[position] != rune('T') {
						goto l99
					}
					position++
				}
			l113:
				{
					position115, tokenIndex115 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l116
					}
					position++
					goto l115
				l116:
					position, tokenIndex = position115, tokenIndex115
					if buffer[position] != rune('A') {
						goto l99
					}
					position++
				}
			l115:
				{
					position117, tokenIndex117 := position, tokenIndex
					if buffer[position] != rune('b') {
						goto l118
					}
					position++
					goto l117
				l118:
					position, tokenIndex = position117, tokenIndex117
					if buffer[position] != rune('B') {
						goto l99
					}
					position++
				}
			l117:
				{
					position119, tokenIndex119 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l120
					}
					position++
					goto l119
				l120:
					position, tokenIndex = position119, tokenIndex119
					if buffer[position] != rune('L') {
						goto l99
					}
					position++
				}
			l119:
				{
					position121, tokenIndex121 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l122
					}
					position++
					goto l121
				l122:
					position, tokenIndex = position121, tokenIndex121
					if buffer[position] != rune('E') {
						goto l99
					}
					position++
				}
			l121:
				if !_rules[rule_]() {
					goto l99
				}
				if !_rules[ruleCreateTable]() {
					goto l99
				}
				if !_rules[rule_]() {
					goto l99
				}
				if !_rules[ruleCreateParaColList]() {
					goto l99
				}
				if !_rules[rule_]() {
					goto l99
				}
				if buffer[position] != rune(';') {
					goto l99
				}
				position++
				if !_rules[ruleAction5]() {
					goto l99
				}
				add(ruleCreateStatement, position100)
			}
			return true
		l99:
			position, tokenIndex = position99, tokenIndex99
			return false
		},
		/* 15 InsertBody <- <(InsertTable _ Values _ ';')> */
		func() bool {
			position123, tokenIndex123 := position, tokenIndex
			{
				position124 := position
				if !_rules[ruleInsertTable]() {
					goto l123
				}
				if !_rules[rule_]() {
					goto l123
				}
				if !_rules[ruleValues]() {
					goto l123
				}
				if !_rules[rule_]() {
					goto l123
				}
				if buffer[position] != rune(';') {
					goto l123
				}
				position++
				add(ruleInsertBody, position124)
			}
			return true
		l123:
			position, tokenIndex = position123, tokenIndex123
			return false
		},
		/* 16 Values <- <(InsertParaColList? _ (('v' / 'V') ('a' / 'A') ('l' / 'L') ('u' / 'U') ('e' / 'E') ('s' / 'S')) _ ValuesBody)> */
		func() bool {
			position125, tokenIndex125 := position, tokenIndex
			{
				position126 := position
				{
					position127, tokenIndex127 := position, tokenIndex
					if !_rules[ruleInsertParaColList]() {
						goto l127
					}
					goto l128
				l127:
					position, tokenIndex = position127, tokenIndex127
				}
			l128:
				if !_rules[rule_]() {
					goto l125
				}
				{
					position129, tokenIndex129 := position, tokenIndex
					if buffer[position] != rune('v') {
						goto l130
					}
					position++
					goto l129
				l130:
					position, tokenIndex = position129, tokenIndex129
					if buffer[position] != rune('V') {
						goto l125
					}
					position++
				}
			l129:
				{
					position131, tokenIndex131 := position, tokenIndex
					if buffer[position] != rune('a') {
						goto l132
					}
					position++
					goto l131
				l132:
					position, tokenIndex = position131, tokenIndex131
					if buffer[position] != rune('A') {
						goto l125
					}
					position++
				}
			l131:
				{
					position133, tokenIndex133 := position, tokenIndex
					if buffer[position] != rune('l') {
						goto l134
					}
					position++
					goto l133
				l134:
					position, tokenIndex = position133, tokenIndex133
					if buffer[position] != rune('L') {
						goto l125
					}
					position++
				}
			l133:
				{
					position135, tokenIndex135 := position, tokenIndex
					if buffer[position] != rune('u') {
						goto l136
					}
					position++
					goto l135
				l136:
					position, tokenIndex = position135, tokenIndex135
					if buffer[position] != rune('U') {
						goto l125
					}
					position++
				}
			l135:
				{
					position137, tokenIndex137 := position, tokenIndex
					if buffer[position] != rune('e') {
						goto l138
					}
					position++
					goto l137
				l138:
					position, tokenIndex = position137, tokenIndex137
					if buffer[position] != rune('E') {
						goto l125
					}
					position++
				}
			l137:
				{
					position139, tokenIndex139 := position, tokenIndex
					if buffer[position] != rune('s') {
						goto l140
					}
					position++
					goto l139
				l140:
					position, tokenIndex = position139, tokenIndex139
					if buffer[position] != rune('S') {
						goto l125
					}
					position++
				}
			l139:
				if !_rules[rule_]() {
					goto l125
				}
				if !_rules[ruleValuesBody]() {
					goto l125
				}
				add(ruleValues, position126)
			}
			return true
		l125:
			position, tokenIndex = position125, tokenIndex125
			return false
		},
		/* 17 ValuesBody <- <('(' _ ValList _ ')')> */
		func() bool {
			position141, tokenIndex141 := position, tokenIndex
			{
				position142 := position
				if buffer[position] != rune('(') {
					goto l141
				}
				position++
				if !_rules[rule_]() {
					goto l141
				}
				if !_rules[ruleValList]() {
					goto l141
				}
				if !_rules[rule_]() {
					goto l141
				}
				if buffer[position] != rune(')') {
					goto l141
				}
				position++
				add(ruleValuesBody, position142)
			}
			return true
		l141:
			position, tokenIndex = position141, tokenIndex141
			return false
		},
		/* 18 ValList <- <((_ InsertValue _ ',' _)* _ InsertValue)> */
		func() bool {
			position143, tokenIndex143 := position, tokenIndex
			{
				position144 := position
			l145:
				{
					position146, tokenIndex146 := position, tokenIndex
					if !_rules[rule_]() {
						goto l146
					}
					if !_rules[ruleInsertValue]() {
						goto l146
					}
					if !_rules[rule_]() {
						goto l146
					}
					if buffer[position] != rune(',') {
						goto l146
					}
					position++
					if !_rules[rule_]() {
						goto l146
					}
					goto l145
				l146:
					position, tokenIndex = position146, tokenIndex146
				}
				if !_rules[rule_]() {
					goto l143
				}
				if !_rules[ruleInsertValue]() {
					goto l143
				}
				add(ruleValList, position144)
			}
			return true
		l143:
			position, tokenIndex = position143, tokenIndex143
			return false
		},
		/* 19 InsertValue <- <('"'? EachValue '"'?)> */
		func() bool {
			position147, tokenIndex147 := position, tokenIndex
			{
				position148 := position
				{
					position149, tokenIndex149 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l149
					}
					position++
					goto l150
				l149:
					position, tokenIndex = position149, tokenIndex149
				}
			l150:
				if !_rules[ruleEachValue]() {
					goto l147
				}
				{
					position151, tokenIndex151 := position, tokenIndex
					if buffer[position] != rune('"') {
						goto l151
					}
					position++
					goto l152
				l151:
					position, tokenIndex = position151, tokenIndex151
				}
			l152:
				add(ruleInsertValue, position148)
			}
			return true
		l147:
			position, tokenIndex = position147, tokenIndex147
			return false
		},
		/* 20 EachValue <- <(<AlphaNum+> Action6)> */
		func() bool {
			position153, tokenIndex153 := position, tokenIndex
			{
				position154 := position
				{
					position155 := position
					if !_rules[ruleAlphaNum]() {
						goto l153
					}
				l156:
					{
						position157, tokenIndex157 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l157
						}
						goto l156
					l157:
						position, tokenIndex = position157, tokenIndex157
					}
					add(rulePegText, position155)
				}
				if !_rules[ruleAction6]() {
					goto l153
				}
				add(ruleEachValue, position154)
			}
			return true
		l153:
			position, tokenIndex = position153, tokenIndex153
			return false
		},
		/* 21 Asterisk <- <('*' Action7)> */
		func() bool {
			position158, tokenIndex158 := position, tokenIndex
			{
				position159 := position
				if buffer[position] != rune('*') {
					goto l158
				}
				position++
				if !_rules[ruleAction7]() {
					goto l158
				}
				add(ruleAsterisk, position159)
			}
			return true
		l158:
			position, tokenIndex = position158, tokenIndex158
			return false
		},
		/* 22 lineComment <- <('/' '/' (!'\n' .)*)> */
		func() bool {
			position160, tokenIndex160 := position, tokenIndex
			{
				position161 := position
				if buffer[position] != rune('/') {
					goto l160
				}
				position++
				if buffer[position] != rune('/') {
					goto l160
				}
				position++
			l162:
				{
					position163, tokenIndex163 := position, tokenIndex
					{
						position164, tokenIndex164 := position, tokenIndex
						if buffer[position] != rune('\n') {
							goto l164
						}
						position++
						goto l163
					l164:
						position, tokenIndex = position164, tokenIndex164
					}
					if !matchDot() {
						goto l163
					}
					goto l162
				l163:
					position, tokenIndex = position163, tokenIndex163
				}
				add(rulelineComment, position161)
			}
			return true
		l160:
			position, tokenIndex = position160, tokenIndex160
			return false
		},
		/* 23 blockComment <- <('/' '*' (!('*' '/') (. / '\n'))* ('*' '/'))> */
		func() bool {
			position165, tokenIndex165 := position, tokenIndex
			{
				position166 := position
				if buffer[position] != rune('/') {
					goto l165
				}
				position++
				if buffer[position] != rune('*') {
					goto l165
				}
				position++
			l167:
				{
					position168, tokenIndex168 := position, tokenIndex
					{
						position169, tokenIndex169 := position, tokenIndex
						if buffer[position] != rune('*') {
							goto l169
						}
						position++
						if buffer[position] != rune('/') {
							goto l169
						}
						position++
						goto l168
					l169:
						position, tokenIndex = position169, tokenIndex169
					}
					{
						position170, tokenIndex170 := position, tokenIndex
						if !matchDot() {
							goto l171
						}
						goto l170
					l171:
						position, tokenIndex = position170, tokenIndex170
						if buffer[position] != rune('\n') {
							goto l168
						}
						position++
					}
				l170:
					goto l167
				l168:
					position, tokenIndex = position168, tokenIndex168
				}
				if buffer[position] != rune('*') {
					goto l165
				}
				position++
				if buffer[position] != rune('/') {
					goto l165
				}
				position++
				add(ruleblockComment, position166)
			}
			return true
		l165:
			position, tokenIndex = position165, tokenIndex165
			return false
		},
		/* 24 ws <- <(' ' / '\t' / '\n' / '\r')> */
		func() bool {
			position172, tokenIndex172 := position, tokenIndex
			{
				position173 := position
				{
					position174, tokenIndex174 := position, tokenIndex
					if buffer[position] != rune(' ') {
						goto l175
					}
					position++
					goto l174
				l175:
					position, tokenIndex = position174, tokenIndex174
					if buffer[position] != rune('\t') {
						goto l176
					}
					position++
					goto l174
				l176:
					position, tokenIndex = position174, tokenIndex174
					if buffer[position] != rune('\n') {
						goto l177
					}
					position++
					goto l174
				l177:
					position, tokenIndex = position174, tokenIndex174
					if buffer[position] != rune('\r') {
						goto l172
					}
					position++
				}
			l174:
				add(rulews, position173)
			}
			return true
		l172:
			position, tokenIndex = position172, tokenIndex172
			return false
		},
		/* 25 _ <- <(ws / lineComment / blockComment)*> */
		func() bool {
			{
				position179 := position
			l180:
				{
					position181, tokenIndex181 := position, tokenIndex
					{
						position182, tokenIndex182 := position, tokenIndex
						if !_rules[rulews]() {
							goto l183
						}
						goto l182
					l183:
						position, tokenIndex = position182, tokenIndex182
						if !_rules[rulelineComment]() {
							goto l184
						}
						goto l182
					l184:
						position, tokenIndex = position182, tokenIndex182
						if !_rules[ruleblockComment]() {
							goto l181
						}
					}
				l182:
					goto l180
				l181:
					position, tokenIndex = position181, tokenIndex181
				}
				add(rule_, position179)
			}
			return true
		},
		/* 26 Letter <- <([a-z] / [A-Z] / '_')> */
		func() bool {
			position185, tokenIndex185 := position, tokenIndex
			{
				position186 := position
				{
					position187, tokenIndex187 := position, tokenIndex
					if c := buffer[position]; c < rune('a') || c > rune('z') {
						goto l188
					}
					position++
					goto l187
				l188:
					position, tokenIndex = position187, tokenIndex187
					if c := buffer[position]; c < rune('A') || c > rune('Z') {
						goto l189
					}
					position++
					goto l187
				l189:
					position, tokenIndex = position187, tokenIndex187
					if buffer[position] != rune('_') {
						goto l185
					}
					position++
				}
			l187:
				add(ruleLetter, position186)
			}
			return true
		l185:
			position, tokenIndex = position185, tokenIndex185
			return false
		},
		/* 27 Number <- <([0-9] ('.' [0-9])*)> */
		func() bool {
			position190, tokenIndex190 := position, tokenIndex
			{
				position191 := position
				if c := buffer[position]; c < rune('0') || c > rune('9') {
					goto l190
				}
				position++
			l192:
				{
					position193, tokenIndex193 := position, tokenIndex
					if buffer[position] != rune('.') {
						goto l193
					}
					position++
					if c := buffer[position]; c < rune('0') || c > rune('9') {
						goto l193
					}
					position++
					goto l192
				l193:
					position, tokenIndex = position193, tokenIndex193
				}
				add(ruleNumber, position191)
			}
			return true
		l190:
			position, tokenIndex = position190, tokenIndex190
			return false
		},
		/* 28 AlphaNum <- <(Letter / Number)> */
		func() bool {
			position194, tokenIndex194 := position, tokenIndex
			{
				position195 := position
				{
					position196, tokenIndex196 := position, tokenIndex
					if !_rules[ruleLetter]() {
						goto l197
					}
					goto l196
				l197:
					position, tokenIndex = position196, tokenIndex196
					if !_rules[ruleNumber]() {
						goto l194
					}
				}
			l196:
				add(ruleAlphaNum, position195)
			}
			return true
		l194:
			position, tokenIndex = position194, tokenIndex194
			return false
		},
		/* 29 SelectTable <- <(SelectKeyspace '.' SelectTableName)> */
		func() bool {
			position198, tokenIndex198 := position, tokenIndex
			{
				position199 := position
				if !_rules[ruleSelectKeyspace]() {
					goto l198
				}
				if buffer[position] != rune('.') {
					goto l198
				}
				position++
				if !_rules[ruleSelectTableName]() {
					goto l198
				}
				add(ruleSelectTable, position199)
			}
			return true
		l198:
			position, tokenIndex = position198, tokenIndex198
			return false
		},
		/* 30 SelectKeyspace <- <(<AlphaNum+> Action8)> */
		func() bool {
			position200, tokenIndex200 := position, tokenIndex
			{
				position201 := position
				{
					position202 := position
					if !_rules[ruleAlphaNum]() {
						goto l200
					}
				l203:
					{
						position204, tokenIndex204 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l204
						}
						goto l203
					l204:
						position, tokenIndex = position204, tokenIndex204
					}
					add(rulePegText, position202)
				}
				if !_rules[ruleAction8]() {
					goto l200
				}
				add(ruleSelectKeyspace, position201)
			}
			return true
		l200:
			position, tokenIndex = position200, tokenIndex200
			return false
		},
		/* 31 SelectTableName <- <(<AlphaNum+> Action9)> */
		func() bool {
			position205, tokenIndex205 := position, tokenIndex
			{
				position206 := position
				{
					position207 := position
					if !_rules[ruleAlphaNum]() {
						goto l205
					}
				l208:
					{
						position209, tokenIndex209 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l209
						}
						goto l208
					l209:
						position, tokenIndex = position209, tokenIndex209
					}
					add(rulePegText, position207)
				}
				if !_rules[ruleAction9]() {
					goto l205
				}
				add(ruleSelectTableName, position206)
			}
			return true
		l205:
			position, tokenIndex = position205, tokenIndex205
			return false
		},
		/* 32 InsertTable <- <(InsertKeyspace '.' InsertTableName)> */
		func() bool {
			position210, tokenIndex210 := position, tokenIndex
			{
				position211 := position
				if !_rules[ruleInsertKeyspace]() {
					goto l210
				}
				if buffer[position] != rune('.') {
					goto l210
				}
				position++
				if !_rules[ruleInsertTableName]() {
					goto l210
				}
				add(ruleInsertTable, position211)
			}
			return true
		l210:
			position, tokenIndex = position210, tokenIndex210
			return false
		},
		/* 33 InsertKeyspace <- <(<AlphaNum+> Action10)> */
		func() bool {
			position212, tokenIndex212 := position, tokenIndex
			{
				position213 := position
				{
					position214 := position
					if !_rules[ruleAlphaNum]() {
						goto l212
					}
				l215:
					{
						position216, tokenIndex216 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l216
						}
						goto l215
					l216:
						position, tokenIndex = position216, tokenIndex216
					}
					add(rulePegText, position214)
				}
				if !_rules[ruleAction10]() {
					goto l212
				}
				add(ruleInsertKeyspace, position213)
			}
			return true
		l212:
			position, tokenIndex = position212, tokenIndex212
			return false
		},
		/* 34 InsertTableName <- <(<AlphaNum+> Action11)> */
		func() bool {
			position217, tokenIndex217 := position, tokenIndex
			{
				position218 := position
				{
					position219 := position
					if !_rules[ruleAlphaNum]() {
						goto l217
					}
				l220:
					{
						position221, tokenIndex221 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l221
						}
						goto l220
					l221:
						position, tokenIndex = position221, tokenIndex221
					}
					add(rulePegText, position219)
				}
				if !_rules[ruleAction11]() {
					goto l217
				}
				add(ruleInsertTableName, position218)
			}
			return true
		l217:
			position, tokenIndex = position217, tokenIndex217
			return false
		},
		/* 35 CreateTable <- <(CreateKeyspace '.' CreateTableName)> */
		func() bool {
			position222, tokenIndex222 := position, tokenIndex
			{
				position223 := position
				if !_rules[ruleCreateKeyspace]() {
					goto l222
				}
				if buffer[position] != rune('.') {
					goto l222
				}
				position++
				if !_rules[ruleCreateTableName]() {
					goto l222
				}
				add(ruleCreateTable, position223)
			}
			return true
		l222:
			position, tokenIndex = position222, tokenIndex222
			return false
		},
		/* 36 CreateKeyspace <- <(<AlphaNum+> Action12)> */
		func() bool {
			position224, tokenIndex224 := position, tokenIndex
			{
				position225 := position
				{
					position226 := position
					if !_rules[ruleAlphaNum]() {
						goto l224
					}
				l227:
					{
						position228, tokenIndex228 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l228
						}
						goto l227
					l228:
						position, tokenIndex = position228, tokenIndex228
					}
					add(rulePegText, position226)
				}
				if !_rules[ruleAction12]() {
					goto l224
				}
				add(ruleCreateKeyspace, position225)
			}
			return true
		l224:
			position, tokenIndex = position224, tokenIndex224
			return false
		},
		/* 37 CreateTableName <- <(<AlphaNum+> Action13)> */
		func() bool {
			position229, tokenIndex229 := position, tokenIndex
			{
				position230 := position
				{
					position231 := position
					if !_rules[ruleAlphaNum]() {
						goto l229
					}
				l232:
					{
						position233, tokenIndex233 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l233
						}
						goto l232
					l233:
						position, tokenIndex = position233, tokenIndex233
					}
					add(rulePegText, position231)
				}
				if !_rules[ruleAction13]() {
					goto l229
				}
				add(ruleCreateTableName, position230)
			}
			return true
		l229:
			position, tokenIndex = position229, tokenIndex229
			return false
		},
		/* 38 DropTable <- <(DropTableKeyspace '.' DropTableName)> */
		func() bool {
			position234, tokenIndex234 := position, tokenIndex
			{
				position235 := position
				if !_rules[ruleDropTableKeyspace]() {
					goto l234
				}
				if buffer[position] != rune('.') {
					goto l234
				}
				position++
				if !_rules[ruleDropTableName]() {
					goto l234
				}
				add(ruleDropTable, position235)
			}
			return true
		l234:
			position, tokenIndex = position234, tokenIndex234
			return false
		},
		/* 39 DropTableKeyspace <- <(<AlphaNum+> Action14)> */
		func() bool {
			position236, tokenIndex236 := position, tokenIndex
			{
				position237 := position
				{
					position238 := position
					if !_rules[ruleAlphaNum]() {
						goto l236
					}
				l239:
					{
						position240, tokenIndex240 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l240
						}
						goto l239
					l240:
						position, tokenIndex = position240, tokenIndex240
					}
					add(rulePegText, position238)
				}
				if !_rules[ruleAction14]() {
					goto l236
				}
				add(ruleDropTableKeyspace, position237)
			}
			return true
		l236:
			position, tokenIndex = position236, tokenIndex236
			return false
		},
		/* 40 DropTableName <- <(<AlphaNum+> Action15)> */
		func() bool {
			position241, tokenIndex241 := position, tokenIndex
			{
				position242 := position
				{
					position243 := position
					if !_rules[ruleAlphaNum]() {
						goto l241
					}
				l244:
					{
						position245, tokenIndex245 := position, tokenIndex
						if !_rules[ruleAlphaNum]() {
							goto l245
						}
						goto l244
					l245:
						position, tokenIndex = position245, tokenIndex245
					}
					add(rulePegText, position243)
				}
				if !_rules[ruleAction15]() {
					goto l241
				}
				add(ruleDropTableName, position242)
			}
			return true
		l241:
			position, tokenIndex = position241, tokenIndex241
			return false
		},
		nil,
		/* 43 Action0 <- <{ p.SelectStatement.Columns = append(p.SelectStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction0, position)
			}
			return true
		},
		/* 44 Action1 <- <{ p.InsertStatement.Columns = append(p.InsertStatement.Columns,buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction1, position)
			}
			return true
		},
		/* 45 Action2 <- <{ p.CreateStatement.Columns = append(p.CreateStatement.Columns, buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction2, position)
			}
			return true
		},
		/* 46 Action3 <- <{ p.sType = Select  }> */
		func() bool {
			{
				add(ruleAction3, position)
			}
			return true
		},
		/* 47 Action4 <- <{ p.validateInsert() }> */
		func() bool {
			{
				add(ruleAction4, position)
			}
			return true
		},
		/* 48 Action5 <- <{ p.setPartitionKey(buffer[begin:end])}> */
		func() bool {
			{
				add(ruleAction5, position)
			}
			return true
		},
		/* 49 Action6 <- <{ p.captureValues(buffer[begin:end]) }> */
		func() bool {
			{
				add(ruleAction6, position)
			}
			return true
		},
		/* 50 Action7 <- <{ p.SelectStatement.AllColumns = true }> */
		func() bool {
			{
				add(ruleAction7, position)
			}
			return true
		},
		/* 51 Action8 <- <{p.SelectStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction8, position)
			}
			return true
		},
		/* 52 Action9 <- <{ p.SelectStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction9, position)
			}
			return true
		},
		/* 53 Action10 <- <{ p.InsertStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction10, position)
			}
			return true
		},
		/* 54 Action11 <- <{ p.InsertStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction11, position)
			}
			return true
		},
		/* 55 Action12 <- <{ p.CreateStatement.Keyspace =buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction12, position)
			}
			return true
		},
		/* 56 Action13 <- <{ p.CreateStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction13, position)
			}
			return true
		},
		/* 57 Action14 <- <{ p.DropStatement.Keyspace = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction14, position)
			}
			return true
		},
		/* 58 Action15 <- <{p.DropStatement.TableName = buffer[begin:end] }> */
		func() bool {
			{
				add(ruleAction15, position)
			}
			return true
		},
	}
	p.rules = _rules
}
