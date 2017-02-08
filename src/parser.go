package main

type Expression struct {
	Columns   []string
	TableName string
}

func (e *Expression) captureColName(val string) {
	e.Columns = append(e.Columns, val)
}
