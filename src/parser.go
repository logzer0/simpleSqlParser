package main

type SelectStatement struct {
	Columns    []string
	TableName  string
	AllColumns bool
}

type InsertStatement struct {
	TableName        string
	ColumnsAndValues map[string]interface{}
	Values           interface{}
}
