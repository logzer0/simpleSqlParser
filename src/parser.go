package main

type SelectStatement struct {
	Columns    []string
	TableName  string
	AllColumns bool
}
