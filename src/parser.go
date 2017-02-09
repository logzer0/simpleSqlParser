package main

import (
	"fmt"
	"log"
)

type Expression struct {
	sType           stmtType
	SelectStatement SelectStatement
	InsertStatement InsertStatement
}

type SelectStatement struct {
	Columns    []string
	TableName  string
	AllColumns bool
}

type InsertStatement struct {
	TableName string
	Columns   []string
	Values    []string
}

type stmtType int

const (
	Select stmtType = iota
	Insert
	Create
)

func (s *SQL) captureType(v string) {
	switch v {
	case "INSERT":
		fmt.Println("Captured an Insert")
		s.Expression.sType = Insert
	case "SELECT":
		fmt.Println("Captured a Select")
		s.Expression.sType = Select
	}
}

func (s *SQL) captureTableName(v string) {
	fmt.Println("The table name is ", v, "  ", s.Expression.sType)
	switch s.Expression.sType {
	case Select:
		s.SelectStatement.TableName = v
	case Insert:
		s.InsertStatement.TableName = v
	case Create:
		//To-do: Implement this
	default:
		log.Println("Error. This is not supported yet!")
	}
}

func (s *SQL) captureColumns(v string) {
	switch s.Expression.sType {
	case Select:
		s.SelectStatement.Columns = append(s.SelectStatement.Columns, v)
	case Insert:
		s.InsertStatement.Columns = append(s.InsertStatement.Columns, v)
	case Create:
		//To-do
	default:
		log.Println("Error. This is not supported yet!")
	}
}

func (s *SQL) captureValues(v string) {
	s.InsertStatement.Values = append(s.InsertStatement.Values, v)
}
