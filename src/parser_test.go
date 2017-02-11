package main

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSelect(t *testing.T) {
	tests := []struct {
		s    string
		stmt SelectStatement
	}{
		{
			s: `SELECT foo FROM tbl1;`,
			stmt: SelectStatement{
				TableName: "tbl1",
				Columns:   []string{"foo"},
			},
		},
		{
			s: `Select * from tbl;`,
			stmt: SelectStatement{
				TableName:  "tbl",
				AllColumns: true,
			},
		},
		{
			s: `Select (col1, col2  ,  col3 ) from tbl;`,
			stmt: SelectStatement{
				TableName: "tbl",
				Columns:   []string{"col1", "col2", "col3"},
			},
		},
	}

	for i, eachTest := range tests {
		got := &SQL{Buffer: eachTest.s}
		got.Init()
		if err := got.Parse(); assert.Nil(t, err) {
			got.Execute()
			assert.Equal(t, got.SelectStatement, eachTest.stmt, fmt.Sprint("Test Case %d ", i))
		}

	}

}

func TestInsert(t *testing.T) {
	tests := []struct {
		s    string
		stmt InsertStatement
	}{
		{
			s: `Insert into instaTbl values ( 1, "a",1.123, "d"   );`,
			stmt: InsertStatement{
				TableName: "instaTbl",
				Values:    []string{"1", "a", "1.123", "d"},
			},
		},
		{
			s: `INSERT INTO insertTable(a,b,c)values("a",23434, 23.533536);`,
			stmt: InsertStatement{
				TableName: "insertTable",
				Values:    []string{"a", "23434", "23.533536"},
				Columns:   []string{"a", "b", "c"},
			},
		},
		{
			s: `INSERT INTO kaboo(a,b,c) VALUES (1);`,
		},
	}

	for i, eachTest := range tests {
		got := &SQL{Buffer: eachTest.s}
		got.Init()
		if err := got.Parse(); assert.Nil(t, err) {
			got.Execute()
			assert.Equal(t, got.InsertStatement, eachTest.stmt, fmt.Sprintf("Test Case %d ", i))
		}
	}
}

func TestCreate(t *testing.T) {
	tests := []struct {
		s    string
		stmt CreateStatement
	}{
		{
			s: `Create Table TableName( col1 , col2, col3 );`,
			stmt: CreateStatement{
				TableName:       "TableName",
				Columns:         []string{"col1", "col2", "col3"},
				PartitioningKey: "col1",
			},
		},
	}

	for i, eachTest := range tests {
		got := &SQL{Buffer: eachTest.s}
		got.Init()
		if err := got.Parse(); assert.Nil(t, err) {
			got.Execute()
			assert.Equal(t, got.CreateStatement, eachTest.stmt, fmt.Sprint("Test Case %d ", i))
		}
	}

}
