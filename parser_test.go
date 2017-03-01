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
			s: `SELECT foo FROM k1.tbl1; `,
			stmt: SelectStatement{
				Keyspace:  "k1",
				TableName: "tbl1",
				Columns:   []string{"foo"},
			},
		},
		{
			s: `Select * from k2.tbl;`,
			stmt: SelectStatement{
				Keyspace:   "k2",
				TableName:  "tbl",
				AllColumns: true,
			},
		},
		{
			s: `Select (col1, col2  ,  col3 ) from k1.tbl;`,
			stmt: SelectStatement{
				Keyspace:  "k1",
				TableName: "tbl",
				Columns:   []string{"col1", "col2", "col3"},
			},
		},
		{
			s: `Select * from k1.tbl where x=10;`,
			stmt: SelectStatement{
				Keyspace:     "k1",
				TableName:    "tbl",
				AllColumns:   true,
				WhereColumns: []string{"x"},
				WhereValues:  []string{"10"},
				Where: map[string]string{
					"x": "10",
				},
				Operators: []string{"="},
			},
		},
		{
			s: `Select * from k1.tbl where x>10 and y=abc;`,
			stmt: SelectStatement{
				Keyspace:     "k1",
				TableName:    "tbl",
				AllColumns:   true,
				WhereColumns: []string{"x", "y"},
				WhereValues:  []string{"10", "abc"},
				Where: map[string]string{
					"x": "10",
					"y": "abc",
				},
				Operators: []string{">", "="},
			},
		},
		{
			s: `Select * from k1.tbl where x>10 and y < abc and z= 3.122 limit 5 ;`,
			stmt: SelectStatement{
				Keyspace:     "k1",
				TableName:    "tbl",
				AllColumns:   true,
				WhereColumns: []string{"x", "y", "z"},
				WhereValues:  []string{"10", "abc", "3.122"},
				Where: map[string]string{
					"x": "10",
					"y": "abc",
					"z": "3.122",
				},
				Operators: []string{">", "<", "="},
				Limit:     int(5),
			},
		},
	}

	for i, eachTest := range tests {
		got := &SQL{Buffer: eachTest.s}
		got.Init()
		if err := got.Parse(); assert.Nil(t, err, fmt.Sprintf("Test Case %d", i)) {
			got.Execute()
			assert.Equal(t, got.SelectStatement, eachTest.stmt, fmt.Sprintf("Test Case %d ", i))
		}

	}

}

func TestInsert(t *testing.T) {
	tests := []struct {
		s    string
		stmt InsertStatement
	}{
		{
			s: `Insert into k2.instaTbl values ( 1, "a",1.123, "d"   );`,
			stmt: InsertStatement{
				Keyspace:  "k2",
				TableName: "instaTbl",
				Values:    []string{"1", "a", "1.123", "d"},
			},
		},
		{
			s: `INSERT INTO k1.insertTable(a,b,c)values("a",23434, 23.533536);`,
			stmt: InsertStatement{
				Keyspace:  "k1",
				TableName: "insertTable",
				Values:    []string{"a", "23434", "23.533536"},
				Columns:   []string{"a", "b", "c"},
			},
		},
		{
			s: `INSERT INTO k1.kaboo(a,b,c) VALUES (1);`,
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
			s: ` Create Table keyspace.TableName( col1 , col2, col3 ); `,
			stmt: CreateStatement{
				Keyspace:        "keyspace",
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
