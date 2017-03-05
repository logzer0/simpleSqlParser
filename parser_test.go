package simpleSqlParser

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSelect(t *testing.T) {
	tests := []struct {
		s            string
		expectedStmt SelectStatement
	}{
		{
			s: `SELECT foo FROM k1.tbl1; `,
			expectedStmt: SelectStatement{
				Keyspace:  "k1",
				TableName: "tbl1",
				Columns:   []string{"foo"},
			},
		},
		{
			s: `Select * from k2.tbl;`,
			expectedStmt: SelectStatement{
				Keyspace:   "k2",
				TableName:  "tbl",
				AllColumns: true,
			},
		},
		{
			s: `Select (col1, col2  ,  col3 ) from k1.tbl;`,
			expectedStmt: SelectStatement{
				Keyspace:  "k1",
				TableName: "tbl",
				Columns:   []string{"col1", "col2", "col3"},
			},
		},
		{
			s: `Select * from k1.tbl where x=10;`,
			expectedStmt: SelectStatement{
				Keyspace:     "k1",
				TableName:    "tbl",
				AllColumns:   true,
				WhereColumns: []string{"x"},
				WhereValues:  []string{"10"},
				Operators:    []string{"="},
			},
		},
		{
			s: `Select * from k1.tbl where x>10 and y=abc;`,
			expectedStmt: SelectStatement{
				Keyspace:     "k1",
				TableName:    "tbl",
				AllColumns:   true,
				WhereColumns: []string{"x", "y"},
				WhereValues:  []string{"10", "abc"},
				Operators:    []string{">", "="},
			},
		},
		{
			s: `Select * from k1.tbl where x>=10 and y < abc and z= 3.122 limit 5 ;`,
			expectedStmt: SelectStatement{
				Keyspace:     "k1",
				TableName:    "tbl",
				AllColumns:   true,
				WhereColumns: []string{"x", "y", "z"},
				WhereValues:  []string{"10", "abc", "3.122"},
				Operators:    []string{">=", "<", "="},
				Limit:        int(5),
			},
		},
	}

	for i, eachTest := range tests {
		got := &SQL{Buffer: eachTest.s}
		got.Init()
		if err := got.Parse(); assert.Nil(t, err, fmt.Sprintf("Test Case %d", i)) {
			got.Execute()
			assert.Equal(t, eachTest.expectedStmt, got.SelectStatement, fmt.Sprintf("Test Case %d ", i))
		}

	}

}

func TestInsert(t *testing.T) {
	tests := []struct {
		s            string
		expectedStmt InsertStatement
	}{
		{
			s: `Insert into k2.instaTbl values ( 1, "a",1.123, "d"   );`,
			expectedStmt: InsertStatement{
				Keyspace:  "k2",
				TableName: "instaTbl",
				Values:    []string{"1", "a", "1.123", "d"},
			},
		},
		{
			s: `INSERT INTO k1.insertTable(a,b,c)values("a",23434, 23.533536);`,
			expectedStmt: InsertStatement{
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
			assert.Equal(t, eachTest.expectedStmt, got.InsertStatement, fmt.Sprintf("Test Case %d ", i))
		}
	}
}

func TestCreate(t *testing.T) {
	tests := []struct {
		s            string
		expectedStmt CreateStatement
	}{
		{
			s: ` Create Table keyspace.TableName( col1 , col2, col3 ); `,
			expectedStmt: CreateStatement{
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
			assert.Equal(t, got.CreateStatement, eachTest.expectedStmt, fmt.Sprint("Test Case %d ", i))
		}
	}

}
