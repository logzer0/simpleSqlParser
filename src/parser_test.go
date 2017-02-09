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
			s: `SELECT foo FROM tbl;`,
			stmt: SelectStatement{
				TableName: "tbl",
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
	s := `Insert into tbl values( 1, "a", 1.224  );`
	got := &SQL{Buffer: s}
	got.Init()
	if err := got.Parse(); assert.Nil(t, err) {
		got.Execute()
	}
}
