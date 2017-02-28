package simpleSqlParser

import "fmt"

type SelectStatement struct {
	Keyspace     string
	Columns      []string
	TableName    string
	AllColumns   bool
	WhereColumns []string
	WhereValues  []string
	Where        map[string]string
	Limit        int
}

type InsertStatement struct {
	Keyspace  string
	TableName string
	Columns   []string
	Values    []string
}

type CreateStatement struct {
	Keyspace        string
	TableName       string
	Columns         []string
	PartitioningKey string
}

type DropStatement struct {
	Keyspace  string
	TableName string
}

type sType int

const (
	Select sType = iota
	Insert
	Create
)

func (s *SQL) captureValues(v string) {
	s.InsertStatement.Values = append(s.InsertStatement.Values, v)
}

func (s *SQL) validateInsert() {
	s.sType = Insert
	if len(s.InsertStatement.Columns) < 1 {
		//We are good
	} else if len(s.InsertStatement.Columns) != len(s.InsertStatement.Values) {
		s.InsertStatement = InsertStatement{}
		fmt.Errorf(fmt.Sprintf("Columns and Values do not match. %d columns provided for %d values ", len(s.InsertStatement.Columns), len(s.InsertStatement.Values)))
	}

}

func (s *SQL) setPartitionKey(v string) {
	s.sType = Create
	s.CreateStatement.PartitioningKey = s.CreateStatement.Columns[0]
}
