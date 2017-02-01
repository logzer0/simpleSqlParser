package main

import (
	"fmt"
	"strings"

	sql "github.com/benbjohnson/sql-parser"
)

func main() {
	foo := "select distinct  from tblasdas"
	stmt, err := sql.NewParser(strings.NewReader(foo)).Parse()
	fmt.Println(stmt, err)
}
