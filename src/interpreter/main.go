package main

import (
	"fmt"
	"sql"
	"strings"
)

func main() {
	foo := "select distinct  from tblasdas"
	stmt, err := sql.NewParser(strings.NewReader(foo)).Parse()
	fmt.Println(stmt, err)
}
