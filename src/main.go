package main

import (
	"fmt"
	"log"
)

func main() {
	expression := "Select (col1,col2, col3) from tbl;"
	calc := &SQL{Buffer: expression}
	calc.Init()
	if err := calc.Parse(); err != nil {
		panic(err)
		log.Fatal(err)
	}
	calc.Print()
	calc.Execute()
	fmt.Println(calc.SelectStatement)
}
