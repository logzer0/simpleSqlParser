package main

import (
	"fmt"
	"log"
)

func main() {
	expression := "SELECT (foo) FROM tbl;"
	calc := &SQL{Buffer: expression}
	calc.Init()
	if err := calc.Parse(); err != nil {
		panic(err)
		log.Fatal(err)
	}
	calc.Print()
	calc.Execute()
	fmt.Println(calc.Expression)
}
