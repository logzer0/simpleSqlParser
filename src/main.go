package main

import (
	"fmt"
	"log"
)

func main() {
	expression := `Create Table CableName( col1, col2, col3 );`
	calc := &SQL{Buffer: expression}
	calc.Init()
	if err := calc.Parse(); err != nil {
		panic(err)
		log.Fatal(err)
	}
	calc.Print()
	calc.Execute()
	fmt.Println(calc.CreateStatement)
}
