package main

import (
	"fmt"
	"log"
)

func main() {
	expression := `INSERT INTO inserTable(a,b,c) VALUES (1);`
	calc := &SQL{Buffer: expression}
	calc.Init()
	if err := calc.Parse(); err != nil {
		panic(err)
		log.Fatal(err)
	}
	calc.Print()
	calc.Execute()
	fmt.Println(calc.InsertStatement)
}
