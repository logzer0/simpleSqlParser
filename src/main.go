package main

import (
	"fmt"
	"log"
)

func main() {
	expression := `Insert into tbl values ( 1, "a",1.123, "d"   );`
	calc := &SQL{Buffer: expression}
	calc.Init()
	if err := calc.Parse(); err != nil {
		panic(err)
		log.Fatal(err)
	}
	calc.Print()
	calc.Execute()
	fmt.Println(calc.Expression.InsertStatement)
}
