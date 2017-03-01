package main

import (
	"fmt"
	"log"
)

func main() {
	expression := `Select * from k1.tbl where x>10 and y <= abc;`
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
