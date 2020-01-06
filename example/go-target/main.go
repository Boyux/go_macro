package main

import "fmt"

func main() {
	var item = struct {
		prog string
	}{prog: "prog"}
	var prog = func(item struct {
		prog string
	}) string {
		return item.prog
	}(item)
	fmt.Println(prog)
}
