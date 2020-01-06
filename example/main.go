package main

import "fmt"

#include "main.mc"

func main() {
	var item = Item(F, string) LEFT F: concat(n, a, m, e) RIGHT
	var F = get(Item(F, string), F, string)(item)
	fmt.Println(F)
}
