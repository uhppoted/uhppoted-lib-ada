package main

import (
	"fmt"
	"os"

	"codegen/decode"
	"codegen/encode"
)

var args = struct {
	encode bool
	decode bool
}{
	encode: false,
	decode: false,
}

func main() {
	fmt.Printf("\n--- uhppoted-lib-ada::codegen\n\n")

	if len(os.Args) > 1 {
		for _, arg := range os.Args[1:] {
			fmt.Printf(">>>> %v\n", arg)

			switch arg {
			case "--encode":
				args.encode = true

			case "--decode":
				args.decode = true

			case "--all":
				args.encode = true
				args.decode = true
			}
		}
	}

	if args.encode {
		encode.UnitTests()
	}

	if args.decode {
		decode.UnitTests()
	}
}
