package encode

import (
	_ "embed"
	"fmt"
	"log"
	"os"
	"text/template"

	lib "github.com/uhppoted/uhppoted-codegen/model/types"

	"codegen/codegen"
	"codegen/model"
)

//go:embed templates/uhppoted-lib-tests-encode.ads
var ads string

//go:embed templates/uhppoted-lib-tests-encode.adb
var adb string

type test struct {
	Name        string
	Description string
}

func UnitTests() {
	log.Println("   ... generating encode unit tests")

	tests := transmogrify(model.Requests)

	encodeTestsADS(tests)
	encodeTestsADB(tests)
}

func encodeTestsADS(tests []test) {
	const file = "../lib/tests/src/uhppoted-lib-tests-encode.ads"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		tmpl := template.Must(template.New("encode-tests").Funcs(codegen.Functions).Parse(ads))
		if err := tmpl.Execute(f, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func encodeTestsADB(tests []test) {
	const file = "../lib/tests/src/uhppoted-lib-tests-encode.adb"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		tmpl := template.Must(template.New("encode-tests").Funcs(codegen.Functions).Parse(adb))
		if err := tmpl.Execute(f, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func transmogrify(requests []lib.Request) []test {
	transmogrified := []test{}

	for _, rq := range requests {
		for _, t := range rq.Tests {
			transmogrified = append(transmogrified, test{
				Name:        fmt.Sprintf("Test_Encode_%v", codegen.AdaName(t.Name)),
				Description: fmt.Sprintf("Test encode %v request", codegen.AdaName(t.Name)),
			})
		}
	}

	return transmogrified
}
