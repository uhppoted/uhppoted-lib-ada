package decode

import (
	_ "embed"
	"fmt"
	"log"
	"os"
	"strings"
	"text/template"

	lib "github.com/uhppoted/uhppoted-codegen/model/types"

	"codegen/codegen"
	"codegen/model"
)

//go:embed templates/uhppoted-lib-decode-tests.ads
var ads string

//go:embed templates/uhppoted-lib-decode-tests.adb
var adb string

type test struct {
	Name        string
	Description string
	Response    string
	// Expected    []string
}

func UnitTests() {
	log.Println("   ... generating decode unit tests")

	tests := transmogrify(model.Responses)

	decodeTestsADS(tests)
	decodeTestsADB(tests)
}

func decodeTestsADS(tests []test) {
	const file = "../lib/tests/src/uhppoted-lib-decode-tests-X.ads"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		tmpl := template.Must(template.New("decode-tests").Funcs(codegen.Functions).Parse(ads))
		if err := tmpl.Execute(f, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func decodeTestsADB(tests []test) {
	const file = "../lib/tests/src/uhppoted-lib-decode-tests-X.adb"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		tmpl := template.Must(template.New("decode-tests").Funcs(codegen.Functions).Parse(adb))
		if err := tmpl.Execute(f, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func transmogrify(responses []lib.Response) []test {
	transmogrified := []test{}

	for _, response := range responses {
		for _, t := range response.Tests {
			transmogrified = append(transmogrified, test{
				Name:        fmt.Sprintf("%v", codegen.AdaName(t.Name)),
				Description: fmt.Sprintf("Test decode %v response", codegen.AdaName(t.Name)),
				Response:    fmt.Sprintf("%v", codegen.KebabCase(strings.TrimSuffix(response.Name, " response"))),
			})
		}
	}

	return transmogrified
}
