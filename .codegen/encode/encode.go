package encode

import (
	_ "embed"
	"log"
	"os"
	"text/template"
	// lib "github.com/uhppoted/uhppoted-codegen/model/types"
)

//go:embed templates/uhppoted-lib-tests-encode.ads
var ads string

//go:embed templates/uhppoted-lib-tests-encode.adb
var adb string

func UnitTests() {
	log.Println("   ... generating encode unit tests")

	encodeTestsADS()
	encodeTestsADB()
}

func encodeTestsADS() {
	const file = "../lib/tests/src/uhppoted-lib-tests-encode.ads"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			// API       []*lib.Function
			// Responses []*lib.Response
		}{
			// API:       model.API,
			// Responses: model.Responses,
		}

		// tmpl := template.Must(template.New("encode").Funcs(codegen.Functions).Parse(ads))
		tmpl := template.Must(template.New("encode-tests").Parse(ads))
		if err := tmpl.Execute(f, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func encodeTestsADB() {
	const file = "../lib/tests/src/uhppoted-lib-tests-encode.adb"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			// API       []*lib.Function
			// Responses []*lib.Response
		}{
			// API:       model.API,
			// Responses: model.Responses,
		}

		// tmpl := template.Must(template.New("encode").Funcs(codegen.Functions).Parse(ads))
		tmpl := template.Must(template.New("encode-tests").Parse(adb))
		if err := tmpl.Execute(f, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}
