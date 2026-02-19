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
	Reply       []string
}

func UnitTests() {
	log.Println("   ... generating decode unit tests")

	tests := transmogrify(model.Responses)

	decodeTestsADS(tests)
	decodeTestsADB(tests)
}

func decodeTestsADS(tests []test) {
	const file = "../lib/tests/src/uhppoted-lib-decode-tests.ads"

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
	const file = "../lib/tests/src/uhppoted-lib-decode-tests.adb"

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
				Reply:       packet(t.Response),
			})
		}
	}

	return transmogrified
}

func packet(p []byte) []string {
	format := "16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#,  16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#, 16#%02x#"

	return []string{
		fmt.Sprintf(format, p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15]) + ",",
		fmt.Sprintf(format, p[16], p[17], p[18], p[19], p[20], p[21], p[22], p[23], p[24], p[25], p[26], p[27], p[28], p[29], p[30], p[31]) + ",",
		fmt.Sprintf(format, p[32], p[33], p[34], p[35], p[36], p[37], p[38], p[39], p[40], p[41], p[42], p[43], p[44], p[45], p[46], p[47]) + ",",
		fmt.Sprintf(format, p[48], p[49], p[50], p[51], p[52], p[53], p[54], p[55], p[56], p[57], p[58], p[59], p[60], p[61], p[62], p[63]),
	}
}
