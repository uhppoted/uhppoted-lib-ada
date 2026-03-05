package integration_tests

import (
	"embed"
	"fmt"
	"log"
	"os"
	"text/template"

	lib "github.com/uhppoted/uhppoted-codegen/model/types"

	"codegen/codegen"
	"codegen/model"
)

//go:embed templates/*
var templateFS embed.FS

type test struct {
	Name    string
	Request []string
	Replies []reply
}

type reply struct {
	Index int
	Reply []string
	Comma string
}

func IntegrationTests() {
	log.Println("   ... generating integration tests")

	functions := transmogrify(model.API)

	if templates, err := template.New("integration-tests").Funcs(codegen.Functions).ParseFS(templateFS, "templates/*"); err != nil {
		log.Fatal(err)
	} else {
		requestsADS(templates, functions)
		repliesADS(templates, functions)
		messagesADS(templates, functions)
		messagesADB(templates, functions)
	}
}

func requestsADS(templates *template.Template, tests []test) {
	const file = "../integration-tests/src/uhppoted-lib-integration_tests-stub-requests.ads"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		if err := templates.ExecuteTemplate(f, "requests.ads", data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func repliesADS(templates *template.Template, tests []test) {
	const file = "../integration-tests/src/uhppoted-lib-integration_tests-stub-replies.ads"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		if err := templates.ExecuteTemplate(f, "replies.ads", data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func messagesADS(templates *template.Template, tests []test) {
	const file = "../integration-tests/src/uhppoted-lib-integration_tests-stub-messages.ads"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		if err := templates.ExecuteTemplate(f, "messages.ads", data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func messagesADB(templates *template.Template, tests []test) {
	const file = "../integration-tests/src/uhppoted-lib-integration_tests-stub-messages.adb"

	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		if err := templates.ExecuteTemplate(f, "messages.adb", data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
}

func transmogrify(functions []lib.Function) []test {
	transmogrified := []test{}

	for _, f := range functions {
		for _, t := range f.Tests {
			transmogrified = append(transmogrified, test{
				Name:    fmt.Sprintf("%v", codegen.AdaName(t.Name)),
				Request: packet(t.Request),
				Replies: replies(t),
			})
		}
	}

	return transmogrified
}

func replies(t lib.FuncTest) []reply {
	list := []reply{}

	for ix, r := range t.Replies {
		if (ix + 1) <= len(t.Replies)-1 {
			list = append(list, reply{
				Index: ix + 1,
				Reply: packet(r.Message),
				Comma: ",",
			})
		} else {
			list = append(list, reply{
				Index: ix + 1,
				Reply: packet(r.Message),
			})
		}
	}

	return list
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
