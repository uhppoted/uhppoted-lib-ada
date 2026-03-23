package integration_tests

import (
	"bytes"
	"embed"
	"fmt"
	"log"
	"os"
	"text/template"
	"time"

	lib "github.com/uhppoted/uhppoted-codegen/model/types"

	"codegen/codegen"
	"codegen/model"
)

//go:embed templates/*
var templateFS embed.FS

type test struct {
	Name     string
	Function string
	Args     []any
	Request  []string
	Replies  []reply
	Returns  returns
}

type reply struct {
	Index int
	Reply []string
	Comma string
}

type returns struct {
	Type     string
	Template string
	Value    any
}

var translations = map[string]string{
	"find controllers response":       "Controller_Record_List",
	"get controller response":         "Controller_Record",
	"set IPv4 response":               "Boolean",
	"get time response":               "DateTime",
	"set time response":               "DateTime",
	"get listener response":           "Listener_Record",
	"set listener addr:port response": "Boolean",
	"get status response":             "Controller_Status",
	"get door response":               "Door_Record",
}

func IntegrationTests() {
	log.Println("   ... generating integration tests")

	templates := template.New("integration-tests")
	funcs := codegen.Functions

	funcs["render"] = func(name string, data any) (string, error) {
		return render(templates, name, data)
	}

	funcs["field"] = field
	funcs["value"] = value
	funcs["get"] = get
	funcs["gets"] = gets

	if templates, err := templates.Funcs(funcs).ParseFS(templateFS, "templates/*"); err != nil {
		log.Fatal(err)
	} else {
		functions := transmogrify(model.API)
		udp := transmogrify(model.API[1:])
		tcp := transmogrify(model.API[1:])

		requestsADS(templates, functions)
		repliesADS(templates, functions)
		messages(templates, functions)
		expected(templates, functions)
		defaultTests(templates, functions)
		udpTests(templates, udp)
		tcpTests(templates, tcp)
	}
}

func requestsADS(templates *template.Template, tests []test) {
	generate(templates, tests, "requests.ads", "../integration-tests/src/uhppoted-lib-integration_tests-stub-requests.ads")
}

func repliesADS(templates *template.Template, tests []test) {
	generate(templates, tests, "replies.ads", "../integration-tests/src/uhppoted-lib-integration_tests-stub-replies.ads")
}

func messages(templates *template.Template, tests []test) {
	generate(templates, tests, "messages.ads", "../integration-tests/src/uhppoted-lib-integration_tests-stub-messages.ads")
	generate(templates, tests, "messages.adb", "../integration-tests/src/uhppoted-lib-integration_tests-stub-messages.adb")
}

func expected(templates *template.Template, tests []test) {
	generate(templates, tests, "expected.ads", "../integration-tests/src/uhppoted-lib-integration_tests-expected.ads")
}

func defaultTests(templates *template.Template, tests []test) {
	generate(templates, tests, "default.ads", "../integration-tests/src/uhppoted-lib-integration_tests-default.ads")
	generate(templates, tests, "default.adb", "../integration-tests/src/uhppoted-lib-integration_tests-default.adb")
}

func udpTests(templates *template.Template, tests []test) {
	generate(templates, tests, "udp.ads", "../integration-tests/src/uhppoted-lib-integration_tests-udp.ads")
	generate(templates, tests, "udp.adb", "../integration-tests/src/uhppoted-lib-integration_tests-udp.adb")
}

func tcpTests(templates *template.Template, tests []test) {
	generate(templates, tests, "tcp.ads", "../integration-tests/src/uhppoted-lib-integration_tests-tcp.ads")
	generate(templates, tests, "tcp.adb", "../integration-tests/src/uhppoted-lib-integration_tests-tcp.adb")
}

func transmogrify(functions []lib.Function) []test {
	transmogrified := []test{}

	for _, f := range functions {
		for _, t := range f.Tests {
			transmogrified = append(transmogrified, test{
				Name:     codegen.AdaName(t.Name),
				Function: codegen.AdaName(f.Name),
				Args:     args(t),
				Request:  packet(t.Request),
				Replies:  replies(t),
				Returns:  response(f, t),
			})
		}
	}

	return transmogrified
}

func args(t lib.FuncTest) []any {
	args := []any{}

	for _, v := range t.Args {
		switch v.Type {
		case "IPv4":
			args = append(args, fmt.Sprintf(`Inet_Addr ("%v")`, v.Value))

		case "address:port":
			args = append(args, fmt.Sprintf(`(Family_Inet, Inet_Addr ("192.168.1.100"), 60001)`))

		case "datetime":
			args = append(args, datetime(v.Value))

		default:
			args = append(args, v.Value)
		}
	}

	return args
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

func response(f lib.Function, t lib.FuncTest) returns {
	r := returns{
		Type:     codegen.AdaName(f.Response.Name),
		Template: "unknown",
		Value:    "null",
	}

	if v, ok := translations[f.Response.Name]; ok {
		r.Type = v

		switch v {
		case "Boolean":
			r.Template = "boolean"
			r.Value = codegen.AdaValue(v, t.Replies[0].Response[1].Value)

		case "DateTime":
			r.Template = "datetime"
			r.Value = datetime(t.Replies[0].Response[1].Value)

		case "Controller_Record_List":
			r.Template = "controllers"
			r.Value = t.Replies

		case "Controller_Record":
			r.Template = "controller"
			r.Value = t.Replies[0].Response

		case "Listener_Record":
			r.Template = "listener"
			r.Value = t.Replies[0].Response

		case "Controller_Status":
			r.Template = "status"
			r.Value = t.Replies[0].Response

		case "Door_Record":
			r.Template = "door"
			r.Value = t.Replies[0].Response
		}
	}

	return r
}

func generate(templates *template.Template, tests []test, template string, file string) {
	if f, err := os.Create(file); err != nil {
		log.Fatalf("%v", err)
	} else {
		defer f.Close()

		var data = struct {
			Tests []test
		}{
			Tests: tests,
		}

		if err := templates.ExecuteTemplate(f, template, data); err != nil {
			log.Fatalf("%v", err)
		}

		log.Printf("... generated %s", file)
	}
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

func render(templates *template.Template, name string, data any) (string, error) {
	var buffer bytes.Buffer

	err := templates.ExecuteTemplate(&buffer, name, data)

	return buffer.String(), err
}

func datetime(v any) string {
	s := fmt.Sprintf("%v", v)
	if datetime, err := time.ParseInLocation("2006-01-02 15:04:05", s, time.Local); err != nil {
		panic(fmt.Sprintf("invalid date (%v)", v))
	} else {
		year, month, day := datetime.Date()

		return fmt.Sprintf(
			"(Year => %v, Month => %v, Day => %v, Hour => %v, Minute => %v, Second => %v)",
			uint16(year), uint8(month), uint8(day),
			uint8(datetime.Hour()), uint8(datetime.Minute()), uint8(datetime.Second()))
	}
}

func field(v string) string {
	switch v {
	case "controller":
		return "ID"

	case "ip address":
		return "Address"

	case "subnet mask":
		return "Netmask"

	case "gateway":
		return "Gateway"

	case "MAC Address", "MAC address":
		return "MAC"

	case "version":
		return "Firmware"

	default:
		return codegen.AdaName(v)
	}
}

func value(t string, v any) string {
	return codegen.AdaValue(t, v)
}

func get(values []lib.Value, key string) any {
	for _, v := range values {
		if v.Name == key {
			return value(v.Type, v.Value)
		}
	}

	panic(fmt.Sprintf("unknown field (%v)", key))
}

func gets(values []lib.Value, key string) string {
	for _, v := range values {
		if v.Name == key {
			return fmt.Sprintf("%v", v.Value)
		}
	}

	panic(fmt.Sprintf("unknown field (%v)", key))
}
