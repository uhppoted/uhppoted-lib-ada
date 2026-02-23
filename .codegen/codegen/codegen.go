package codegen

import (
	"fmt"
	"regexp"
	"strings"
	"text/template"
	"unicode"
)

var Functions = template.FuncMap{
	"var":    AdaName,
	"args":   args,
	"rpad":   rpad,
	"record": record,
}

type KV struct {
	Name  string
	Value string
}

func AdaName(s string) string {
	tokens := regexp.MustCompile(`[ \-:]+`).Split(s, -1)

	for i, token := range tokens {
		tokens[i] = capitalize(token)
	}

	return strings.Join(tokens, "_")
}

func KebabCase(s string) string {
	tokens := regexp.MustCompile(`\s+`).Split(s, -1)

	for i, token := range tokens[1:] {
		tokens[i+1] = strings.ToLower(token)
	}

	return strings.Join(tokens, "-")
}

func capitalize(s string) string {
	runes := []rune(s)

	if len(runes) > 0 {
		runes[0] = unicode.ToUpper(runes[0])
	}

	if string(runes) == "Ip" {
		return "IP"
	}

	return string(runes)
}

func rpad(length int, s string) string {
	if len(s) >= length {
		return s
	}

	return s + strings.Repeat(" ", length-len(s))
}

func args(list []any) string {
	s := []string{}

	for _, v := range list {
		s = append(s, fmt.Sprintf("%v", v))
	}

	return strings.Join(s, ", ")
}

func record(fields []KV) string {
	s := []string{}
	w := 0

	for _, v := range fields {
		w = max(w, len(v.Name))
	}

	format := fmt.Sprintf("         %%-%vv => %%v", w)

	for _, v := range fields {
		s = append(s, fmt.Sprintf(format, v.Name, v.Value))
	}

	return fmt.Sprintf("(\n%v)", strings.Join(s, ",\n"))
}
