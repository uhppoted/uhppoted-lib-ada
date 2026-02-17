package codegen

import (
	"fmt"
	"regexp"
	"strings"
	"text/template"
	"unicode"
)

var Functions = template.FuncMap{
	"var":  AdaName,
	"args": args,
	"rpad": rpad,
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
