package codegen

import (
	"regexp"
	"strings"
	"text/template"
	"unicode"
)

var Functions = template.FuncMap{
	"rpad": rpad,
}

func AdaName(s string) string {
	tokens := regexp.MustCompile(`[ \-:]+`).Split(s, -1)

	for i, token := range tokens {
		tokens[i] = capitalize(token)
	}

	return strings.Join(tokens, "_")
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
