package rules

import (
	"errors"
	"io"
	"os"
	"regexp"
	"ur/encoding/mapping"
)

// A rule could not be parsed.
var ParseErr = errors.New("parse error")

// Parse rules from a reader.
func Parse(r io.Reader) (rules []Rule, err error) {
	mr := mapping.NewReader(r)
	for mr.Scan() {
		patternStr, replacement, err := mr.Mapping()
		if err != nil { return nil, err }

		pattern, err := regexp.Compile(patternStr)
		if err != nil { return nil, err }

		rule := Rule{Pattern: pattern, Replacement: replacement}
		rules = append(rules, rule)
	}
	return rules, mr.Err()
}

// Parse rules from a file.
func ParseFile(path string) (rules []Rule, err error) {
	r, err := os.Open(path)
	if err != nil { return nil, err }
	defer r.Close()
	return Parse(r)
}
