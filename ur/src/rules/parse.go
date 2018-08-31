package rules

import (
	"bufio"
	"errors"
	"io"
	"os"
	"regexp"
	"strings"
)

// A rule could not be parsed.
var ParseErr = errors.New("parse error")

// Parse rules from a reader.
func Parse(r io.Reader) (rules []Rule, err error) {
	s := bufio.NewScanner(r)
	for s.Scan() {
		rule, err := parseRule(s.Text())
		if err != nil { return nil, err }
		rules = append(rules, rule)
	}
	return rules, s.Err()
}

// Parse rules from a file.
func ParseFile(path string) (rules []Rule, err error) {
	r, err := os.Open(path)
	if err != nil { return nil, err }
	defer r.Close()
	return Parse(r)
}

// Parse a single rule from a line.
func parseRule(l string) (Rule, error) {
	parts := strings.SplitN(l, " ", 2)
	if len(parts) != 2 { return Rule{}, ParseErr }

	pattern, err := regexp.Compile(parts[0])
	if err != nil { return Rule{}, err }

	rule := Rule{Pattern: pattern, Replacement: parts[1]}
	return rule, nil
}
