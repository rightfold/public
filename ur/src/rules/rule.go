package rules

import "regexp"

// A rule pairs a pattern with a replacement. Rules are applied to paths (as in
// URLs) and produce new paths. The replacement may reference groups in the
// pattern. Consult the documentation of the regexp package for the syntax of
// patterns and replacements.
type Rule struct {
	Pattern *regexp.Regexp
	Replacement string
}
