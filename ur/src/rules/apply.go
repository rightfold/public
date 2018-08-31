package rules

// Find the first rule that matches a path. If no rule matches the path, return
// nil.
func findMatching(rules []Rule, path string) *Rule {
	for _, rule := range rules {
		if rule.Pattern.MatchString(path) {
			return &rule
		}
	}
	return nil
}

// Find the first rule that matches a path, and apply it to the path. If no
// rule matches the path, false is returned.
func Apply(rules []Rule, path string) (string, bool) {
	rule := findMatching(rules, path)
	if rule == nil {
		return "", false
	} else {
		target := rule.Pattern.ReplaceAllString(path, rule.Replacement)
		return target, true
	}
}
