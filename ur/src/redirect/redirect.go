package redirect

import (
	"net/http"
	"ur/rules"
)

// An HTTP handler that redirects according to rules.
type Redirect struct {
	Rules []rules.Rule
}

func (r *Redirect) ServeHTTP(res http.ResponseWriter, req *http.Request) {
	target, ok := rules.Apply(r.Rules, req.URL.Path)
	if !ok {
		res.WriteHeader(http.StatusNotFound)
	} else {
		http.Redirect(res, req, target, http.StatusFound)
	}
}
