package redirect

import (
	"log"
	"net/http"
	"strings"
	"ur/rules"
	"ur/shorts"
)

// An HTTP handler that redirects according to rules.
type Redirect struct {
	Rules []rules.Rule
	Shorts *shorts.Database
}

func (r *Redirect) ServeHTTP(res http.ResponseWriter, req *http.Request) {
	path := req.URL.Path
	if strings.HasPrefix(path, "/s/") {
		short := path[3:]
		r.serveShort(res, req, short)
	} else {
		r.serveRule(res, req, path)
	}
}

func (r *Redirect) serveShort(res http.ResponseWriter, req *http.Request, short string) {
	redirect, err := r.Shorts.Lookup(short)

	if err == shorts.UnknownShortErr {
		http.NotFound(res, req)
		return
	}

	if err != nil {
		log.Print(err)
		return
	}

	http.Redirect(res, req, redirect, http.StatusFound)
}

func (r *Redirect) serveRule(res http.ResponseWriter, req *http.Request, path string) {
	redirect, ok := rules.Apply(r.Rules, path)
	if !ok {
		http.NotFound(res, req)
	} else {
		http.Redirect(res, req, redirect, http.StatusFound)
	}
}
