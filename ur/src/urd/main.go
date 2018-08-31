package main

import (
	"log"
	"net/http"
	"regexp"
	"ur/redirect"
	"ur/rules"
)

func main() {
	rules := []rules.Rule{
		{regexp.MustCompile(`^/com/(\d+)$`), "https://example.com/$1"},
		{regexp.MustCompile(`^/org/(\d+)$`), "https://example.org/$1"},
	}

	handler := redirect.Redirect{rules}

	server := http.Server{
		Addr: "127.0.0.1:8000",
		Handler: &handler,
	}

	err := server.ListenAndServe()
	log.Fatal(err)
}
