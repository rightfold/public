package main

import (
	"log"
	"net/http"
	"os"
	"ur/redirect"
	"ur/rules"
)

func main() {
	if len(os.Args) != 2 {
		log.Fatal("usage: urd rules.txt")
	}

	rules, err := rules.ParseFile(os.Args[1])
	if err != nil { log.Fatal(err) }

	handler := redirect.Redirect{rules}

	server := http.Server{
		Addr: "127.0.0.1:8000",
		Handler: &handler,
	}

	err = server.ListenAndServe()
	log.Fatal(err)
}
