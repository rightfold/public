package main

import (
	"log"
	"net/http"
	"os"
	"ur/redirect"
	"ur/rules"
	"ur/shorts"
)

func main() {
	if len(os.Args) != 3 {
		log.Fatal("usage: urd rules.txt shorts.json")
	}

	rules, err := rules.ParseFile(os.Args[1])
	if err != nil { log.Fatal(err) }

	shorts, err := shorts.Open(os.Args[2])
	if err != nil { log.Fatal(err) }
	defer shorts.Close()

	handler := redirect.Redirect{Rules: rules, Shorts: shorts}

	server := http.Server{
		Addr: "127.0.0.1:8000",
		Handler: &handler,
	}

	err = server.ListenAndServe()
	log.Fatal(err)
}
