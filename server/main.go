package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func handler(w http.ResponseWriter, r *http.Request) {
	if len(r.URL.Path) > 1 {
		io.WriteString(w, "bruh")
		log.Default().Printf("we got a bruh over here of length %d - %s", len(r.URL.Path), r.URL.Path)
		return
	}
	body, err := os.ReadFile("index.html")
	if err != nil {
		io.WriteString(w, "server bruh - can't find page")
		log.Default().Fatalf("big bruh moment - no index.html")
		return
	}
	w.Write(body)
}

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe(":8080", nil))
}
