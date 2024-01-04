package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
	"strconv"
)

func ayoHandler(w http.ResponseWriter, r *http.Request) {
	log.Printf("ayo - we got an AYO: %s", r.URL.Path)
	var err error
	v, err := url.ParseQuery(r.URL.RawQuery)
	if err != nil {
		log.Fatalf("ok this ain't it: %s", r.URL.RawQuery)
		io.WriteString(
			w,
			"<div hx-get=\"/ayo\" hx-swap=outerHTML>oopsie</div>",
		)
		return
	}
	var c = 0
	c_str := v.Get("count")
	if c_str != "" {
		c, err = strconv.Atoi(c_str)
	}
	if err != nil {
		log.Fatalf("ok this ain't a int: %s", c_str)
		io.WriteString(
			w,
			"<div hx-get=\"/ayo\" hx-swap=outerHTML>oopsie</div>",
		)
		return
	}

	log.Printf("ayo - the AYO count is %d", c)
	fmt.Fprintf(
		w,
		"<div hx-get=\"/ayo?count=%d\" hx-swap=outerHTML><b>ayy lmao #%d</b></div>",
		c + 1,
		c,
	)
}

func indexHandler(w http.ResponseWriter, r *http.Request) {
	if len(r.URL.Path) > 1 {
		io.WriteString(w, "bruh")
		log.Printf(
			"we got a bruh over here of length %d - %s",
			len(r.URL.Path),
			r.URL.Path,
		)
		return
	}
	body, err := os.ReadFile("index.html")
	if err != nil {
		io.WriteString(w, "server bruh - can't find page")
		log.Fatalf("big bruh moment - no index.html")
		return
	}
	w.Write(body)
}

func main() {
	http.HandleFunc("/", indexHandler)
	http.HandleFunc("/ayo", ayoHandler)
	log.Printf("Starting up")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
