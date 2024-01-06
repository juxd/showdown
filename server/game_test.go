package main

import (
	"math/rand"
	"testing"
)

func TestGenerateRandomCoordinates(t *testing.T) {
	r := rand.New(rand.NewSource(69))
	res := generate7RandomCoordinates(r)

	var seen [7]Coord
	for i, c := range res {
		if c.X < 0 || c.X > 7 || c.Y < 0 || c.Y > 7 {
			t.Fatalf("%v Coordinates are out of range", c)
		}
		for j, s := range seen {
			if j < i && s.X == c.X && s.Y == c.Y {
				t.Fatalf("repeated coordinates %d %d", i, j)
			}
		}
		seen[i] = c
	}
}
