package main

import (
	"math/rand"
)

type Coord struct {
	X uint8
	Y uint8
}

type Game struct {
	Pegs [7]Coord
	Thaler Coord
	CurrentTurn uint8
	PlayerChoices [2]uint8
}

func generate7RandomCoordinates(r *rand.Rand) [7]Coord {
	var res [7]Coord
	gen := r.Perm(64)

	for i, _ := range res {
		res[i] = Coord{ uint8(gen[i] % 8), uint8(gen[i] / 8) }
	}

	return res
}

func InitializeGame(r *rand.Rand) Game {
	game := Game{}
	game.Pegs = generate7RandomCoordinates(r)
	return game
}
