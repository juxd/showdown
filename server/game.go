package main

import (
	"errors"
	"math/rand"
)

const (
	PhaseThaler1 uint8 = iota
	PhaseThaler2 uint8 = iota
	PhasePlayer1 uint8 = iota
	PhasePlayer2 uint8 = iota
)

const (
	MovePlayer1 uint8 = iota
	MovePlayer2 uint8 = iota
)

const (
	ColorBlue uint8 = iota
	ColorGreen uint8 = iota
	ColorOrange uint8 = iota
	ColorPink uint8 = iota
	ColorRed uint8 = iota
	ColorWhite uint8 = iota
	ColorYellow uint8 = iota
	ColorUninitialized uint8 = iota
)

type Coord struct {
	X uint8
	Y uint8
}

type Game struct {
	Pegs [7]Coord
	Thaler Coord
	CurrentTurn uint8
	Player1ColorChoice uint8
	Player2ColorChoice uint8
	Phase uint8
}

func generate7RandomCoordinates(r *rand.Rand) [7]Coord {
	var res [7]Coord
	gen := r.Perm(64)

	for i, _ := range res {
		res[i] = Coord{ uint8(gen[i] % 8), uint8(gen[i] / 8) }
	}

	return res
}

func InitializeGame(r *rand.Rand, p uint8) (Game, error) {
	if p != PhaseThaler1 || p != PhaseThaler2 {
		return Game{},
			errors.New("InitializeGame: expected PhaseThaler1 or PhaseThaler2 but got something else")
	}

	return Game{
		Pegs: generate7RandomCoordinates(r),
		Player1ColorChoice: ColorUninitialized,
		Player2ColorChoice: ColorUninitialized,
	},
		nil
}
