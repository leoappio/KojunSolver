all: build

build:
	ghc -o main main.hs matrixHelper.hs types.hs kojunSolver.hs

clean:
	find . \( -name "*.o" -o -name "*.hi" -o -name "main" \) -delete
