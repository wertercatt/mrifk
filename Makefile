EXE = .exe
HFLAGS = -v -O

all : mrifk$(EXE)

mrifk$(EXE) : Mrifk*.hs
		ghc --make -XParallelListComp -o $@ ${HFLAGS} Mrifk.hs