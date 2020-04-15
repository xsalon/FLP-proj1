# Project: DKA-2-MKA (FLP 19/20)
# Author: Marek Salon (xsalon00)
# Description: Makefile

all:
	ghc src/dka-2-mka.hs -o dka-2-mka
clean:
	rm -f src/dka-2-mka.o src/dka-2-mka.hi dka-2-mka
