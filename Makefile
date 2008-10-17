ghci:	ghci-tmp

ghci-tmp:
	ghci -Wall -i:src:execute/tmp src/Graphics/Gnuplot/Simple.hs

ghci-pipe:
	ghci -Wall -i:src:execute/pipe src/Graphics/Gnuplot/Simple.hs

ghci-shell:
	ghci -Wall -i:src:execute/shell src/Graphics/Gnuplot/Simple.hs
