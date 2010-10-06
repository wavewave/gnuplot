ghci:	ghci-tmp

ghci-tmp:
	ghci -Wall -i:src:dist/build/autogen:execute/tmp src/Graphics/Gnuplot/Simple.hs

ghci-pipe:
	ghci -Wall -i:src:dist/build/autogen:execute/pipe src/Graphics/Gnuplot/Simple.hs

ghci-shell:
	ghci -Wall -i:src:dist/build/autogen:execute/shell src/Graphics/Gnuplot/Simple.hs

ghci-demo:
	ghci -Wall -i:src:dist/build/autogen:execute/tmp src/Demo.hs
