dat = ReadList["global-data.dat"];
datSolitons = ReadList["solitons.txt"][[1]];

Export["gnuplot-data.dat",dat,"Table"]
Export["gnuplot-data-solitons.dat",datSolitons,"Table"]
