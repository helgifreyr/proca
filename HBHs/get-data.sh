# rsync and re run
rsync -avzsh blafis:proca/HBHs/m=1.0 .
./re-run.sh m=1.0/alpha=1.0/2nd/w=0.*/*
grep Int m=1.0/alpha=1.0/2nd/w=*/rh=*/tmp.txt
