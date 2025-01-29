n=0; 
while [[ $n -lt 20 ]]; 
do dune exec bin/ /*file name*/.exe; 
n=$((n+1));
 done