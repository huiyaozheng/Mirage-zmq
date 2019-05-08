cd PUB/unikernel
./pub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../SUB 
python3 sub.py > ../output2.tmp & p2=$! 
cd ..
wait "$p2"
disown "$p1"
kill "$p1"
pass=true
file="output2.tmp"
while read line; do
    if grep -q '^A' <<< "$line"; 
    then 
        : 
    else 
        pass=false 
    fi
done < "$file"
rm output1.tmp
rm output2.tmp
if $pass 
then 
    echo "PUB unikernel - SUB Pyzmq passed"
else 
    echo "PUB unikernel - SUB Pyzmq failed"
fi