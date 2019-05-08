cd PULL/unikernel
./pull_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../PUSH 
python3 push.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="PULL/unikernel/expected_output.txt"
pass=true
exec 7<$file
exec 8<$file_regex
while read data1 <&$FD1; do
	read data2 <&$FD2;
    if grep -q "$data2" <<< "$data1"; 
    then 
        : 
    else 
        pass=false 
    fi
done
rm output1.tmp
rm output2.tmp
if $pass 
then 
    echo "PULL unikernel - PUSH Pyzmq passed"
else 
    echo "PULL unikernel - PUSH Pyzmq failed"
fi

cd PULL/unikernel
./pull_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../PUSH/unikernel
./push_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="PULL/unikernel/expected_output.txt"
pass=true
exec 7<$file
exec 8<$file_regex
while read data1 <&$FD1; do
	read data2 <&$FD2;
    if grep -q "$data2" <<< "$data1"; 
    then 
        : 
    else 
        pass=false 
    fi
done
rm output1.tmp
rm output2.tmp
if $pass 
then 
    echo "PULL unikernel - PUSH unikernel passed"
else 
    echo "PULL unikernel - PUSH unikernel failed"
fi
