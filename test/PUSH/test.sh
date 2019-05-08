cd PUSH/unikernel
./push_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../PULL
python3 pull.py > ../output2.tmp & p2=$! 
cd ..
wait "$p2"
disown "$p1"
kill "$p1"
FD1=7
FD2=8
file="output2.tmp"
file_regex="PULL/expected_output.txt"
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
    echo "PUSH unikernel - PULL Pyzmq passed"
else 
    echo "PUSH unikernel - PULL Pyzmq failed"
fi

cd PUSH/unikernel
./push_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../PULL/unikernel
./pull_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p2"
disown "$p1"
kill "$p1"
FD1=7
FD2=8
file="output2.tmp"
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
    echo "PUSH unikernel - PULL unikernel passed"
else 
    echo "PUSH unikernel - PULL unikernel failed"
fi
