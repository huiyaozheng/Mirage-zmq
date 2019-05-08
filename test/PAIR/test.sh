cd PAIR/unikernel1
./pair_unikernel 2> ../../output1.tmp & p1=$! 
cd ..
python3 pair.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1" "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="PAIR/unikernel1/expected_output.txt"
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
file="output2.tmp"
file_regex="PAIR/expected_output.txt"
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
    echo "PAIR unikernel - PAIR Pyzmq passed"
else 
    echo "PAIR unikernel - PAIR Pyzmq failed"
fi

cd PAIR/unikernel1
./pair_unikernel 2> ../../output1.tmp & p1=$! 
cd ..
cd unikernel2
./pair_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1" "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="PAIR/unikernel1/expected_output.txt"
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
file="output2.tmp"
file_regex="PAIR/unikernel2/expected_output.txt"
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
    echo "PAIR unikernel - PAIR unikernel passed"
else 
    echo "PAIR unikernel - PAIR unikernel failed"
fi