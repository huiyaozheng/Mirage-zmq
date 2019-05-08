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

cd PUB/unikernel
./pub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../XSUB 
python3 xsub.py > ../output2.tmp & p2=$! 
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
    echo "PUB unikernel - XSUB Pyzmq passed"
else 
    echo "PUB unikernel - XSUB Pyzmq failed"
fi

cd PUB/unikernel
./pub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../SUB/unikernel
./sub_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p2"
disown "$p1"
kill "$p1"
FD1=7
FD2=8
file="output2.tmp"
file_regex="SUB/unikernel/expected_output.txt"
eof1=0
eof2=0
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
    echo "PUB unikernel - SUB unikernel passed"
else 
    echo "PUB unikernel - SUB unikernel failed"
fi

cd PUB/unikernel
./pub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../XSUB/unikernel
./xsub_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p2"
disown "$p1"
kill "$p1"
FD1=7
FD2=8
file="output2.tmp"
file_regex="XSUB/unikernel/expected_output.txt"
eof1=0
eof2=0
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
    echo "PUB unikernel - XSUB unikernel passed"
else 
    echo "PUB unikernel - XSUB unikernel failed"
fi