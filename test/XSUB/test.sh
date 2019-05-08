cd XSUB/unikernel
./xsub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../PUB 
python3 pub.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="XSUB/unikernel/expected_output.txt"
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
    echo "XSUB unikernel - PUB Pyzmq passed"
else 
    echo "XSUB unikernel - PUB Pyzmq failed"
fi

cd XSUB/unikernel
./xsub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../XPUB 
python3 xpub.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
pass=true
FD1=7
FD2=8
file="output1.tmp"
file_regex="XSUB/unikernel/expected_output.txt"
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
    echo "XSUB unikernel - XPUB Pyzmq passed"
else 
    echo "XSUB unikernel - XPUB Pyzmq failed"
fi

cd XSUB/unikernel
./xsub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../PUB/unikernel
./pub_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="XSUB/unikernel/expected_output.txt"
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
    echo "XSUB unikernel - PUB unikernel passed"
else 
    echo "XSUB unikernel - PUB unikernel failed"
fi

cd XSUB/unikernel
./xsub_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../XPUB/unikernel
./xpub_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="XSUB/unikernel/expected_output.txt"
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
    echo "XSUB unikernel - XPUB unikernel passed"
else 
    echo "XSUB unikernel - XPUB unikernel failed"
fi