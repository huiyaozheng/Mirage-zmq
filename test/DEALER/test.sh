cd DEALER/unikernel
./dealer_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../REP
python3 rep.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="DEALER/unikernel/expected_output.txt"
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
FD1=7
FD2=8
file="output2.tmp"
file_regex="REP/expected_output.txt"
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
    echo "DEALER unikernel - REP Pyzmq passed"
else 
    echo "DEALER unikernel - REP Pyzmq failed"
fi

cd DEALER/unikernel
./dealer_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../REP/unikernel
./rep_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="DEALER/unikernel/expected_output.txt"
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
FD1=7
FD2=8
file="output2.tmp"
file_regex="REP/unikernel/expected_output.txt"
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
    echo "DEALER unikernel - REP unikernel passed"
else 
    echo "DEALER unikernel - REP unikernel failed"
fi

cd DEALER/unikernel
./dealer_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../ROUTER
python3 router.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="DEALER/unikernel/expected_output.txt"
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
FD1=7
FD2=8
file="output2.tmp"
file_regex="ROUTER/expected_output.txt"
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
    echo "DEALER unikernel - ROUTER Pyzmq passed"
else 
    echo "DEALER unikernel - ROUTER Pyzmq failed"
fi

cd DEALER/unikernel
./dealer_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../ROUTER/unikernel
./router_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="DEALER/unikernel/expected_output.txt"
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
FD1=7
FD2=8
file="output2.tmp"
file_regex="ROUTER/unikernel/expected_output.txt"
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
    echo "DEALER unikernel - ROUTER unikernel passed"
else 
    echo "DEALER unikernel - ROUTER unikernel failed"
fi

cd DEALER/unikernel
./dealer_unikernel 2> ../../output1.tmp & p1=$! 
cd ../
python3 dealer_bind.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1" "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="DEALER/unikernel/expected_output.txt"
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
FD1=7
FD2=8
file="output2.tmp"
file_regex="DEALER/expected_output_bind.txt"
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
    echo "DEALER unikernel - DEALER Pyzmq passed"
else 
    echo "DEALER unikernel - DEALER Pyzmq failed"
fi

cd DEALER/unikernel
./dealer_unikernel 2> ../../output1.tmp & p1=$! 
cd ../unikernel_bind
./dealer_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1" "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="DEALER/unikernel/expected_output.txt"
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
FD1=7
FD2=8
file="output2.tmp"
file_regex="DEALER/unikernel_bind/expected_output.txt"
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
    echo "DEALER unikernel - DEALER unikernel passed"
else 
    echo "DEALER unikernel - DEALER unikernel failed"
fi