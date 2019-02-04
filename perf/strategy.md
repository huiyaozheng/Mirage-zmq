# Sockets to evaluate

Since many sockets share much underlying code, I will evaluate the performance of some chosen sockets first. They include REP, PUB and ROUTER. These sockets are more likely to be used in a 'server' setting.

# Environment

To ensure all comparisons are fair, a Xen environment is used. In which:
- dom0 is a ubuntu server
- Any Mirage unikernel runs as a domU system
- Any Mirage executable/C++ executable/Java program (as server) runs in a domU ubuntu system
- The corresponding client runs in another domU ubuntu system

# Sets of experiments

For each chosen socket, I will carry out the following experiment:

|Index|Endpoint 1 (Server)|Endpoint 2 (Client)|
|---|---|---|
|1|MirageOS executable (socket)|C++ executable|
|2|MirageOS executable (socket)|Java Program|
|3|MirageOS executable (socket)|MirageOS executable|
|4|MirageOS executable (direct)|C++ executable|
|5|MirageOS executable (direct)|Java Program|
|6|MirageOS executable (direct)|MirageOS executable|
|7|MirageOS unikernel|C++ executable|
|8|MirageOS unikernel|Java Program|
|9|MirageOS unikernel|MirageOS executable|
|10|C++ executable|C++ executable|
|11|C++ executable|Java Program|
|12|C++ executable|MirageOS executable|
|13|Java program|C++ executable|
|14|Java program|Java Program|
|15|Java program|MirageOS executable|
|16|OCaml Zmq executable|C++ executable|
|17|OCaml Zmq executable|Java Program|
|18|OCaml Zmq executable|MirageOS executable|

|Index|Number of clients|Focus|
|---|---|---|
|a|Single|Increasing number of messages|
|b|Multiple|Increasing number of connected clients (clients send messages more slowly)|

Experiments in the two tables are paired to be 1a, 1b, 2a, etc.

Each type of endpoints will have the same behaviour (they should largely send the same messages over the wire).

'MirageOS executable (socket)' refers to an executable compiled to run on macOS or Linux that uses my implementation, with argument `--net socket`. It uses the system's TCP implementation.

'MirageOS executable (direct)' refers to an executable compiled to run on macOS or Linux that uses my implementation, with argument `--net direct`. It uses MirageOS's TCP implementation.

'MirageOS unikernel' refers to a unikernel compiled to run on Xen that uses my implementation.

'C++ executable' refers to an executable compiled to run on macOS or Linux that uses the official C++ implementation.

'Java Program' refers to a Java program compiled to run on JVM that uses the Java implementation.

All measurements shall measure the behaviour of the server side.

These experiments will show how MirageOS executable and unikernel servers compare with servers using C++/Java implementations.

It may also be interesting to see how different clients compare when communicating with the same server.

# Metrics to measure

## RTT Latency 

Latency will be measured on the client side via some system clock library. It is defined as the length of time from the client sending the request to receiving the reply. The server would immediately 'reply' to a message without processing it.

## Memory usage of server

Memory usage of the server will be measured via different ways:
- For MirageOS executable, C++ executable and Java program, their memory usage is measured with a system tool such as `utop`.
- For MirageOS unikernel, its memory usage is measured with a monitor tool provided by Xen

(or OCaml gc/spacetime)

## Throughput

messages processed per second