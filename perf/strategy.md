# Sockets to evaluate

Since many sockets share much underlying code, I will evaluate the performance of some chosen sockets first. They include REP, PUB and ROUTER. These sockets are more likely to be used in a 'server' setting.

# Sets of experiments

For each chosen socket, I will carry out the following experiment:

|Index|Endpoint 1 (Server)|Endpoint 2 (Client)|
|---|---|---|
|1|MirageOS executable|C++ executable|
|2|MirageOS executable|Java Program|
|3|MirageOS executable|MirageOS executable|
|4|MirageOS unikernel|C++ executable|
|5|MirageOS unikernel|Java Program|
|6|MirageOS unikernel|MirageOS executable|
|7|C++ executable|C++ executable|
|8|C++ executable|Java Program|
|9|C++ executable|MirageOS executable|
|10|Java program|C++ executable|
|11|Java program|Java Program|
|12|Java program|MirageOS executable|

|Index|Number of clients|Focus|
|---|---|---|
|a|Single|Increasing number of messages|
|b|Multiple|Increasing number of connected clients (clients send messages more slowly)|

Experiments in the two tables are paired to be 1a, 1b, 2a, etc.

Each type of endpoints will have the same behaviour (they should largely send the same messages over the wire).

'MirageOS executable' refers to an executable compiled to run on macOS or Linux that uses my implementation.

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
