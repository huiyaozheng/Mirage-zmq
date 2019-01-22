# Sets of experiments

1. MirageOS executable server <-> cpp client
2. MirageOS executable server <-> Java client
3. MirageOS executable server <-> MirageOS executable client
4. MirageOS unikernel server <-> cpp client
5. MirageOS unikernel server <-> Java client
6. MirageOS unikernel server <-> MirageOS executable client
7. cpp server <-> cpp client
8. cpp server <-> java client
9. cpp server <-> MirageOS executable client
10. Java server <-> cpp client
11. Java server <-> Java client
12. Java server <-> MirageOS executable client

This will give us some idea about how executable and unikernel compare with the original implementations.

Within each group, we can see how the clients compare with each other.

# Metrics to measure

## Latency 

measure the period from the client sending the request to receiving the reply

## Memory usage of server

how does the memory usage scale with increasing number of messages (is there memory leakage) and connected clients (should be linear)