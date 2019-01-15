import zmq
import time
context = zmq.Context()

#  Socket to talk to server
print("Starting server...")
socket = context.socket(zmq.PUB)
socket.bind("tcp://127.0.0.1:5556")
while True:
    socket.send(b"ABC")
    #print(socket.recv_string())