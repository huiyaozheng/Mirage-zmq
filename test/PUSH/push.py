import zmq
import time
context = zmq.Context()

#  Socket to talk to server
print("Starting server...")
socket = context.socket(zmq.PUSH)
socket.bind("tcp://127.0.0.1:5556")
i = 1
while True:
    socket.send_string("Work item " + str(i))
    print("Work item " + str(i) + " sent")
    time.sleep(3)
    i = i + 1
    