import zmq

context = zmq.Context()

#  Socket to talk to server
print("Starting server...")
socket = context.socket(zmq.REP)
socket.bind("tcp://127.0.0.1:5555")
while True:
    msg = socket.recv_string()
    print("Received " + msg)
    socket.send(b"Received")