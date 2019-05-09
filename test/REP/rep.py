import zmq
context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://127.0.0.1:5556")
while True:
    msg = socket.recv_string()
    print("Received " + msg)
    socket.send(b"Received")