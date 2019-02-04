#include <zmq.hpp>
#include <string>
#include <iostream>

int main () {
    //  Prepare our context and socket
    zmq::context_t context (1);
    zmq::socket_t socket (context, ZMQ_REP);
    socket.bind ("tcp://*:5555");
    while (true) {
        zmq::message_t request;
        //  Wait for next request from client
        socket.recv (&request);
        zmq::message_t reply (5);
        memcpy (reply.data (), "Reply", 5);
        socket.send (reply);
    }
    return 0;
}