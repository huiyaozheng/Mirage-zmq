#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <zmq.hpp>
using namespace std;
const int NO_OF_REQ = 100000;
const int NO_OF_THREADS = 100;
auto target_address = "tcp://10.0.0.2:5555";
auto msg_length = 255;
auto msg =
    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
    "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";

void req_worker(int index) {
  zmq::context_t context(1);
  zmq::socket_t socket(context, ZMQ_REQ);
  socket.connect(target_address);
  cout << "Server connected" << endl;

  for (int request_nbr = 0; request_nbr != NO_OF_REQ; request_nbr++) {
    zmq::message_t request(msg_length);
    memcpy(request.data(), msg, msg_length);
    socket.send(request);
    zmq::message_t reply;
    socket.recv(&reply);
    this_thread::sleep_for(chrono::milliseconds(100));
  }
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    cout << "A CSV filename is needed" << endl;
    return 0;
  }

  thread t[NO_OF_THREADS];
  for (int i = 0; i < NO_OF_THREADS; ++i) {
    t[i] = thread(req_worker, i);
    this_thread::sleep_for(chrono::milliseconds(200));
  }
  for (int i = 0; i < NO_OF_THREADS; ++i) {
    t[i].join();
    cout << i<< endl;
  }
  return 0;
}