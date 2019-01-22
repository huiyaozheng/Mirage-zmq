#include <iostream>
#include <string>
#include <zmq.hpp>
#include <chrono>
class Timer {
 public:
  void start() {
    m_StartTime = std::chrono::system_clock::now();
    m_bRunning = true;
  }
  void stop() {
    m_EndTime = std::chrono::system_clock::now();
    m_bRunning = false;
  }
  double elapsedMicroseconds() {
    std::chrono::time_point<std::chrono::system_clock> endTime;
    if (m_bRunning) {
      endTime = std::chrono::system_clock::now();
    } else {
      endTime = m_EndTime;
    }
    return std::chrono::duration_cast<std::chrono::microseconds>(endTime -
                                                                 m_StartTime)
        .count();
  }

 private:
  std::chrono::time_point<std::chrono::system_clock> m_StartTime;
  std::chrono::time_point<std::chrono::system_clock> m_EndTime;
  bool m_bRunning = false;
};

int main() {
  //  Prepare our context and socket
  zmq::context_t context(1);
  zmq::socket_t socket(context, ZMQ_REQ);

  std::cout << "Connecting to hello world server…" << std::endl;
  socket.connect("tcp://localhost:5555");

  Timer timer;

  //  Do 10 requests, waiting each time for a response
  for (int request_nbr = 0; request_nbr != 10; request_nbr++) {
    zmq::message_t request(5);
    memcpy(request.data(), "Hello", 5);
    std::cout << "Sending Hello " << request_nbr << "…" << std::endl;
    
    timer.start();
    socket.send(request);

    //  Get the reply.
    zmq::message_t reply;
    socket.recv(&reply);
    
    timer.stop();

    std::cout << "Microseconds: " << timer.elapsedMicroseconds() << std::endl;

    std::cout << "Received World " << request_nbr << std::endl;
  }
  return 0;
}