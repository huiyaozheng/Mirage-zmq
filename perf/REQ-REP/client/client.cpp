#include <iostream>
#include <string>
#include <fstream>
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

class Writer {
  std::ofstream out;
public:
  Writer(std::string filename) {
    out.open (filename+".csv");
  }

  void write(std::string line) {
    out << line << '\n';
  }

  ~Writer() {
    out.close();
  }
};

int main(int argc, char *argv[]) {
  if (argc != 2) {
    std::cout << "A CSV filename is needed" << std::endl;
    return 0;
  }
  Writer w(argv[1]);
  
  w.write("Req no., latency (microsecond)");
  zmq::context_t context(1);
  zmq::socket_t socket(context, ZMQ_REQ);
  std::cout << "Connecting to server…" << std::endl;
  socket.connect("tcp://localhost:5555");
  Timer timer;
  const int REQ_NO = 10000;
  for (int request_nbr = 0; request_nbr != REQ_NO; request_nbr++) {
    zmq::message_t request(7);
    memcpy(request.data(), "Request", 7);
    std::cout << "Sending request " << request_nbr << "…" << std::endl;
    timer.start();
    socket.send(request);
    zmq::message_t reply;
    socket.recv(&reply);
    timer.stop();
    double latency = timer.elapsedMicroseconds();
    //std::cout << "Microseconds: " << timer.elapsedMicroseconds() << std::endl;
    w.write(std::to_string(request_nbr) + ',' + std::to_string(latency));
    std::cout << "Received reply " << request_nbr << std::endl;
  }
  return 0;
}