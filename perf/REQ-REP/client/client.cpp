#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include <thread>
#include <zmq.hpp>
using namespace std;
const int NO_OF_REQ = 1000;
const int NO_OF_THREADS = 1;
auto target_address = "tcp://localhost:5555";
auto msg_length = 255;
auto msg =
    "77777777777777777777777777777777777777777777777777777777777777777777777777"
    "77777777777777777777777777777777777777777777777777777777777777777777777777"
    "777777777777777777777777777777777777777777777777777777"
    "7777777777777777777777777777777777777777777777777777";
double latency[NO_OF_REQ * NO_OF_THREADS];

class Timer {
 public:
  void start() {
    m_StartTime = chrono::system_clock::now();
    m_bRunning = true;
  }
  void stop() {
    m_EndTime = chrono::system_clock::now();
    m_bRunning = false;
  }
  double elapsedMicroseconds() {
    chrono::time_point<chrono::system_clock> endTime;
    if (m_bRunning) {
      endTime = chrono::system_clock::now();
    } else {
      endTime = m_EndTime;
    }
    return chrono::duration_cast<chrono::microseconds>(endTime - m_StartTime)
        .count();
  }

 private:
  chrono::time_point<chrono::system_clock> m_StartTime;
  chrono::time_point<chrono::system_clock> m_EndTime;
  bool m_bRunning = false;
};

class Writer {
  ofstream out;

 public:
  Writer(string filename) { out.open(filename + ".csv"); }

  void write(string line) { out << line << '\n'; }

  ~Writer() { out.close(); }
};

void req_worker(int index) {
  zmq::context_t context(1);
  zmq::socket_t socket(context, ZMQ_REQ);
  cout << "Connecting to server…" << endl;
  socket.connect(target_address);
  Timer timer;
  for (int request_nbr = 0; request_nbr != NO_OF_REQ; request_nbr++) {
    zmq::message_t request(msg_length);
    memcpy(request.data(), msg, msg_length);
    // cout << "Sending request " << request_nbr << "…" << endl;
    timer.start();
    socket.send(request);
    zmq::message_t reply;
    socket.recv(&reply);
    timer.stop();
    double _latency = timer.elapsedMicroseconds();
    // cout << "Microseconds: " << timer.elapsedMicroseconds() <<
    // endl;
    // w.write(to_string(request_nbr) + ',' + to_string(latency));
    // cout << "Received reply " << request_nbr << endl;
    latency[index * NO_OF_REQ + request_nbr] = _latency;
    this_thread::sleep_for(chrono::milliseconds(100));
  }
}

int main(int argc, char *argv[]) {
  if (argc != 2) {
    cout << "A CSV filename is needed" << endl;
    return 0;
  }
  Writer w(argv[1]);

  auto line = "Req no."s;
  for (int j = 0; j < NO_OF_THREADS; ++j) {
    line += ", latency (microseconds)"s;
  }
  w.write(line);

  thread t[NO_OF_THREADS];
  for (int i = 0; i < NO_OF_THREADS; ++i) {
    t[i] = thread(req_worker, i);
  }
  for (int i = 0; i < NO_OF_THREADS; ++i) {
    t[i].join();
  }
  for (int i = 0; i < NO_OF_REQ; ++i) {
    auto line = to_string(i);
    for (int j = 0; j < NO_OF_THREADS; ++j) {
      line += ", " + to_string(latency[i + j * NO_OF_REQ]);
    }
    w.write(line);
  }
  return 0;
}