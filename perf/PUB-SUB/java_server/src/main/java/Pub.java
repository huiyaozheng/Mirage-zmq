import org.zeromq.ZMQ;
import org.zeromq.ZContext;

public class Pub
{
    public static void main(String[] args) throws Exception
    {
        try (ZContext context = new ZContext()) {
            ZMQ.Socket socket = context.createSocket(ZMQ.PUB);
            socket.bind("tcp://*:5555");

            while (!Thread.currentThread().isInterrupted()) {
                String content = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA";
                socket.send(content.getBytes(ZMQ.CHARSET), 0);
            }
        }
    }
}
