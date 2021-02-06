package ru.ifmo.rain.mozhevitin.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import static java.util.concurrent.Executors.*;

public class HelloUDPServer implements HelloServer {
    //finals?
    private DatagramSocket udpSocket;
    private ExecutorService requestListener;
    private ExecutorService workersPool;

    /**
     * {@inheritDoc}
     */
    @Override
    public void start(int port, int threads) {
        try {
            udpSocket = new DatagramSocket(port);
        } catch (SocketException e) {
            System.err.println("Can't create socket with port " + port + ". Server didn't start.");
            return;
        }

        requestListener = newSingleThreadExecutor();
        workersPool = newFixedThreadPool(threads);

        requestListener.submit(() -> {
            try {
                while (!Thread.interrupted()) {
                    DatagramPacket packet2Receive = new DatagramPacket(
                            new byte[udpSocket.getReceiveBufferSize()],
                            udpSocket.getReceiveBufferSize());

                    udpSocket.receive(packet2Receive);
                    workersPool.submit(() -> {
                        try {
                            packet2Receive.setData(("Hello, " + new String(
                                    packet2Receive.getData(),
                                    packet2Receive.getOffset(),
                                    packet2Receive.getLength(),
                                    StandardCharsets.UTF_8
                            )).getBytes(StandardCharsets.UTF_8));
                            udpSocket.send(packet2Receive);
                        } catch (IOException e) {
                            if (!udpSocket.isClosed()) {
                                System.err.println("I/O error occurred while processing the request.");
                            }
                        }
                    });
                }
            } catch (IOException e) {
                if (!udpSocket.isClosed()) {
                    System.err.println("I/O error occurred while processing the request.");
                }
            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        udpSocket.close();
        requestListener.shutdownNow();
        workersPool.shutdown();
        try {
            workersPool.awaitTermination(10, TimeUnit.SECONDS);
        } catch (InterruptedException ignored) {
        }
    }

    /**
     * Provides command line interface to run {@link HelloServer#start(int, int)}
     * with specific arguments
     *
     * @param args command line arguments to set-up {@link HelloServer#start(int, int)}
     *
     * To find out the meaning of the arguments check
     * {@link HelloUDPServer#start(int, int)}
     */
    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Wrong program arguments. 2 non-null arguments expected.");
            return;
        }

        int port, workers;
        try {
            port = Integer.parseInt(args[0]);
            workers = Integer.parseInt(args[1]);

            HelloServer server = new HelloUDPServer();
            server.start(port, workers);
        } catch (NumberFormatException nfe) {
            System.err.println("Integer arguments expected.");
        }
    }
}