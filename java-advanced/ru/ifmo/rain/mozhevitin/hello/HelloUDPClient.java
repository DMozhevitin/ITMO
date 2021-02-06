package ru.ifmo.rain.mozhevitin.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

import static java.util.concurrent.Executors.*;

public class HelloUDPClient implements HelloClient {
    private static final int DEFAULT_SOCKET_TIMEOUT = 500;

    /**
     {@inheritDoc}
     */
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        SocketAddress serverSockAddr;
        try {
            serverSockAddr = new InetSocketAddress(InetAddress.getByName(host), port);
        } catch (UnknownHostException e) {
            System.err.println("Unknown host: " + host);
            return;
        }

        ExecutorService workersPool = newFixedThreadPool(threads);

        IntStream.range(0, threads).forEach(thread -> {
            workersPool.submit(() -> {
                try (DatagramSocket udpSocket = new DatagramSocket()) {
                    udpSocket.setSoTimeout(DEFAULT_SOCKET_TIMEOUT);

                    final DatagramPacket packet = new DatagramPacket(
                            new byte[udpSocket.getReceiveBufferSize()],
                            udpSocket.getReceiveBufferSize(),
                            serverSockAddr
                    );

                    IntStream.range(0, requests)
                            .mapToObj(requestId -> prefix + thread + "_" + requestId)
                            .forEach(msg -> {
                                System.out.println("Sending : " + msg);
                                while (true) {
                                    try {
                                        packet.setData(msg.getBytes(StandardCharsets.UTF_8));
                                        udpSocket.send(packet);

                                        DatagramPacket responsePacket = new DatagramPacket(
                                                new byte[udpSocket.getReceiveBufferSize()],
                                                udpSocket.getReceiveBufferSize(),
                                                serverSockAddr
                                        );

                                        responsePacket.setData(new byte[udpSocket.getReceiveBufferSize()]);
                                        udpSocket.receive(responsePacket);

                                        String msg2 = new String(
                                                responsePacket.getData(),
                                                responsePacket.getOffset(),
                                                responsePacket.getLength(),
                                                StandardCharsets.UTF_8);

                                        if (msg2.contains(msg)) {
                                            System.out.println("Received " + msg2);
                                            break;
                                        }
                                    } catch (IOException e) {
                                        System.err.println("Error while sending request : " + e.getMessage());
                                    }
                                }
                            });
                } catch (SocketException e) {
                    System.err.println("Unable to create socket: " + e.getMessage());
                }
            });
        });

        workersPool.shutdown();

        try {
            final int timeout = DEFAULT_SOCKET_TIMEOUT * 10;
            workersPool.awaitTermination(timeout * threads * requests, TimeUnit.MILLISECONDS);
        } catch (InterruptedException ignored) {
        }

    }

    /**
     * Provides command line interface to run {@link HelloUDPClient#run(String, int, String, int, int)}
     * with specific arguments
     *
     * @param args command line arguments to set-up {@link HelloUDPClient#run(String, int, String, int, int)}
     *
     * To find out the meaning of the arguments check
     * {@link HelloUDPClient#run(String, int, String, int, int)}
     */
    public static void main(String[] args) {
        if (args == null || args.length != 5) {
            System.err.println("Expected 5 arguments");
        } else {
            for (int i = 0; i < 5; i++) {
                if (args[i] == null) {
                    System.err.println("Arguments can't be null");
                    return;
                }
            }

            String host = args[0];
            String prefix = args[2];
            int port, threads, requestsPerThread;
            try {
                port = Integer.parseInt(args[1]);
                threads = Integer.parseInt(args[3]);
                requestsPerThread = Integer.parseInt(args[4]);
            } catch (NumberFormatException nfe) {
                System.err.println("Integer argument expected: " + nfe.getMessage());
                return;
            }

            HelloClient client = new HelloUDPClient();
            client.run(host, port, prefix, threads, requestsPerThread);
        }
    }
}
