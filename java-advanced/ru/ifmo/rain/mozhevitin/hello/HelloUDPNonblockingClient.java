package ru.ifmo.rain.mozhevitin.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;

public class HelloUDPNonblockingClient implements HelloClient {
    private static final int BUFF_SIZE = 1 << 15;
    private static final int DEFAULT_TIMEOUT = 1 << 9;

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try (Selector selector = Selector.open()) {
            InetSocketAddress addr = new InetSocketAddress(host, port);
            List<DatagramChannel> channels = new ArrayList<>();
            int[] reqNumPerChannel = new int[threads];

            for (int i = 0; i < threads; i++) {
                DatagramChannel dc = DatagramChannel.open();
                dc.configureBlocking(false);
                dc.connect(addr);
                dc.register(selector, OP_WRITE);
                channels.add(dc);
            }

            int done = 0;

            mainLoop:
            while (!Thread.interrupted()) {
                selector.select(DEFAULT_TIMEOUT);
                wakeupKeys(selector);
                for (Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                    try {
                        SelectionKey key = it.next();
                        if (!key.isValid()) {
                            continue ;
                        }

                        if (key.isReadable()) {
                            DatagramChannel channel = (DatagramChannel) key.channel();
                            int index = channels.indexOf(channel);
                            ByteBuffer buf = ByteBuffer.allocate(BUFF_SIZE);
                            channel.receive(buf);
                            String message = new String(buf.array(), StandardCharsets.UTF_8).trim();

                            if (!checkResponse(message, prefix, index, reqNumPerChannel[index])) {
                                continue;
                            }

                            int reqNum = ++reqNumPerChannel[index];
                            if (reqNum >= requests) {
                                done++;
                            }

                            key.interestOps(OP_WRITE);
                        }

                        if (done == threads) {
                            break mainLoop;
                        }

                        if (key.isWritable()) {
                            DatagramChannel channel = (DatagramChannel) key.channel();
                            int index = channels.indexOf(channel);
                            int reqNum = reqNumPerChannel[index];

                            if (reqNum > requests - 1) {
                                key.interestOps(OP_READ);
                                break;
                            }

                            String message = makeRequest(prefix, index, reqNum);
                            ByteBuffer buffer = ByteBuffer.wrap(message.getBytes(StandardCharsets.UTF_8));
                            channel.write(buffer);
                            key.interestOps(OP_READ);
                        }
                    } finally {
                        it.remove();
                    }
                }
            }

            for (DatagramChannel channel : channels) {
                channel.close();
            }

        } catch (IOException e) {
            System.err.println("An error occurred while sending requests. Shutting down...");
        }
    }

    private String makeRequest(String prefix, int index, int reqNum) {
        return prefix + index + "_" + reqNum;
    }

    private boolean checkResponse(String response, String prefix, int index, int reqNum) {
        String expectedMsg = makeRequest(prefix, index, reqNum);
        return response.contains(expectedMsg);
    }

    private void wakeupKeys(Selector selector) {
        if (selector.selectedKeys().isEmpty()) {
            for (SelectionKey key : selector.keys()) {
                key.interestOps(OP_WRITE);
            }
        }
    }

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

            HelloClient client = new HelloUDPNonblockingClient();
            client.run(host, port, prefix, threads, requestsPerThread);
        }
    }
}

