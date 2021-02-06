package ru.ifmo.rain.mozhevitin.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.*;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import static java.nio.channels.SelectionKey.OP_READ;
import static java.nio.channels.SelectionKey.OP_WRITE;
import static java.util.concurrent.Executors.newFixedThreadPool;
import static java.util.concurrent.Executors.newSingleThreadExecutor;

public class HelloUDPNonblockingServer implements HelloServer {
    //finals?
    private ExecutorService workersPool;
    private ExecutorService requestListener;
    private Selector selector;

    private static final int BUFFER_SIZE = 1 << 15;

    @Override
    public void start(int port, int threads) {
        try {
            selector = Selector.open();
        } catch (IOException e) {
            System.err.println("Cannot start server");
            return;
        }

        workersPool = newFixedThreadPool(threads);
        requestListener = newSingleThreadExecutor();

        requestListener.submit(() -> {
            try (DatagramChannel dc = DatagramChannel.open()) {
                InetSocketAddress address = new InetSocketAddress(port);
                dc.configureBlocking(false);
                dc.register(selector, OP_READ);
                dc.bind(address);

                SocketAddress toAddr = null;

                while (!Thread.interrupted()) {
                    selector.select();

                    for (Iterator<SelectionKey> it = selector.selectedKeys().iterator(); it.hasNext(); ) {
                        SelectionKey key = it.next();
                        try {
                            if (!key.isValid()) {
                                continue;
                            }

                            if (key.isReadable()) {
                                toAddr = read(key);
                                selector.wakeup();
                            } else if (key.isWritable()) {
                                write(key, toAddr);
                                selector.wakeup();
                            }
                        } finally {
                            it.remove();
                        }
                    }
                }
            } catch (IOException e) {
                System.err.println("An error occurred while processing request.");
            }

        });
    }

    @Override
    public void close() {
        try {
            selector.close();
        } catch (IOException ignored) {
        }

        requestListener.shutdownNow();
        workersPool.shutdownNow();

        try {
            workersPool.awaitTermination(10, TimeUnit.SECONDS);
        } catch (InterruptedException ignored) {
        }
    }

    private void write(SelectionKey key, SocketAddress to) throws IOException {
        DatagramChannel ch = (DatagramChannel) key.channel();
        ByteBuffer buffer = (ByteBuffer) key.attachment();
        buffer = ByteBuffer.wrap(("Hello, " + new String(buffer.array(), StandardCharsets.UTF_8).trim())
                .getBytes(StandardCharsets.UTF_8));
        ch.send(buffer, to);
        key.interestOps(OP_READ);
    }

    private SocketAddress read(SelectionKey key) throws IOException {
        DatagramChannel ch = (DatagramChannel) key.channel();
        ByteBuffer buffer = ByteBuffer.allocate(BUFFER_SIZE);
        SocketAddress address = ch.receive(buffer);
        key.attach(buffer);
        key.interestOps(OP_WRITE);
        return address;
    }

    public static void main(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Wrong program arguments. 2 non-null arguments expected.");
            return;
        }

        int port, workers;
        try {
            port = Integer.parseInt(args[0]);
            workers = Integer.parseInt(args[1]);

            HelloServer server = new HelloUDPNonblockingServer();
            server.start(port, workers);
        } catch (NumberFormatException nfe) {
            System.err.println("Integer arguments expected.");
        }
    }
}

