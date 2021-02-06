package ru.ifmo.rain.mozhevitin.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

public class HashCalcFileVisitor extends SimpleFileVisitor<Path> {
    private final static int BUFFER_SIZE = 1024;
    private final static int INIT_HASH_VALUE = 0x811c9dc5;
    private final static int FNV_PRIME = 0x01000193;
    private BufferedWriter writer;

    HashCalcFileVisitor(BufferedWriter writer) {
        this.writer = writer;
    }

    private FileVisitResult writeAndContinue(Path file, int hash) throws IOException {
        writer.write(String.format("%08x", hash) + " " + file);
        writer.newLine();

        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        int hash = INIT_HASH_VALUE;
        try (final InputStream inputStream = Files.newInputStream(file)) {
            byte[] bytes = new byte[BUFFER_SIZE];
            int readBytes;
            while ((readBytes = inputStream.read(bytes)) != -1) {
                for (int i = 0; i < readBytes; i++) {
                    hash = (hash * FNV_PRIME) ^ (bytes[i] & 0xff);
                }
            }
        } catch (IOException e) {
            hash = 0;
        }

        return writeAndContinue(file, hash);
    }

    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        return writeAndContinue(file, 0);
    }
}
