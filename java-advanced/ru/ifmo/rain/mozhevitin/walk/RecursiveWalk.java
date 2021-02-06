package ru.ifmo.rain.mozhevitin.walk;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class RecursiveWalk {
    public static void main(String[] args) {
        try {
            if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
                System.err.println("Wrong program arguments. Expected <inputFileName> <outputFileName>");
            } else {
                walk(args[0], args[1]);
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }

    private static void walk(final String inputFileName, final String outputFileName) throws IOException {
        Path inputFile;
        Path outputFile;

        try {
            inputFile = Paths.get(inputFileName);
        } catch (InvalidPathException e) {
            throw new IOException("Wrong path to input file");
        }

        try {
            outputFile = Paths.get(outputFileName);
        } catch (InvalidPathException e) {
            throw new IOException("Wrong path to output file");
        }

        if (outputFile.getParent() != null) {
            try {
                Files.createDirectories(outputFile.getParent());
            } catch (IOException e) {
                throw new IOException("Error while creating parent directories for output file" + e.getMessage());
            }
        }

        try (final BufferedReader in = Files.newBufferedReader(inputFile)) {

            try (BufferedWriter out = Files.newBufferedWriter(outputFile)) {

                HashCalcFileVisitor fileVisitor = new HashCalcFileVisitor(out);
                String fileName;
                while ((fileName = in.readLine()) != null) {
                    try {
                        Files.walkFileTree(Paths.get(fileName), fileVisitor);
                    } catch (InvalidPathException e) {
                        out.write(String.format("%08x", 0) + " " + fileName);
                        out.newLine();
                    }
                }

            } catch (IOException e) {
                throw new IOException("Error in output file: " + e.getMessage());
            }
        } catch (IOException e) {
            throw new IOException("Error in input file: " + e.getMessage());
        }
    }
}
