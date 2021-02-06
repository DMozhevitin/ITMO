package ru.ifmo.rain.mozhevitin.implementor;

import info.kgeorgiy.java.advanced.implementor.Impler;
import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

/**
 * The following class implements interfaces and classes given by user.
 *
 */
public class Implementor implements Impler {
    /**
     * Default constructor
     */
    public Implementor() {}

    /**
     * Creates all upper directories for {@link Path} given
     *
     * @param path {@link Path} to create directories for
     * @throws IOException if directories creation fails for some reason
     */
    static void createPath(final Path path) throws IOException {
        final Path parent = path.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
    }


    /**
     * Creates all parent directories of a given root.
     *
     * @param token the {@link Class} token of implemented class
     * @param root  the given path
     * @return the encoded string
     * @throws ImplerException if an error occurs while creating parent dirs
     */
    private Path getSourceRoot(Class<?> token, Path root) throws ImplerException {
        Path path = root.resolve(token.getPackageName().replace('.', File.separatorChar))
                .resolve(token.getSimpleName() + CodeGenUtils.IMPL_SUFFIX + ".java");
        try {
            if (path.getParent() != null) {
                Files.createDirectories(path.getParent());
            }
        } catch (IOException e) {
            throw new ImplerException("Can't create root directories!");
        }

        return path;
    }

    /**
     * Validates the given {@link Class} token.
     *
     * @param token the {@link Class} token of implemented class
     * @throws ImplerException if an unsupported token given
     */
    private void validateToken(Class<?> token) throws ImplerException {
        int modifiers = token.getModifiers();

        if (token.isPrimitive() || token.isArray() || token == Enum.class || Modifier.isFinal(modifiers)
                || Modifier.isPrivate(modifiers)) {
            throw new ImplerException("Given token is unsupported!");
        }
    }

    /**
     * Produces code implementing class or interface specified by provided {@code token}.
     *
     * @param token type token to create implementation for.
     * @param root  root directory.
     * @throws ImplerException when implementation can't be generated.
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        Path path = getSourceRoot(token, root);
        validateToken(token);

        try (BufferedWriter writer = Files.newBufferedWriter(path)) {
            writer.write(toUnicode(CodeGenUtils.generate(token)));
        } catch (IOException e) {
            throw new ImplerException("Can't write to output file: " + e.getMessage() + "!");
        }
    }

    /**
     * Converts given string to unicode escaping.
     *
     * @param data {@link String} to convert.
     * @return converted string.
     */
    static String toUnicode(String data) {
        StringBuilder builder = new StringBuilder();
        for (char c : data.toCharArray()) {
            if (c < 128) {
                builder.append(c);
            } else {
                builder.append(String.format("\\u%04X", (int) c));
            }
        }

        return builder.toString();
    }

    /**
     * Validates received arguments.
     *
     * @param args arguments given by user.
     * @return {@code true}, if all arguments are valid, else {@code false}.
     */
   static boolean validateArgs(String[] args) {
        if (args == null || args.length != 2) {
            System.err.println("Invalid arguments number, expected <class-name> <output-path>");

            return true;
        }

        for (String arg : args) {
            if (arg == null) {
                System.err.println("Invalid argument, expected not null");

                return true;
            }
        }

        return false;
    }

    /**
     * Provides an interface for command line access to {@link #implement(Class, Path)}.
     * Expected combinations of arguments:
     * <ul>
     *     <li>2 arguments to call {@link #implement(Class, Path)} method (Class name and directory to save).</li>
     * </ul>
     *
     * @param args command line arguments
     */
    public static void main(String[] args) {
        if (validateArgs(args)) {
            return;
        }

        try {
            if (args.length == 2) {
                new Implementor().implement(Class.forName(args[0]), Path.of(args[1]));
            } else {
                System.err.println("Invalid command");
            }
        } catch (ClassNotFoundException e) {
            System.err.println("Invalid class name given: " + e.getMessage());
        } catch (InvalidPathException e) {
            System.err.println("Invalid path given: " + e.getMessage());
        } catch (ImplerException e) {
            System.err.println("Error while creating " +
                    "java-file " + e.getMessage());
        }
    }
}
