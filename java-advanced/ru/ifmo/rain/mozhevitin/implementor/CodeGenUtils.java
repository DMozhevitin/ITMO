package ru.ifmo.rain.mozhevitin.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Modifier;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Class that provides methods to generate classes and interfaces source code.
 */
public class CodeGenUtils {
    /**
     * A file's name suffix constant.
     */
    static final String IMPL_SUFFIX = "Impl";

    /**
     * A new line constant.
     */
    private static final String EOLN = System.lineSeparator();

    /**
     * An opening bracket constant.
     */
    private static final String OPEN_BRACKET = "(";

    /**
     * A closing bracket constant.
     */
    private static final String CLOSE_BRACKET = ")";

    /**
     * An end of expression constant.
     */
    private static final String END_EXPRESSION = ";";

    /**
     * A single whitespace constant.
     */
    private static final String SPACE = " ";

    /**
     * A tabulation constant.
     */
    private static final String TAB = "\t";

    /**
     * An empty string constant.
     */
    private static final String EMPTY = "";

    /**
     * A package constant.
     */
    private static final String PACKAGE = "package";

    /**
     * A modifier {@code public} constant.
     */
    private static final String PUBLIC = "public";

    /**
     * A {@code class} constant.
     */
    private static final String CLASS = "class";

    /**
     * A {@code super} constant.
     */
    private static final String SUPER = "super";

    /**
     * An {@code implements} constant.
     */
    private static final String IMPLEMENTS = "implements";

    /**
     * An {@code extends} constant.
     */
    private static final String EXTENDS = "extends";

    /**
     * A {@code throws} constant.
     */
    private static final String THROWS = "throws";

    /**
     * A {@code return} constant.
     */
    private static final String RETURN = "return";

    /**
     * A delimiter constant.
     */
    private static final String COMMO = ", ";

    /**
     * Returns the default value for a given {@link Class} token.
     *
     * @param clazz the given {@link Class} token
     * @return the default value for given {@link Class} token
     */
    private static String getDefaultValue(Class<?> clazz) {
        if (!clazz.isPrimitive()) {
            return "null";
        } else if (clazz.equals(void.class)) {
            return "";
        } else if (clazz.equals(boolean.class)) {
            return "true";
        } else {
            return "0";
        }
    }


    /**
     * Returns the packages of a given {@link Class} token.
     *
     * @param token the given {@link Class} token
     * @return a string of the packages of a given {@link Class} token
     */
    private static String generatePackageName(Class<?> token) {
        Package pack = token.getPackage();

        return (pack == null ? EMPTY : String.join(SPACE, PACKAGE, pack.getName(), END_EXPRESSION, EOLN));
    }

    /**
     * Returns the new class name for a given {@link Class} token.
     *
     * @param token the given {@link Class} token
     * @return the new class name
     */
    static String getClassName(Class<?> token) {
        return token.getSimpleName() + IMPL_SUFFIX;
    }

    /**
     * Generates a set of {@link MethodKey} from the given array of methods.
     *
     * @param methods the given methods
     * @return the set of {@link MethodKey}
     */
    private static Set<MethodKey> getMethodKeys(Method[] methods) {
        return Arrays.stream(methods).map(MethodKey::new)
                .collect(Collectors.toCollection(HashSet::new));
    }

    /**
     * Generates a list of unique methods for the given {@link Class} token.
     *
     * @param token the given {@link Class} token
     * @return a list of strings with methods sourcecode
     */
    private static List<String> generateMethods(Class<?> token) {
        Set<MethodKey> methods = getMethodKeys(token.getMethods());

        while (token != null) {
            methods.addAll(getMethodKeys(token.getDeclaredMethods()));
            token = token.getSuperclass();
        }

        return methods.stream().map(MethodKey::getMethod)
                .filter(method -> Modifier.isAbstract(method.getModifiers()))
                .map(CodeGenUtils::generateMethod)
                .collect(Collectors.toList());
    }

    /**
     * Generates the method source code.
     *
     * @param method the given method
     * @return the source code of the method
     */
    private static String generateMethod(Method method) {
        return String.join(EOLN,
                TAB + generateExecutable(method,
                        String.join(SPACE, method.getReturnType().getCanonicalName(), method.getName())),
                String.join(SPACE, TAB + TAB + RETURN, getDefaultValue(method.getReturnType())) + END_EXPRESSION,
                TAB + "}");
    }

    /**
     * Generates the constructor source code.
     *
     * @param constructor the given constructor
     * @return the source code of the constructor
     */
    private static String generateConstructor(Constructor<?> constructor) {
        return String.join(EOLN,
                TAB + generateExecutable(constructor, getClassName(constructor.getDeclaringClass())),
                String.join(EMPTY, TAB + TAB, SUPER, OPEN_BRACKET, getArguments(constructor, false),
                        CLOSE_BRACKET) + END_EXPRESSION,
                TAB + "}");
    }

    /**
     * Generates the string of the thrown exceptions of the executable.
     *
     * @param executable the given executable (constructor or method)
     * @return the string with thrown exceptions
     */
    private static String getExceptions(Executable executable) {
        Class<?>[] exceptions = executable.getExceptionTypes();

        if (exceptions.length != 0) {
            return String.join(SPACE, THROWS,
                    Arrays.stream(exceptions).map(Class::getCanonicalName).collect(Collectors.joining(COMMO)));
        }

        return EMPTY;
    }

    /**
     * Generates the opening line for the given executable (method or constructor).
     *
     * @param executable the given {@link Class} token
     * @param name       the given {@link Class} token
     * @return the opening line of the executable
     */
    private static String generateExecutable(Executable executable, String name) {
        return String.join(SPACE,
                PUBLIC,
                name,
                OPEN_BRACKET + getArguments(executable, true) + CLOSE_BRACKET,
                getExceptions(executable),
                "{");
    }

    /**
     * Generates the opening line for the given {@link Class} token.
     *
     * @param token the given {@link Class} token
     * @return the opening line of the class
     */
    private static String generateClassDeclaration(Class<?> token) {
        return String.join(SPACE,
                PUBLIC,
                CLASS,
                getClassName(token),
                (token.isInterface() ? IMPLEMENTS : EXTENDS),
                token.getCanonicalName(),
                "{");
    }


    /**
     * Generates the string with arguments for the given executable.
     *
     * @param executable    the given executable (constructor or method)
     * @param isDeclaration true if declaring the arguments, false if calling a function
     * @return the string of arguments
     */
    private static String getArguments(Executable executable, boolean isDeclaration) {
        return Arrays.stream(executable.getParameters())
                .map(param -> ((isDeclaration ? param.getType().getCanonicalName() + SPACE : EMPTY) +
                        param.getName()))
                .collect(Collectors.joining(COMMO));
    }

    /**
     * Generates a list of constructors for the given {@link Class} token.
     *
     * @param token the given {@link Class} token
     * @return a list of strings with constructors sourcecode
     * @throws ImplerException if no available constructors found
     */
    private static List<String> generateConstructors(Class<?> token) throws ImplerException {
        if (token.isInterface()) {
            return new ArrayList<>();
        }

        List<Constructor<?>> constructors = Arrays.stream(token.getDeclaredConstructors())
                .filter(c -> !Modifier.isPrivate(c.getModifiers())).collect(Collectors.toList());

        if (constructors.isEmpty()) {
            throw new ImplerException("At least one non-private constructor required!");
        }

        return constructors.stream().map(CodeGenUtils::generateConstructor).collect(Collectors.toList());
    }

    /**
     * Generates the sourcecode of class for the given {@link Class} token.
     *
     * @param token the given {@link Class} token
     * @return a string with the sourcecode for the given class
     * @throws ImplerException if an error during code generation occurred
     */
    static String generate(Class<?> token) throws ImplerException {
        return String.join(EOLN,
                generatePackageName(token),
                generateClassDeclaration(token),
                String.join(EOLN, generateConstructors(token)),
                String.join(EOLN, generateMethods(token)),
                "}");
    }

    static class MethodKey {
        /**
         * The method to be hashed and compared
         */
        private Method method;

        /**
         * The constructor crating the new instance of ComparedMethod.
         *
         * @param method the simple method to be made unique
         */
        MethodKey(Method method) {
            this.method = method;
        }

        /**
         * A method to return the current method.
         *
         * @return the method itself
         */
        Method getMethod() {
            return method;
        }

        /**
         * A method counting the hashcode of the current method.
         *
         * @return the hash of the method
         */
        public int hashCode() {
            int POW = 31;
            int hash = method.getName().hashCode() + method.getReturnType().hashCode();
            hash = hash + POW * Arrays.hashCode(method.getParameterTypes());

            return hash;
        }

        /**
         * A method checking whether the object that is given and the method are the same.
         *
         * @param o an object for this method to be compared to
         * @return true if equal, false otherwise
         */
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }

            if (o == null || getClass() != o.getClass()) {
                return false;
            }

            MethodKey that = (MethodKey) o;

            return Objects.equals(method.getReturnType(), that.method.getReturnType()) &&
                    method.getName().equals(that.method.getName()) &&
                    Arrays.equals(method.getParameterTypes(), that.method.getParameterTypes());
        }
    }
}
