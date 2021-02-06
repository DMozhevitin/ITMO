cd "$(dirname $0)"
rm -rf _javadoc
TESTS_PATH=../../../../../../../java-advanced-2020
javadoc \
    -private \
    -link https://docs.oracle.com/en/java/javase/11/docs/api/ \
    -d _javadoc \
    -cp artifacts/JarImplementorTest.jar:"lib/hamcrest-core-1.3.jar:lib/junit-4.11.jar:lib/jsoup-1.8.1.jar:"lib/quickcheck-0.6.jar:\
     ./JarImplementor.java \
     ./CodeGenUtils.java \
     ./Implementor.java \
     ../../../../../package-info.java \
     "$TESTS_PATH"/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/Impler.java \
     "$TESTS_PATH"/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/JarImpler.java \
     "$TESTS_PATH"/modules/info.kgeorgiy.java.advanced.implementor/info/kgeorgiy/java/advanced/implementor/ImplerException.java\
