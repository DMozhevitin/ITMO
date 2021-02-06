cd "$(dirname $0)"
TESTS_PATH=../../../../../../../java-advanced-2020
MODULE_NAME=ru.ifmo.rain.mozhevitin.implementor
MODULE_PATH=../../../../../

OUT_PATH=_build

LIB_PATH="$TESTS_PATH/lib:$TESTS_PATH/artifacts:."
SRC_PATH="$PWD"
JAR_PATH="$PWD"
rm -rf *.jar
javac --module-path "$LIB_PATH"  "$MODULE_PATH"/module-info.java  "$SRC_PATH"/*.java -d "$OUT_PATH"

cd "$OUT_PATH" || exit

jar -c --file="$JAR_PATH"/_implementor.jar --main-class="$MODULE_NAME".Implementor --module-path="$LIB_PATH" \
    module-info.class ru/ifmo/rain/mozhevitin/implementor/*
cd .. && rm -rf "$OUT_PATH"
