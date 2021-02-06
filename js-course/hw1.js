'use strict';

/**
 * Складывает два целых числа
 * @param {Number} a Первое целое
 * @param {Number} b Второе целое
 * @throws {TypeError} Когда в аргументы переданы не числа
 * @returns {Number} Сумма аргументов
 */
function abProblem(a, b) {
    if (typeof a !== "number" || typeof b !== "number") {
        throw new TypeError("Numbers expected")
    }

    return a + b;
}

/**
 * Определяет век по году
 * @param {Number} year Год, целое положительное число
 * @throws {TypeError} Когда в качестве года передано не число
 * @throws {RangeError} Когда год – отрицательное значение
 * @returns {Number} Век, полученный из года
 */
function centuryByYearProblem(year) {
    if (typeof year !== "number") {
        throw new TypeError("Number expected")
    }

    if (year < 0) {
        throw new RangeError("Year can't be negative")
    }

    const century = Math.floor(year / 100);
    return year % 100 > 0 ? century + 1 : century;
}

/**
 * Переводит цвет из формата HEX в формат RGB
 * @param {String} hexColor Цвет в формате HEX, например, '#FFFFFF'
 * @throws {TypeError} Когда цвет передан не строкой
 * @throws {RangeError} Когда значения цвета выходят за пределы допустимых
 * @returns {String} Цвет в формате RGB, например, '(255, 255, 255)'
 */
function colorsProblem(hexColor) {
    if (typeof hexColor !== "string") {
        throw new TypeError("String expected");
    }

    const pattern = /^#[0-9A-Fa-f]{6}$/
    if (hexColor.match(pattern) === null) {
        throw new RangeError("Invalid input format")
    }

    const rgb = [parseInt(hexColor.substr(1, 2), 16),
        parseInt(hexColor.substr(3, 2), 16),
        parseInt(hexColor.substr(5, 2), 16)];


    return '(' + rgb.join(', ') + ')';
}

/**
 * Находит n-ое число Фибоначчи
 * @param {Number} n Положение числа в ряде Фибоначчи
 * @throws {TypeError} Когда в качестве положения в ряде передано не число
 * @throws {RangeError} Когда положение в ряде не является целым положительным числом
 * @returns {Number} Число Фибоначчи, находящееся на n-ой позиции
 */
function fibonacciProblem(n) {
    if (typeof n !== "number") {
        throw new TypeError("Number expected")
    }

    if (n <= 0 || !Number.isInteger(n)) {
        throw new RangeError("Positive integer expected");
    }

    let a = 1;
    let b = 1;

    for (let i = 3; i <= n; i++) {
        const c = a + b;
        a = b;
        b = c;
    }

    return b;
}

/**
 * Транспонирует матрицу
 * @param {(Any[])[]} matrix Матрица размерности MxN
 * @throws {TypeError} Когда в функцию передаётся не двумерный массив
 * @returns {(Any[])[]} Транспонированная матрица размера NxM
 */
function matrixProblem(matrix) {
    const err = new TypeError("Matrix expected");
    if (Array.isArray(matrix)) {
        if (matrix.length === 0) {
            return [];
        }

        let size;
        if (Array.isArray(matrix[0])) {
            size = matrix[0].length;
        } else {
            throw err;
        }

        for (let i = 0; i < matrix.length; i++) {
            if (!(Array.isArray(matrix[i]) && matrix[i].length === size)) {
                throw err;
            }
        }
    } else {
        throw err;
    }

    const n = matrix.length;
    const m = matrix[0].length;

    let transposed = []

    for (let i = 0; i < m; i++) {
        transposed.push([])
    }

    for (let i = 0; i < n; i++) {
        for (let j = 0; j < m; j++) {
            transposed[j][i] = matrix[i][j];
        }
    }

    return transposed;
}

/**
 * Переводит число в другую систему счисления
 * @param {Number} n Число для перевода в другую систему счисления
 * @param {Number} targetNs Система счисления, в которую нужно перевести (Число от 2 до 36)
 * @throws {TypeError} Когда переданы аргументы некорректного типа
 * @throws {RangeError} Когда система счисления выходит за пределы значений [2, 36]
 * @returns {String} Число n в системе счисления targetNs
 */
function numberSystemProblem(n, targetNs) {
    if (typeof n !== "number" || typeof targetNs !== "number") {
        throw new TypeError("Numbers expected")
    }

    if (targetNs < 2 || targetNs > 36) {
        throw new RangeError("Number system must be from 2 to 36");
    }

    return n.toString(targetNs);
}

/**
 * Проверяет соответствие телефонного номера формату
 * @param {String} phoneNumber Номер телефона в формате '8–800–xxx–xx–xx'
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Boolean} Если соответствует формату, то true, а иначе false
 */
function phoneProblem(phoneNumber) {
    if (typeof phoneNumber !== "string") {
        throw new TypeError("String expected")
    }

    const pattern = /^8-800-[0-9]{3}-[0-9]{2}-[0-9]{2}$/
    return phoneNumber.match(pattern) !== null
}

/**
 * Определяет количество улыбающихся смайликов в строке
 * @param {String} text Строка в которой производится поиск
 * @throws {TypeError} Когда в качестве аргумента передаётся не строка
 * @returns {Number} Количество улыбающихся смайликов в строке
 */
function smilesProblem(text) {
    if (typeof text !== "string") {
        throw new TypeError("String expected")
    }

    const isSmile = function(a, b, c) {
        const str = a.toString() + b.toString() + c.toString();
        return str === ':-)' || str === '(-:';
    }

    let cnt = 0;

    for (let i = 2; i < text.length; i++) {
        if (isSmile(text[i - 2], text[i - 1], text[i])) {
            cnt++;
        }
    }

    return cnt;
}

/**
 * Определяет победителя в игре "Крестики-нолики"
 * Тестами гарантируются корректные аргументы.
 * @param {(('x' | 'o')[])[]} field Игровое поле 3x3 завершённой игры
 * @returns {'x' | 'o' | 'draw'} Результат игры
 */
function ticTacToeProblem(field) {
    for (let i = 0; i < 3; i++) {
        if (field[i][0] === field[i][1] && field[i][0] === field[i][2]) {
            return field[i][0];
        }

        if ((field[0][i] === field[1][i] && field[0][i] === field[2][i])) {
            return field[0][i];
        }
    }

    if (field[0][0] === field[1][1] && field[0][0] === field[2][2]) {
        return field[0][0];
    }

    if (field[0][2] === field[1][1] && field[1][1] === field[2][0]) {
        return field[1][1];
    }

    return 'draw';
}

module.exports = {
    abProblem,
    centuryByYearProblem,
    colorsProblem,
    fibonacciProblem,
    matrixProblem,
    numberSystemProblem,
    phoneProblem,
    smilesProblem,
    ticTacToeProblem
};
