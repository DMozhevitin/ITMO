'use strict';

class CombParser {
    constructor(parse) {
        this.parse = parse;
    }

    toString = function () {
        return this.parse.toString()
    }
}

function textParser(text) {
    if (typeof text !== "string") {
        throw new TypeError("Text should be string");
    }

    return new CombParser((s, pos) => {
        if (s.substr(pos, text.length) === text) {
            return {res: text, end: pos + text.length}
        } else {
            return { error: true, end: pos };
        }
    });
}

function regexpParser(regexp) {
    if (!regexp instanceof RegExp) {
        throw new TypeError("Regexp should be RegExp");
    }

    return new CombParser((s, pos) => {
        const res = regexp.exec(s.substr(pos));
        if (res && res.index === 0) {
            const m = res[0];

            return { res: m, end: pos + m.length }
        } else {
            return { error: true, end: pos }
        }
    });
}

function anyParser(...parsers) {
    return new CombParser((s, pos) => {
        let maxPos = pos;
        for (let i = 0; i < parsers.length; i++) {
            const res = parsers[i].parse(s, pos);

            if (!res.error) {
                return res;
            } else {
                maxPos = Math.max(maxPos, res.end);
            }
        }

        return { error: true, end: maxPos }
    });
}

function sequenceParser(...parsers) {

    return new CombParser((s, pos) => {
        let idx = pos;
        let results = [];

        for (let parser of parsers) {
            const res = parser.parse(s, idx);

            // if (!res) {
            //     return;
            // }

            if (res.error) {
                return res;
            }

            results.push(res);
            idx = res.end;
        }


        return {res: results, end: idx}
    });
}

function replicateParser(parser, sep) {
    const separated = !sep ? parser :
        sequenceParser(sep, parser);

    return new CombParser((s, pos) => {
        let res = [], end = pos, r = parser.parse(s, end);

        if (r.error) {
            return r;
        }

        let fst = true

        while ((r && !r.error) && r.end > end) {
            if (fst) {
                res.push(r.res);
            } else {
                res.push(r.res[1].res)
            }

            fst = false;

            end = r.end;
            r = (separated.parse(s, end));
        }

        return {res: res, end: end};
    });
}

const ws = textParser(" ");
const endl = textParser(";");
const identifier = /([^;]*)/
const phone = /\b\d{10}\b/
const mail = /[^\s]+/

const addContactParser = sequenceParser(
    textParser("Создай "),
    textParser("контакт "),
    regexpParser(identifier),
    endl
);

const deleteContactParser = sequenceParser(
    textParser("Удали "),
    textParser("контакт "),
    regexpParser(identifier),
    endl
);


const somethingParserSuffix = [
    replicateParser(
        anyParser(
            sequenceParser(
                textParser("телефон "),
                regexpParser(phone)
            ),

            sequenceParser(
                textParser("почту "),
                regexpParser(mail)
            ),
        ),
        sequenceParser(
            ws,
            textParser("и"),
            ws
        )
    ),
    ws,
    textParser("для "),
    textParser("контакта "),
    regexpParser(identifier),
    endl
]

const addSomethingParser = sequenceParser(
    textParser("Добавь "),
    ...somethingParserSuffix
);

const deleteSomethingParser = sequenceParser(
    textParser("Удали "),
    ...somethingParserSuffix
);


const selectParser = sequenceParser(
    textParser("Покажи "),
    replicateParser(
        anyParser(
            textParser(
                "имя "
            ),

            textParser(
                "почты "
            ),

            textParser(
                "телефоны "
            )
        ),
        textParser("и ")
    ),
    textParser("для "),
    textParser("контактов, "),
    textParser("где "),
    textParser("есть "),
    regexpParser(identifier),
    endl
);

const deleteContactsWhereParser = sequenceParser(
    textParser("Удали "),
    textParser("контакты, "),
    textParser("где "),
    textParser("есть "),
    regexpParser(identifier),
    endl
);

function executeAddContact(q) {
    const name = q.res[2].res;

    if (phoneBook.get(name)) {
        return;
    }

    phoneBook.set(name, {
        phones: [],
        emails: [],
        order: order,
        name: name
    });

    order++;
}

function executeDeleteContact(q) {
    const name = q.res[2].res;

    if (name === '') {
        return;
    }

    phoneBook.delete(name);
}

function executeAddSomething(q) {
    const entries = q.res[1].res;
    const pairs = entries.map(e => [e[0].res, e[1].res]);

    const name = q.res[5].res;

    if (!phoneBook.get(name)) {
        return;
    }

    pairs.forEach(p => {
        const keyRus = p[0].trim();
        const value = p[1].trim();

        let key;
        if (keyRus === "телефон") {
           key = "phones";
        } else {
           key = "emails";
        }

        if (!phoneBook.get(name)[key].includes(value)) {
            phoneBook.get(name)[key].push(value);
        }
    })
}

function executeDeleteSomething(q) {
    const entries = q.res[1].res;
    const pairs = entries.map(e => [e[0].res, e[1].res]);

    const name = q.res[5].res;

    if (!phoneBook.get(name)) {
        return;
    }

    pairs.forEach(p => {
        const keyRus = p[0].trim();
        const value = p[1].trim();

        let key;
        if (keyRus === "телефон") {
            key = "phones";
        } else {
            key = "emails";
        }

        phoneBook.get(name)[key] = phoneBook.get(name)[key].filter(e => e !== value);
    })
}

let order = 0;

function extractValues(record, entries) {
    let arr = []
    entries.forEach(e => {
        if (e === "name") {
            arr.push(record.name);
        } else if (e === "phones") {
            arr.push(record.phones.map(ph => {
                return '+7 (' + ph.substring(0, 3) + ') ' + ph.substring(3, 6) + '-'
                    + ph.substring(6, 8) + '-' + ph.substring(8, 10);
            }).join(','));
        } else {
            arr.push(record.emails.join(','));
        }
    })

    return arr.join(';');
}

function executeSelect(q) {
    const entries = q.res[1].res.map(e => {
        const e1 = e.trim();

        if (e1 === "имя") {
            return "name";
        } else if (e1 === "почты") {
            return "emails";
        } else {
            return "phones";
        }
    });

    const par = q.res[q.res.length - 2].res;

    let res = [];

    if (par === '') {
        return [];
    }

    let records = [...phoneBook.values()];
    records.sort((a, b) => a.order - b.order);

    records.forEach(r => {
        if (r.name.includes(par) || r.phones.some(ph => ph.includes(par))
            || r.emails.some(em => em.includes(par))) {

            res.push(extractValues(r, entries));
        }
    })

    return res;
}


function executeDeleteContactsWhere(q) {
    const par = q.res[q.res.length - 2].res;

    if (par === '') {
        return;
    }

    let keys2delete = [];
    let records = [...phoneBook.values()];

    records.forEach(r => {
        if (r.name.includes(par) || r.phones.some(ph => ph.includes(par))
            || r.emails.some(em => em.includes(par))) {

            keys2delete.push(r.name);
        }
    })

    keys2delete.forEach(k => phoneBook.delete(k));
}

const parsersNexecs = [
    [addContactParser, executeAddContact],
    [deleteContactParser, executeDeleteContact],
    [addSomethingParser, executeAddSomething],
    [deleteSomethingParser, executeDeleteSomething],
    [selectParser, executeSelect],
    [deleteContactsWhereParser, executeDeleteContactsWhere]
]

function executeQuery(query) {
    if (typeof query !== "string") {
        throw new TypeError("String expected.");
    }

    let maxError = -1;

    for (let parserNexec of parsersNexecs) {
        const parser = parserNexec[0];
        const exec = parserNexec[1];

        const p = parser.parse(query, 0);

        if (!p.error) {
            return { success: exec(p)} ;
        } else {
            maxError = Math.max(maxError, p.end);
        }
    }

    return { error: true, end: maxError };
}


/**
 * Телефонная книга
 */
const phoneBook = new Map();


/**
 * Вызывайте эту функцию, если есть синтаксическая ошибка в запросе
 * @param {number} lineNumber – номер строки с ошибкой
 * @param {number} charNumber – номер символа, с которого запрос стал ошибочным
 */
function syntaxError(lineNumber, charNumber) {
    throw new Error(`SyntaxError: Unexpected token at ${lineNumber}:${charNumber}`);
}

/**
 * Выполнение запроса на языке pbQL
 * @param {string} query
 * @returns {string[]} - строки с результатами запроса
 */
function run(query) {
    let shouldKillLast = false;
    if (query[query.length - 1] && query[query.length - 1] !== ';') {
        shouldKillLast = true;
    }

    let splitted = query.split(';')

    let result = []

    if (splitted.length > 1) {
        splitted = splitted.map(s => s + ';');

        if (!shouldKillLast) {
            splitted.pop();
        }

        for (let i = 0; i < splitted.length; i++) {
            if (splitted[i] === '') {
                continue;
            }

            let res = executeQuery(splitted[i]);

            if (res.error) {
                if (splitted[i][res.end] === 'и' && splitted[i][res.end + 1] === ' ' &&
                    splitted[i][res.end - 1] === ' '
                    && splitted[i].substr(res.end - 1 - 'Добавь'.length, 'Добавь'.length) !== 'Добавь') {
                    syntaxError(i + 1, res.end + 3);
                } else {
                    syntaxError(i + 1, res.end + 1);
                }
            } else {
                if (i === splitted.length - 1 && shouldKillLast) {
                    syntaxError(splitted.length, splitted[splitted.length - 1].length);
                }

                if (res.success) {
                    result.push(...res.success);
                }
            }
        }
    } else if (query !== '') {
        const res = executeQuery(query);
        if (res.error) {
            syntaxError(1, res.end + 1);
        }
    }


    return result;
}


module.exports = {phoneBook, run};
