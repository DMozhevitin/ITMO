const Exception = function (name, Message) {
    const result = function (...args) {
        this.message = Message(...args);
        this.name = name;
    };
    result.prototype = new Error;
    return result;
};

const MissingOperationException = Exception(
    "MissingOperationException",
    index => "Missed operation at index " + index
);

const MissingOperandException = Exception(
    "MissingOperandException",
    symbol => "Not enough operands for operation" + symbol
);

const TooMuchOperandsException = Exception(
    "TooMuchOperandsException",
    symbol => "Too much operands for operation " + symbol
);

const MissingCloseBracketException = Exception(
    "MissingCloseBracketException",
    index => "Missed close bracket at index " + index
);

const UnexpectedSymbolException = Exception(
    "UnexpectedSymbolException",
    symbol => "Unexpected symbol : " + symbol
);

const Const = function (value) {
    this.getValue = function () {
        return value;
    }
};

Const.prototype.evaluate = function () {
    return this.getValue();
};

Const.prototype.toString = function () {
    return this.getValue().toString();
};

Const.prototype.prefix = Const.prototype.toString;
Const.prototype.postfix = Const.prototype.toString;

const ZERO = new Const(0);
const ONE = new Const(1);
const TWO = new Const(2);

Const.prototype.diff = function () {
    return ZERO;
};

const VARIABLES = {
    "x": 0,
    "y": 1,
    "z": 2
};

const Variable = function (name) {
    this.getName = function () {
        return name;
    };
};

Variable.prototype.evaluate = function (...variablesValue) {
    return variablesValue[VARIABLES[this.getName()]];
};

Variable.prototype.toString = function () {
    return this.getName();
};

Variable.prototype.prefix = Variable.prototype.toString;
Variable.prototype.postfix = Variable.prototype.toString;

Variable.prototype.diff = function (name) {
    if (this.getName() === name) {
        return ONE;
    } else {
        return ZERO;
    }
};

const Operation = function (...operands) {
    this.getOperands = function () {
        return operands;
    }
};

Operation.prototype.evaluate = function (...variableValues) {
    const result = this.getOperands().map(function (operand) {
        return operand.evaluate(...variableValues);
    });
    return this.operationRule(...result);
};

Operation.prototype.toString = function () {
    return this.getOperands().join(" ") + " " + this.getSymbol();
};

Operation.prototype.prefix = function () {
    return "(" + this.getSymbol() + " " + this.getOperands().map(function (operand) {
        return operand.prefix();
    }).join(" ") + ")";
};

Operation.prototype.postfix = function () {
    return "(" + this.getOperands().map(function (operand) {
        return operand.postfix();
    }).join(" ") + " " + this.getSymbol() + ")";
};

Operation.prototype.diff = function (...variableValues) {
    const diffs = this.getOperands().map(function (operand) {
        return operand.diff(...variableValues);
    });
    return this.diffRule(this.getOperands(), diffs);
};

const createOperation = function (operationRule, diffRule, symbol) {
    const result = function (...operands) {
        Operation.apply(this, operands);
    };
    result.prototype = new Operation;
    result.prototype.operationRule = operationRule;
    result.prototype.diffRule = diffRule;
    result.prototype.getSymbol = function () {
        return symbol;
    };
    return result;
};

const Add = createOperation(
    (a, b) => a + b,
    (operands, diffs) => new Add(diffs[0], diffs[1]),
    "+");

const Subtract = createOperation(
    (a, b) => a - b,
    (operands, diffs) => new Subtract(diffs[0], diffs[1]),
    "-"
);

const Multiply = createOperation(
    (a, b) => a * b,
    (operands, diffs) => new Add(
        new Multiply(operands[0], diffs[1]),
        new Multiply(operands[1], diffs[0])
    ),
    "*"
);

const Divide = createOperation(
    (a, b) => a / b,
    (operands, diffs) => new Divide(
        new Subtract(
            new Multiply(operands[1], diffs[0]),
            new Multiply(operands[0], diffs[1])),
        new Multiply(operands[1], operands[1])
    ),
    "/"
);
const Negate = createOperation(
    a => -a,
    (operands, diffs) => new Negate(diffs[0]),
    "negate"
);

const Sum = createOperation(
    (...a) => a.reduce(function (sum, current) {
        return sum + current;
    }, 0),
    (operands, diffs) => new Sum(...diffs),
    "sum"
);

const Sumsq = createOperation(
    (...a) => Sum.prototype.operationRule(...a.map(x => x * x)),
    (operands, diffs) => Sum.prototype.diffRule(operands, diffs.map(function (current, index) {
        return new Multiply(TWO, new Multiply(current, operands[index]));
    })),
    "sumsq"
);

const Length = createOperation(
    (...a) => Math.sqrt(Sumsq.prototype.operationRule(...a)),
    (operands, diffs) => operands.length !== 0 ?
        new Divide(Sumsq.prototype.diffRule(operands, diffs), new Multiply(TWO, new Length(...operands))) :
        ZERO,
    "length"
);

const isDigit = digit => digit >= "0" && digit <= "9";

const isNumber = function (number) {
    let index = 0;
    if (number[index] === "-") {
        index++;
        if (number.length === 1) {
            return false;
        }
    }
    if (index === number.length) {
        return false;
    }
    while (isDigit(number[index]) && index < number.length) {
        index++;
    }
    return index === number.length;
};

const OPERATIONS = {
    "+": [Add, 2],
    "-": [Subtract, 2],
    "/": [Divide, 2],
    "*": [Multiply, 2],
    "negate": [Negate, 1],
    "sumsq": [Sumsq, 0],
    "length": [Length, 0]
};

const Parser = function (expression, parse) {
    this.parse = parse;
    this.expression = expression;
    this.currentIndex = 0;
    this.currentToken = '';
};

Parser.prototype.nextToken = function () {
    if (this.expression[this.currentIndex] === "(" || this.expression[this.currentIndex] === ")") {
        res = this.expression[this.currentIndex];
        this.currentIndex++;
    } else {
        let stopIndex = this.currentIndex;
        while (this.expression[stopIndex] !== "(" && this.expression[stopIndex] !== ")" &&
        this.expression[stopIndex] !== " " && stopIndex < this.expression.length) {
            stopIndex++;
        }
        res = this.expression.slice(this.currentIndex, stopIndex);
        this.currentIndex = stopIndex;
    }
    this.skipWhitespace();
    this.currentToken = res;
};

Parser.prototype.skipWhitespace = function () {
    while (this.currentIndex < this.expression.length && this.expression[this.currentIndex] === " ") {
        this.currentIndex++;
    }
};

const parsePrefix = function (expression) {
    let parser = new Parser(expression, function () {
            if (this.currentToken === "(") {
                this.nextToken();
                if (this.currentToken in OPERATIONS) {
                    let args = [];
                    let operation = this.currentToken;
                    this.nextToken();
                    while (this.currentToken !== ")" && this.currentIndex < expression.length) {
                        args.push(this.parse());
                        this.nextToken();
                    }
                    if (this.currentToken !== ")") {
                        throw new MissingCloseBracketException(this.currentIndex);
                    }
                    if (OPERATIONS[operation][1] > args.length) {
                        throw new MissingOperandException(operation);
                    } else if (OPERATIONS[operation][1] !== 0 && OPERATIONS[operation][1] < args.length) {
                        throw new TooMuchOperandsException(operation, this.currentIndex);
                    }
                    return new OPERATIONS[operation][0](...args);
                } else {
                    throw new MissingOperationException(this.currentIndex);
                }
            } else if (this.currentToken in VARIABLES) {
                return new Variable(this.currentToken);
            } else if (isNumber(this.currentToken)) {
                return new Const(parseInt(this.currentToken));
            } else {
                throw new UnexpectedSymbolException(this.currentToken, this.currentIndex);
            }
        }
    );
    parser.skipWhitespace();
    parser.nextToken();
    let result = parser.parse();
    if (parser.currentIndex < expression.length) {
        throw  new UnexpectedSymbolException(expression[parser.currentIndex]);
    }
    return result;
};

const parsePostfix = function (expression) {
    let parser = new Parser(expression, function () {
        if (this.currentToken === "(") {
            this.nextToken();
            let args = [];
            while (!(this.currentToken in OPERATIONS) && this.currentIndex < expression.length) {
                args.push(this.parse());
                this.nextToken();
            }
            if (this.currentToken in OPERATIONS) {
                let operation = this.currentToken;
                this.nextToken();
                if (this.currentToken !== ")") {
                    throw new MissingCloseBracketException(this.currentIndex);
                } else if (OPERATIONS[operation][1] > args.length) {
                    throw new MissingOperandException(operation);
                } else if (OPERATIONS[operation][1] !== 0 && OPERATIONS[operation][1] < args.length) {
                    throw new TooMuchOperandsException(operation, this.currentIndex);
                }
                return new OPERATIONS[operation][0](...args);
            } else {
                throw new MissingOperationException(this.currentIndex);
            }
        } else if (this.currentToken in VARIABLES) {
            return new Variable(this.currentToken);
        } else if (isNumber(this.currentToken)) {
            return new Const(parseInt(this.currentToken));
        } else {
            throw new UnexpectedSymbolException(this.currentToken, this.currentIndex);
        }
    });
    parser.skipWhitespace();
    parser.nextToken();
    let result = parser.parse();
    if (parser.currentIndex < expression.length) {
        throw  new UnexpectedSymbolException(expression[parser.currentIndex]);
    }
    return result;
};
