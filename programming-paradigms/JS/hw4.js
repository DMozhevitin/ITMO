const abstractOperation = operation => (...operands) => (...values) => {
    let result = [];
    for (operand of operands) {
        result.push(operand(...values));
    }

    return operation(...result);
}

const variables = ['x', 'y', 'z'];

const variable = name => {
    let index = variables.indexOf(name);
    return (...values) => values[index];
};

const cnst = value => () => value;

const add = abstractOperation((a, b) => a + b);

const subtract = abstractOperation((a, b) => a - b);

const multiply = abstractOperation((a, b) => a * b);

const divide = abstractOperation((a, b) => a / b);

const negate = abstractOperation(a => -a);

const abs = abstractOperation(a => Math.abs(a));

const iff = abstractOperation((a, b, c) => a >= 0 ? b : c);

const avg5 = abstractOperation((...operands) => {
    let sum = operands.reduce((sum, currentSum) => sum + currentSum);
    return sum / operands.length;
});

const med3 = abstractOperation((...operands) => {
    operands.sort((a, b) => a- b);
    return operands[1];
});

const pi = cnst(Math.PI);

const e = cnst(Math.E);

const one = cnst(1);

const two = cnst(2);


function isDigit(character) {
    return character >= "0" && character <= "9";
}

const operations = {
    '+'      : [add, 2],
    '-'      : [subtract, 2],
    '*'      : [multiply, 2],
    '/'      : [divide, 2],
    'negate' : [negate, 1],
    'abs'    : [abs, 1],
    'iff'    : [iff, 3],
    'med3'   : [med3, 3],
    'avg5'   : [avg5, 5]
};

const constants = {
    'pi' : pi,
    'e' : e,
    'one' : one,
    'two' : two
}

const vars = {
    "x" : variable("x"),
    "y" : variable("y"),
    "z" : variable("z")
}

const parse = expression => (...values) => {
    let tokens = expression.split(/\s+/);
    let stack = [];
    tokens.map(function(token) {
        if (token in operations) {
            let args = [];
            let len = stack.length;
            stack.slice(len - operations[token][1], len).forEach(function () {
                args.push(stack.pop());
            });
            args.reverse();
            stack.push(operations[token][0](...args));
        } else if (isDigit(token[0]) || (token[0] === '-' && token.length !== 1)) {
            stack.push(cnst(parseInt(token)));
        } else if (token in vars) {
            stack.push(vars[token]);
        } else if(token in constants) {
            stack.push(constants[token]);
        }
    })

    return stack.pop()(...values);
};