////////////////////////////////// Expressions////////////////////////////////////////

type Value = number
    | boolean
    | Value[];

type MemoryEntry = {
    type: string;
    writeable: boolean
    value: Value | undefined,
    funcBody: Expression | undefined,
    params: string[],
}

let output: Value[] = []; // for tests only


class Memory {
    #values: Map<string, MemoryEntry>;

    constructor(vals: Map<string, MemoryEntry> | undefined = undefined) {
        // we care about a new memory being created vs one we want to shadow
        // if we're not shadowing, we have to create the memory Map and built ins
        if (vals === undefined) {
            this.#values = new Map<string, MemoryEntry>();

            this.writeToMemory(new Identifier('π'), Math.PI);

            // functions
            this.setFunction(new Identifier("sqrt"), ["x"], new BuiltInExpressionFunction(Math.sqrt, ["x"]));
            this.setFunction(new Identifier("sin"), ["x"], new BuiltInExpressionFunction(Math.sin, ["x"]));
            this.setFunction(new Identifier("cos"), ["x"], new BuiltInExpressionFunction(Math.cos, ["x"]));
            this.setFunction(new Identifier("exp"), ["x"], new BuiltInExpressionFunction(Math.exp, ["x"]));
            this.setFunction(new Identifier("ln"), ["x"], new BuiltInExpressionFunction(Math.log, ["x"]));
            this.setFunction(new Identifier("hypot"), ["x", "y"], new BuiltInExpressionFunction(Math.hypot, ["x", "y"]));

        } else {
            this.#values = vals;
        }
    }

    writeToMemory(i: Identifier, val: Value) {
        // Creating a number | boolean | List. Functions go through another path.
        const entry: MemoryEntry = {
            funcBody: undefined,
            params: [],
            type: typeof val, // type is umber | boolean | object, where object is a list.
            value: val,
            writeable: true, // variables (this code path) are always writable

        };
        // console.log(`Memory: setting ${i.name} to val: ${val} with type: ${typeof val} with permission ${permission}`);
        this.#values.set(i.name, entry);
    }

    getFromMemory(i: Identifier): Value {
        const memoryEntry: MemoryEntry = this.#getMemoryEntry(i);

        const val: Value | undefined = memoryEntry.value;

        if (val === undefined) {
            throw new Error(`${i.name} trying to get a function as a variable.`);
        }

        return val;
    }

    getFunction(i: Identifier): MemoryEntry {
        const fn: MemoryEntry = this.#getMemoryEntry(i);
        if (fn.type !== 'FUN') {
            throw new Error(`${i.name} is not a function.`);
        }

        return fn;
    }

    setFunction(i: Identifier, args: string[], e: Expression) {
        if (this.identifierExists(i)) {
            throw new Error(`${i.name} already defined. Cannot redefine.`);
        }
        // Creating a Function. number | boolean | List go through another path.
        const entry: MemoryEntry = {
            funcBody: e,
            params: args,
            type: 'FUN', // type is FUN for function
            value: undefined,
            writeable: false, // functions (this code path) are never writable
        };

        this.#values.set(i.name, entry);
    }

    identifierExists(i: Identifier): boolean {
        return this.#values.has(i.name);
    }

    getType(i: Identifier): string {
        const memoryEntry: MemoryEntry = this.#getMemoryEntry(i);

        return memoryEntry.type;
    }

    isLocationWritable(i: Identifier): boolean {
        const memoryEntry: MemoryEntry = this.#getMemoryEntry(i);

        return memoryEntry.writeable;
    }

    #getMemoryEntry(i: Identifier) {
        const memoryEntry: MemoryEntry | undefined = this.#values.get(i.name);
        if (memoryEntry === undefined) {
            throw new Error(`Function ${i.name} not defined.`);
        }

        return memoryEntry;
    }

    shadow(): Memory {
        const newValues: Map<string, MemoryEntry> = new Map<string, MemoryEntry>(this.#values);

        return new Memory(newValues);
    }
}


interface Expression {
    interpret(env: Memory): Value;
}

/*
    Wrapper expression for built ins passes in the expected param names we can pull out of memory, which has been set by
    the regular FunctionCallExpression flow.
 */
class BuiltInExpressionFunction implements Expression {
    constructor(public jsFunc: Function, public paramNames: string[]) {
    }

    interpret(env: Memory): Value {
        const params: number[] = [];
        for (const paramName of this.paramNames) {
            const res = env.getFromMemory(new Identifier(paramName));
            if (typeof res !== "number") {
                throw new Error("Built in types require number params");
            }
            params.push(res);
        }
        return this.jsFunc(...params);
    }
}

class Identifier implements Expression {
    constructor(public name: string) {
    }

    interpret(env: Memory): Value {
        if (!env.identifierExists(this)) {
            throw new Error(`Identifier ${this.name} does not exist.`);
        }
        const type: string = env.getType(this);
        const allowedTypes: string[] = ['number', 'boolean', 'object'];
        if (!allowedTypes.includes(type)) {
            throw new Error(`${this.name} not in ${allowedTypes}. Got ${type}`);
        }

        return env.getFromMemory(this);
    }
}

class Numeral implements Expression {
    constructor(public numerial: number) {
    }

    interpret(env: Memory): number {
        return this.numerial;
    }
}

class ListExpression implements Expression {
    constructor(public expressions: Expression[]) {
    }

    interpret(env: Memory): Value {
        const res = [];
        for (const exp of this.expressions) {
            res.push(exp.interpret(env));
        }

        return res;
    }
}

class BellaBoolean implements Expression {
    constructor(public boolval: boolean) {
    }

    interpret(env: Memory): boolean {
        return this.boolval;
    }
}

class UnaryOpExpression implements Expression {
    constructor(public op: "!" | "-", public expression: Expression) {
    }

    interpret(env: Memory): Value {

        const res = this.expression.interpret(env);
        const typeOf: string = typeof res;

        switch (this.op) {
            case "-":
                if (typeOf != "number") {
                    throw new Error(`Unary Operator ${this.op} cannot be applied to ${typeOf}.`);
                }
                return -res;
            case "!":
                if (typeOf != "boolean") {
                    throw new Error(`Unary Operator ${this.op} cannot be applied to ${typeOf}.`);
                }
                return !res;
            default:
                // In case this is set incorrectly during runtime
                throw new Error("Unknown unary operator");
        }
    }
}

class BinaryOpExpression implements Expression {
    constructor(public op: "+" | "-" | "*" | "/" | "%" | "**" | "<" | "<=" | "==" | "!=" | ">=" | ">" | "&&" | "||",
                public left: Expression, public right: Expression) {
    }

    interpret(env: Memory): Value {

        const resLeft: Value = this.left.interpret(env);
        const resRight: Value = this.right.interpret(env);

        switch (this.op) {
            case "+":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft + resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "-":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft - resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "*":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft * resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "/":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    if (resRight === 0) {
                        throw new Error("Divide by zero.");
                    }
                    return (resLeft / resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "%":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft % resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "**":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (Math.pow(resLeft, resRight));
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "<":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft < resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "<=":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft <= resRight)
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "==":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft === resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "!=":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft !== resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case ">=":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft >= resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case ">":
                if ((typeof resLeft === "number" && typeof resRight === "number")) {
                    return (resLeft > resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "&&":
                if ((typeof resLeft === "boolean" && typeof resRight === "boolean")) {
                    return (resLeft && resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            case "||":
                if ((typeof resLeft === "boolean" && typeof resRight === "boolean")) {
                    return (resLeft || resRight);
                }
                throw new Error(`Incorrect type for op: ${this.op}`);
            default:
                // In case this is set incorrectly during runtime
                throw new Error("Unknown binary operator");
        }
    }
}

class TernaryExpression implements Expression {
    constructor(public test: Expression, public first: Expression, public second: Expression) {
    }

    interpret(env: Memory): Value {
        const res = this.test.interpret(env);
        if (!(typeof res == "boolean")) {
            throw new Error(`Invalid test type given.`)
        }

        if (res) {
            return this.first.interpret(env);
        } else {
            return this.second.interpret(env);
        }
    }
}

class SubscriptExpression implements Expression {
    constructor(public listExp: Expression, public indexExp: Expression) {
    }

    interpret(env: Memory): Value {
        const list = this.listExp.interpret(env);
        if (!(typeof list === "object")) {
            throw new Error(`List expression is not a list.`);
        }
        const index = this.indexExp.interpret(env);
        if (!(typeof index === "number")) {
            throw new Error(`Index expression is not a number.`);
        }

        const res = list[index];
        return res;
    }

}

class FunctionCallExpression implements Expression {
    constructor(public id: Identifier, public args: Expression[]) {
    }

    interpret(env: Memory): Value {
        const fnMemory: MemoryEntry = env.getFunction(this.id);
        const shadowedMemory: Memory = env.shadow();

        if (fnMemory.funcBody === undefined) {
            // This cannot be undefined because we validate that this memory is a function
            // in `env.getFunction` but let's make the compiler happy without using `!`
            throw new Error('Function body not defined.');
        }

        if (this.args.length != fnMemory.params.length) {
            throw new Error("Argument count differs.");
        }
        const fnBody: Expression = fnMemory.funcBody;

        // Now we have to apply the args into the shadow memory
        for (let i = 0; i < this.args.length; i++) {
            shadowedMemory.writeToMemory(
                new Identifier(fnMemory.params[i]),
                this.args[i].interpret(env)
            );
        }

        // call the method body with the shadow memory
        const res = fnBody.interpret(shadowedMemory);

        // since functions cannot have statements, only expressions
        // functions cannot create variables, so no need to do anything
        // with the shadow memory

        return res;
    }


}

/////////////////////////// Statements //////////////////////////////////////

interface Statement {
    interpret(env: Memory): void;
}

class FunctionDeclaration implements Statement {
    constructor(public id: Identifier, public args: string[], public body: Expression) {
    }

    interpret(env: Memory): void {
        if (env.identifierExists(this.id)) {
            throw new Error(`Identifier ${this.id.name} already declared.`);
        }

        env.setFunction(this.id, this.args, this.body);
    }
}

class WhileStatement implements Statement {
    constructor(public check: Expression, public body: Block) {
    }

    interpret(env: Memory): void {
        while (true) {
            const res: Value = this.check.interpret(env);
            if (!(typeof res === "boolean")) {
                throw new Error(`Invalid test type given.`)
            }
            if (!res) {
                return;
            }
            for (let statement of this.body.statements) {
                statement.interpret(env);
            }
        }
    }
}

class AssignmentStatement implements Statement {
    constructor(public target: Identifier, public source: Expression) {
    }

    interpret(env: Memory) {
        if (!env.identifierExists(this.target)) {
            throw new Error(`Variable ${this.target.name} not declared.`);
        }

        if (!env.isLocationWritable(this.target)) {
            throw new Error(`Variable ${this.target.name} not writable.`);
        }

        // We don't check the type because functions are RO so will be caught
        // by the above check
        env.writeToMemory(this.target, this.source.interpret(env));
    }
}

class VariableDeclaration implements Statement {
    constructor(public id: Identifier, public initializer: Expression) {
    }

    interpret(env: Memory): void {
        if (env.identifierExists(this.id)) {
            throw new Error(`Variable ${this.id.name} already declared.`);
        }

        env.writeToMemory(this.id, this.initializer.interpret(env));
    }
}

class PrintStatement implements Statement {
    constructor(public source: Expression) {
    }

    interpret(env: Memory): void {
        const res = this.source.interpret(env);
        output.push(res);
        console.log(res);
    }
}

class Program {
    #env: Memory;

    constructor(public body: Block) {
        this.#env = new Memory();
        // Clean up output for tests
        output = [];
    }

    evaluate(): void {
        for (let statement of this.body.statements) {
            statement.interpret(this.#env);
        }
    }
}

class Block {
    constructor(public statements: Statement[]) {
    }
}

function interpret(program: Program): void {
    program.evaluate();
}


///////////////////////////////////////////////// Testing ///////////////////////////////////////////////
// quick unit tester
function testSuccess(programName: Program, expectedOutput: any[]) {
    // Modified from https://stackoverflow.com/a/16436975
    function arraysEqual(a: any[], b: any[]): boolean {
        if (a === b) return true;
        if (a.length !== b.length) return false;

        for (let i: number = 0; i < a.length; ++i) {
            if (a[i] !== b[i]) return false;
        }
        return true;
    }

    interpret(programName);
    if (!arraysEqual(expectedOutput, output)) {
        throw new Error(`Expected output: [${expectedOutput}] and program output: [${output}] do not match`);
    }
}

function testError(programName: Program, errorReason: string) {
    try {
        interpret(programName);
    } catch (e: any) { // From the compiler
        if (e.message !== errorReason) {
            console.log(`ERROR NOT CAUGHT ${e.message}`);
            throw e; // Get stack trace
        }

        // We got an error - either expected or not, but the program was not successful
        return;
    }
    console.log(`Got successful program despite expecting error: ${errorReason}`);
}

// success programs
const simplePrintStatement: Program = new Program(
    new Block([
        new PrintStatement(new Numeral(5))
    ])
);
testSuccess(simplePrintStatement, [5]);

const simpleAssignment: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(10)),
        new AssignmentStatement(new Identifier("x"), new Numeral(20)),
        new PrintStatement(new Identifier("x")),
    ])
);
testSuccess(simpleAssignment, [20]);

const simpleVariableDeclaration: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(10)),
        new PrintStatement(new Identifier("x")),
        new VariableDeclaration(new Identifier("y"), new BellaBoolean(false)),
        new PrintStatement(new Identifier("y")),
    ])
);
testSuccess(simpleVariableDeclaration, [10, false]);

const simpleUnaryOpExpressionOne: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(10)),
        new AssignmentStatement(new Identifier("x"), new UnaryOpExpression("-", new Identifier("x"))),
        new PrintStatement(new Identifier("x")),
    ])
);
testSuccess(simpleUnaryOpExpressionOne, [-10]);

const simpleUnaryOpExpressionTwo: Program = new Program(
    new Block([
        new PrintStatement(new UnaryOpExpression("-", new Numeral(10)))
    ])
);
testSuccess(simpleUnaryOpExpressionTwo, [-10]);

const simpleUnaryOpExpressionThree: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new BellaBoolean(true)),
        new AssignmentStatement(new Identifier("x"), new UnaryOpExpression("!", new Identifier("x"))),
        new PrintStatement(new Identifier("x")),
    ])
);
testSuccess(simpleUnaryOpExpressionThree, [false]);

const simpleUnaryOpExpressionFour: Program = new Program(
    new Block([
        new PrintStatement(new UnaryOpExpression("!", new BellaBoolean(true)))
    ])
);
testSuccess(simpleUnaryOpExpressionFour, [false]);

const simpleBOpExpressionAdd: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("+", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionAdd, [20]);

const simpleBOpExpressionSub: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("-", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionSub, [0]);

const simpleBOpExpressionMul: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("*", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionMul, [100]);

const simpleBOpExpressionDiv: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("/", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionDiv, [1]);

const simpleBOpExpressionComp1: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("<", new Numeral(10), new Numeral(9)))
    ])
);
testSuccess(simpleBOpExpressionComp1, [false]);

const simpleBOpExpressionComp2: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("<=", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionComp2, [true]);

const simpleBOpExpressionComp3: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("==", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionComp3, [true]);

const simpleBOpExpressionComp4: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression("!=", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionComp4, [false]);

const simpleBOpExpressionComp5: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression(">", new Numeral(10), new Numeral(9)))
    ])
);
testSuccess(simpleBOpExpressionComp5, [true]);

const simpleBOpExpressionComp6: Program = new Program(
    new Block([
        new PrintStatement(new BinaryOpExpression(">=", new Numeral(10), new Numeral(10)))
    ])
);
testSuccess(simpleBOpExpressionComp6, [true]);


const simpleTernaryExpressionExpressionOne: Program = new Program(
    new Block([
        new PrintStatement(new TernaryExpression(new BellaBoolean(true), new Numeral(1), new Numeral(2)))
    ])
);
testSuccess(simpleTernaryExpressionExpressionOne, [1]);

const simpleTernaryExpressionExpressionTwo: Program = new Program(
    new Block([
        new PrintStatement(new TernaryExpression(new BellaBoolean(false), new Numeral(1), new Numeral(2)))
    ])
);
testSuccess(simpleTernaryExpressionExpressionTwo, [2]);

const simpleListAssignment: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new ListExpression(
            [new Numeral(1), new Numeral(2), new BellaBoolean(false)]
        )),
        new PrintStatement(new Identifier("x"))
    ])
);
// Expected output and output do not match 1,2,false 1,2,false
// tests fine but the test runner is flaky on this test
// testSuccess(simpleListAssignment, [[1,2,false]]);

const simpleListIndex: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new ListExpression(
            [new Numeral(1), new Numeral(2), new BellaBoolean(false)]
        )),
        new PrintStatement(new SubscriptExpression(new Identifier("x"), new Numeral(1)))
    ])
);
testSuccess(simpleListIndex, [2]);

const simpleWhileLoop: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new BellaBoolean(true)),
        new WhileStatement(new Identifier("x"), new Block([
            new PrintStatement(new Identifier("x")),
            new AssignmentStatement(new Identifier("x"), new BellaBoolean(false))
        ]))
    ])
);

testSuccess(simpleWhileLoop, [true]);

const complexWhileLoop: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(3)),
        new WhileStatement(new BinaryOpExpression(">", new Identifier("x"), new Numeral(0)), new Block([
            new PrintStatement(new Identifier("x")),
            new AssignmentStatement(new Identifier("x"), (new BinaryOpExpression("-", new Identifier("x"), new Numeral(1))))
        ]))
    ])
);

testSuccess(complexWhileLoop, [3, 2, 1]);

const simpleFunctionDec: Program = new Program(
    new Block([
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
    ])
);

testSuccess(simpleFunctionDec, []);

const simpleFunctionCall: Program = new Program(
    new Block([
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
        new PrintStatement((new FunctionCallExpression(new Identifier("x"), []))),
    ])
);

testSuccess(simpleFunctionCall, [3]);

const testShadowMemory: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("y"), new Numeral(10)),
        new FunctionDeclaration(new Identifier("x"), ["y"], new Numeral(3)),
        new PrintStatement((new FunctionCallExpression(new Identifier("x"), [new Numeral(10)]))),
        new PrintStatement(new Identifier("y")),
    ])
);

testSuccess(testShadowMemory, [3, 10]);

const simplePiTest: Program = new Program(
    new Block([
        new PrintStatement(new Identifier("π")),
    ])
);
testSuccess(simplePiTest, [3.141592653589793]);

const simpleHypotTest: Program = new Program(
    new Block([
        new PrintStatement((new FunctionCallExpression(new Identifier("hypot"), [new Numeral(3), new Numeral(4)]))),
    ])
);
testSuccess(simpleHypotTest, [5]);


/////////////////////////////////////////// ERROR TEST CASES ////////////////////////////////////////////

//these programs intentionally generate an error

const doubleVariableDeclaration: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(10)),
        new VariableDeclaration(new Identifier("x"), new Numeral(1)),
    ])
);
testError(doubleVariableDeclaration, "Variable x already declared.");

const variableNotDeclared: Program = new Program(
    new Block([
        new PrintStatement(new Identifier("y")),
    ])
);
testError(variableNotDeclared, "Identifier y does not exist.");

const errorUnaryOpExpressionOne: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(10)),
        new UnaryOpExpression("!", new Identifier("x")),
        new PrintStatement(new Identifier("x")),
    ])
);
testError(errorUnaryOpExpressionOne, "Unary Operator ! cannot be applied to number.");

const errorUnaryOpExpressionTwo: Program = new Program(
    new Block([
        new PrintStatement(new UnaryOpExpression("!", new Numeral(10)))
    ])
);
testError(errorUnaryOpExpressionTwo, "Unary Operator ! cannot be applied to number.");

const errorUnaryOpExpressionThree: Program = new Program(
    new Block([
        new PrintStatement(new UnaryOpExpression("!", new Numeral(10)))
    ])
);
testError(errorUnaryOpExpressionThree, "Unary Operator ! cannot be applied to number.");

const errorUnaryOpExpressionFour: Program = new Program(
    new Block([
        new PrintStatement(new UnaryOpExpression("-", new BellaBoolean(true)))
    ])
);
testError(errorUnaryOpExpressionFour, "Unary Operator - cannot be applied to boolean.");

const assignmentWithoutDeclare: Program = new Program(
    new Block([
        new AssignmentStatement(new Identifier("x"), new Numeral(20)),
    ])
);
testError(assignmentWithoutDeclare, "Variable x not declared.");

const simpleTernaryExpressionExpressionError: Program = new Program(
    new Block([
        new PrintStatement(new TernaryExpression(new Numeral(1), new Numeral(1), new Numeral(2)))
    ])
);
testError(simpleTernaryExpressionExpressionError, "Invalid test type given.");

const listIsNotList: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(1)),
        new PrintStatement(new SubscriptExpression(new Identifier("x"), new Numeral(1)))
    ])
);
testError(listIsNotList, "List expression is not a list.");

const indexIsNotNumber: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new ListExpression(
            [new Numeral(1), new Numeral(2), new BellaBoolean(false)]
        )),
        new PrintStatement(new SubscriptExpression(new Identifier("x"), new BellaBoolean(true)))
    ])
);
testError(indexIsNotNumber, "Index expression is not a number.");


const functionCreateIdExists: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(10)),
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(20)),
    ])
);
testError(functionCreateIdExists, "Identifier x already declared.");

const doubleFunctionDec: Program = new Program(
    new Block([
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
    ])
)
testError(doubleFunctionDec, "Identifier x already declared.");

const funcDecOverVariable: Program = new Program(
    new Block([
        new VariableDeclaration(new Identifier("x"), new Numeral(3)),
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
    ])
)
testError(funcDecOverVariable, "Identifier x already declared.");


const funcCallDoesNotExist: Program = new Program(
    new Block([
        new FunctionCallExpression(new Identifier("x"), []),
    ])
)
testError(funcCallDoesNotExist, "Function x not defined.");

const funcCallDifferentArgCountOne: Program = new Program(
    new Block([
        new FunctionDeclaration(new Identifier("x"), ['x'], new Numeral(3)),
        new FunctionCallExpression(new Identifier("x"), []),
    ])
)
testError(funcCallDifferentArgCountOne, "Argument count differs.");

const funcCallDifferentArgCountTwo: Program = new Program(
    new Block([
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
        new FunctionCallExpression(new Identifier("x"), [new Numeral(10)]),
    ])
)
testError(funcCallDifferentArgCountTwo, "Argument count differs.");

const writeVariableToFnName: Program = new Program(
    new Block([
        new FunctionDeclaration(new Identifier("x"), [], new Numeral(3)),
        new AssignmentStatement(new Identifier("x"), new Numeral(10)),
    ])
)
testError(writeVariableToFnName, "Variable x not writable.");


console.log("All tests pass - yay!");