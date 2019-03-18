package Lithp;

import ParserCombinator.ParseTree;

import java.util.List;

public class LithpEvaluator {
    /** The scratch environment for running code */
    private LithpEnv globalEnv;

    public LithpEvaluator() {
        /* Stores all the builtin behaviors and values */
        LithpEnv builtinEnv = new LithpEnv();
        globalEnv = new LithpEnv();
        builtinEnv.loadBuiltins(this);
        globalEnv.setParent(builtinEnv);
    }

    public int eval(ParseTree AST) {
        LithpValue result = eval(globalEnv, LithpValue.read(AST.getRoot().getChild()));
        if (result.getType() == LithpValue.Type.ERR && result.getErr().equals("exit")) return -1;
        System.out.println(result);
        return 0;
    }

    private LithpValue evalSexpr(LithpEnv env, LithpValue value) {
        List<LithpValue> cells = value.getCells();
        if (cells.size() == 0) return value;
        LithpValue macroCheck = eval(env, cells.get(0));
        if (macroCheck.getType() == LithpValue.Type.MACRO) {
            value.pop(); // pull the macro out of the expression and evaluate args
            return macroCheck.getBuiltinFunction().apply(env, value);
        }
        if (cells.size() == 1) {
            return macroCheck;
        }
        cells.set(0, macroCheck);
        for (int i = 1; i < value.getCount(); i++) {
            cells.set(i, eval(env, cells.get(i)));
        } // evaluate all children
        for (LithpValue cell : value) {
            if (cell.getType() == LithpValue.Type.ERR) return cell;
        }

        LithpValue function = value.pop();
        if (function.getType() != LithpValue.Type.FUNC) {
            return LithpValue.err("S-expression does not start with function: " + value);
        }

        return builtinCall(env, function, value);
    }

    private LithpValue eval(LithpEnv env, LithpValue value) {
        if (value.getType() == LithpValue.Type.SYM) {
            return env.get(value);
        }
        if (value.getType() == LithpValue.Type.S_EXPR) {
            return evalSexpr(env, value);
        }
        return value;
    }

    // builtin macros
    LithpValue builtinIf(LithpEnv env, LithpValue args) {
        if (args.getCount() != 3) {
            return LithpValue.err("Function 'if' actual and formal argument lists differ in length.\n" +
                    "Formal: 3, Actual: " + args.getCount() + ".");
        }
        LithpValue cond = eval(env, args.pop());
        if (cond.getType() == LithpValue.Type.ERR) return cond;
        else {
            LithpValue temp = LithpValue.sexpr();
            temp.add(cond);
            cond = builtinBool(temp);
        }
        LithpValue ifBody = args.pop();
        LithpValue elseBody = args.pop();
        LithpEnv evaluationEnv = new LithpEnv();
        evaluationEnv.setParent(env);
        return cond.equals(LithpValue.TRUE) ? eval(evaluationEnv, ifBody) : eval(evaluationEnv, elseBody);
    }
    LithpValue builtinDef(LithpEnv env, LithpValue args) {
        return builtinVar(env, args, "def");
    }
    // TODO: let forms should evaluate their expr in a local environment
    LithpValue builtinLet(LithpEnv env, LithpValue args) {
        LithpValue expr = args.pop(args.getCount() - 1);
        builtinVar(env, args, "let");
        return eval(env, expr);
    }
    LithpValue builtinDefValues(LithpEnv env, LithpValue args) {
        return builtinVarValues(env, args, "def-values");
    }
    LithpValue builtinLetValues(LithpEnv env, LithpValue args) {
        LithpValue expr = args.pop(args.getCount() - 1);
        builtinVarValues(env, args, "let-values");
        return eval(env, expr);
    }
    private LithpValue builtinVarValues(LithpEnv env, LithpValue args, String func) {
        if (args.getCount() == 0) {
            return LithpValue.VOID;
        }
        if (args.getCount() != 2) {
            return LithpValue.err("Function 'def-values' actual and formal argument lists differ in length.\n" +
                    "Formal: 2, Actual: " + args.getCount() + ".");
        }
        LithpValue sym = args.pop();
        if (sym.getType() == LithpValue.Type.SYM) {
            if (env.contains(sym)) {
                sym = env.get(sym);
            } else {
                return builtinVar(env, sym, func);
            }
        }
        LithpValue.Type listType = sym.getType();
        if (listType != LithpValue.Type.Q_EXPR && listType != LithpValue.Type.S_EXPR) {
            return LithpValue.err("Function 'def-values' passed incorrect type for first argument.\n" +
                    "Expected Symbol or List, found " + typeName(listType) + ": " + sym + ".");
        }
        LithpValue valueList = eval(env, args.pop());
        if (valueList.getType() != LithpValue.Type.Q_EXPR) {
            return LithpValue.err("Function 'def-values' passed incorrect type for second argument.\n" +
                    "Expected List, found " + typeName(valueList.getType()) + ": " + valueList + ".");
        }
        if (sym.getCount() != valueList.getCount()) {
            return LithpValue.err("Function 'def-values' passed lists of different length.\n" +
                    "Keys: " + sym +"\n" +
                    "Values: " + valueList);
        }
        List<LithpValue> keys = sym.getCells();
        List<LithpValue> values = valueList.getCells();
        if (func.equals("def-values")) {
            for (int i = 0; i < sym.getCount(); i++) {
                env.def(keys.get(i), eval(env, values.get(i)));
            }
        }
        if (func.equals("let-values")) {
            for (int i = 0; i < sym.getCount(); i++) {
                env.put(keys.get(i), eval(env, values.get(i)));
            }
        }
        return LithpValue.VOID;
    }
    private LithpValue builtinVar(LithpEnv env, LithpValue args, String func) {
        if (args.getCount() == 0) {
            return LithpValue.VOID;
        }
        LithpValue sym = args.pop();
        //<editor-fold desc="Error checking">
        if (args.getCount() != 1) { // symbol arg popped already
            return LithpValue.err("Function '"+func+"' actual and formal arguments lists differ in length.\n" +
                    "Formal: 2, Actual: " + (args.getCount() + 1) + ".");
        }
        LithpValue.Type listType = sym.getType();
        if (listType == LithpValue.Type.S_EXPR) {
            sym = eval(env, sym);
            listType = sym.getType();
            if (listType == LithpValue.Type.Q_EXPR && sym.getCount() == 1 && sym.get(0).getType() == LithpValue.Type.SYM) {
                sym = sym.pop();
                listType = LithpValue.Type.SYM;
            }
        }
        if (listType != LithpValue.Type.SYM) {
            return LithpValue.err("Function '"+func+"' passed incorrect type for first argument.\n" +
                    "Expected Symbol, found " + typeName(listType) + ": " + sym + ".");
        }
        //</editor-fold>;
        if (func.equals("def")) env.def(sym, eval(env, args.pop()));
        if (func.equals("let")) env.put(sym, eval(env, args.pop()));
        return LithpValue.VOID;
    }
    LithpValue builtinQuote(LithpValue arg) {
        //<editor-fold desc="Error checking">
        if (arg.getCount() != 1) {
            return LithpValue.err("Function 'quote' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        LithpValue sexp = arg.pop();
        if (sexp.getType() != (LithpValue.Type.S_EXPR)) {
            return LithpValue.err("Function 'quote' passed incorrect type.\n" +
                    "Expected S-Expression, found " + typeName(sexp) + ": " + sexp + ".");
        }
        //</editor-fold>
        sexp.setType(LithpValue.Type.Q_EXPR);
        return sexp;
    }
    LithpValue builtinExit() {
        return LithpValue.exit();
    }
    // builtin functions
    LithpValue builtinLambda(LithpEnv creator, LithpValue args) {
        if (args.getCount() != 2) {
            return LithpValue.err("Function 'lambda' actual and formal argument lists differ in length.\n" +
                    "Formal: 2, Actual: " + args.getCount());
        }
        LithpValue formals = args.pop();
        LithpValue body = args.pop();
        if (formals.getType() != LithpValue.Type.Q_EXPR) {
            return LithpValue.err("Function 'lambda' passed incorrect type for first argument.\n" +
                    "Expected S-Expression or List, found " + typeName(formals) + ": " + formals + ".");
        }
        if (body.getType() != LithpValue.Type.Q_EXPR) {
            return LithpValue.err("Function 'lambda' passed incorrect type for second argument.\n" +
                    "Expected S-Expression or List, found " + typeName(body) + ": " + body + ".");
        }
        body.setType(LithpValue.Type.S_EXPR);
        for (LithpValue sym : formals) {
            if (sym.getType() != LithpValue.Type.SYM) {
                return LithpValue.err("Function 'lambda' first argument contains non-symbols.\n" +
                        "Expected Symbol, found " + typeName(sym) + ": " + sym + ".");
            }
        }
        return LithpValue.lambda(creator, formals, body);
    }
    private LithpValue builtinCall(LithpEnv env, LithpValue func, LithpValue args) {if (func.isBuiltin()) {
            return func.getBuiltinFunction().apply(env, args);
        }
        int formal = func.getFormals().getCount();
        int actual = args.getCount();
        if (formal == 0 && actual == 1 && args.get(0).equals(LithpValue.VOID)) {
            func.getEnv().setParent(env);
            return eval(func.getEnv(), new LithpValue(func.getBody()));
        }
        func = new LithpValue(func); // copy now so we don't consume formals
        LithpValue formals = func.getFormals();
        while (args.getCount() > 0) {
            if (formals.getCount() <= 0) {
                return LithpValue.err("A function's formal and actual argument lists differ in length.\n" +
                        "Formal: " + formal + ", Actual: " + actual + ".");
            }
            LithpValue formalSym = formals.pop();
            if (formalSym.getSym().equals("&")) {
                if (formals.getCount() != 1) {
                    return LithpValue.err("Can't bind function formals.\n" +
                            "Symbol '&' not followed by exactly one Symbol.");
                }
                formalSym = formals.pop();
                if (args.getCount() == 1 && args.get(0).equals(LithpValue.VOID)) {
                    func.getEnv().put(formalSym, LithpValue.qexpr());
                } else {
                    func.getEnv().put(formalSym, builtinList(args));
                }
                break;
            }
            func.getEnv().put(formalSym, args.pop());
        }
        if (formals.getCount() > 0 && formals.get(0).getSym().equals("&")) {
            if (formals.getCount() != 2) {
                return LithpValue.err("Can't bind function formals.\n" +
                        "Symbol '&' not followed by exactly one Symbol.");
            }
            formals.pop();
            func.getEnv().put(formals.pop(), LithpValue.qexpr());
        }
        if (formals.getCount() == 0) {
            func.getEnv().setParent(env);
            return eval(func.getEnv(), new LithpValue(func.getBody()));
        } else {
            return func;
        }
    }
    // list functions
    LithpValue builtinLen(LithpValue arg) {
        //<editor-fold desc="Error checking">
        if (arg.getCount() != 1) {
            return LithpValue.err("Function 'len' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        LithpValue list = arg.pop();
        if (list.getType() != (LithpValue.Type.Q_EXPR)) {
            return LithpValue.err("Function 'len' passed incorrect type.\n" +
                    "Expected List, found " + typeName(list) + ": " + list + ".");
        }
        //</editor-fold>
        return LithpValue.num(list.getCount());
    }
    LithpValue builtinHead(LithpValue arg) {
        //<editor-fold desc="Error checking">
        if (arg.getCount() != 1) {
            return LithpValue.err("Function 'head' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        LithpValue ret = new LithpValue(arg).pop();
        if (ret.getType() != LithpValue.Type.Q_EXPR) {
            return LithpValue.err("Function 'head' passed incorrect type.\n" +
                    "Expected List, found " + typeName(ret) + ": " + ret + ".");
        }
        if (ret.getCount() == 0) {
            return LithpValue.err("Function 'head' passed '().");
        }
        //</editor-fold>
        List<LithpValue> qexpr = ret.getCells(); // ret is defined in the editor fold
        while (qexpr.size() > 1) {
            qexpr.remove(1);
        }
        return ret;
    }
    LithpValue builtinTail(LithpValue arg) {
        //<editor-fold desc="Error checking">
        if (arg.getCount() != 1){
            return LithpValue.err("Function 'tail' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        LithpValue ret = new LithpValue(arg.pop());
        if (ret.getType() != LithpValue.Type.Q_EXPR) {
            return LithpValue.err("Function 'tail' passed incorrect type.\n" +
                    "Expected List, found " + typeName(ret) + ": " + ret + ".");
        }
        if (ret.getCount() == 0) {
            return LithpValue.err("Function 'tail' passed '().");
        }
        //</editor-fold>
        ret.getCells().remove(0); // ret is defined in the editor fold, ret = arg.pop()
        return ret;
    }
    LithpValue builtinJoin(LithpValue args) {
        //<editor-fold desc="Error checking">
        if (args.getCount() == 0) {
            return LithpValue.err("Function 'join' passed 0 arguments.");
        }
        for (LithpValue v : args) {
            if (v.getType() != LithpValue.Type.Q_EXPR) {
                return LithpValue.err("Function 'join' passed incorrect type.\n" +
                        "Expected List, found " + typeName(v) + ": " + v + ".");
            }
        }
        //</editor-fold>
        LithpValue x = new LithpValue(args.pop());
        while (args.getCount() > 0) {
            x.join(args.pop());
        }
        return x;
    }
    LithpValue builtinList(LithpValue arg) {
        arg.setType(LithpValue.Type.Q_EXPR);
        return arg;
    }
    LithpValue builtinEval(LithpEnv env, LithpValue arg) {
        //<editor-fold desc="Error checking">
        if (arg.getCount() != 1) {
            return LithpValue.err("Function 'eval' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        LithpValue x = arg.pop();
        if (x.getType() != LithpValue.Type.Q_EXPR) {
            return LithpValue.err("Function 'eval' passed incorrect type.\n" +
                    "Expected List, found " + typeName(x) + ": " + x + ".");
        }
        //</editor-fold>
        x.setType(LithpValue.Type.S_EXPR); // x = arg.pop() in the editor fold
        return eval(env, x);
    }
    // order functions
    private LithpValue builtinOrd(LithpValue args, String op) {
        if (args.getCount() != 2) {
            return LithpValue.err("Function '" + op + "' actual and formal argument lists differ in length.\n" +
                    "Formal: 2, Actual: " + args.getCount());
        }
        if (op.equals("eq?")) return args.pop().equals(args.pop()) ? LithpValue.TRUE : LithpValue.FALSE;
        LithpValue x = args.pop();
        if (x.getType() != LithpValue.Type.NUM) {
            return LithpValue.err("Function '" + op + "' passed incorrect type for first argument.\n" +
                    "Excpected Number, found " + typeName(x.getType()) + ": " + x + ".");
        }
        LithpValue y = args.pop();
        if (y.getType() != LithpValue.Type.NUM) {
            return LithpValue.err("Function '" + op + "' passed incorrect type for second argument.\n" +
                    "Excpected Number, found " + typeName(y.getType()) + ": " + y + ".");
        }
        LithpValue ret;
        switch(op) {
            case "<": ret = x.getNum() < y.getNum() ? LithpValue.TRUE : LithpValue.FALSE; break;
            case ">": ret = x.getNum() > y.getNum() ? LithpValue.TRUE : LithpValue.FALSE; break;
            case "<=": ret = x.getNum() <= y.getNum() ? LithpValue.TRUE : LithpValue.FALSE; break;
            case ">=": ret = x.getNum() >= y.getNum() ? LithpValue.TRUE : LithpValue.FALSE; break;
            case "==": ret = x.getNum() == y.getNum() ? LithpValue.TRUE : LithpValue.FALSE; break;
            case "!=": ret = x.getNum() != y.getNum() ? LithpValue.TRUE : LithpValue.FALSE; break;
            default: ret = LithpValue.FALSE;
        }
        return ret;
    }
    LithpValue builtinEqAny(LithpValue args) {
        return builtinOrd(args, "eq?");
    }
    LithpValue builtinEq(LithpValue args) {
        return builtinOrd(args, "==");
    }
    LithpValue builtinNeq(LithpValue args) {
        return builtinOrd(args, "!=");
    }
    LithpValue builtinLe(LithpValue args) {
        return builtinOrd(args, "<");
    }
    LithpValue builtinLeq(LithpValue args) {
        return builtinOrd(args, "<=");
    }
    LithpValue builtinGe(LithpValue args) {
        return builtinOrd(args, ">");
    }
    LithpValue builtinGeq(LithpValue args) {
        return builtinOrd(args, ">-");
    }
    // boolean operations
    LithpValue builtinBool(LithpValue arg) {
        if (arg.getCount() != 1) {
            return LithpValue.err("Function 'bool' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        LithpValue x = arg.pop();
        if (x.equals(LithpValue.FALSE) || x.equals(LithpValue.NIL) || x.equals(LithpValue.sexpr()) || (x.getType() == LithpValue.Type.NUM && x.getNum() == 0)) {
            return LithpValue.FALSE;
        }
        return LithpValue.TRUE;
    }
    LithpValue builtinAnd(LithpValue args) {
        for(LithpValue term : args) {
            if (term.equals(LithpValue.FALSE) || term.equals(LithpValue.NIL) || term.equals(LithpValue.sexpr()) || (term.getType() == LithpValue.Type.NUM && term.getNum() == 0)) {
                return LithpValue.FALSE;
            }
        }
        return LithpValue.TRUE;
    }
    LithpValue builtinOr(LithpValue args) {
        for(LithpValue term : args) {
            if (!(term.equals(LithpValue.FALSE) || term.equals(LithpValue.NIL) || term.equals(LithpValue.sexpr()) || (term.getType() == LithpValue.Type.NUM && term.getNum() == 0))) {
                return LithpValue.TRUE;
            }
        }
        return LithpValue.FALSE;
    }
    LithpValue builtinNot(LithpValue arg) {
        if (arg.getCount() != 1) {
            return LithpValue.err("Function 'not' actual and formal argument lists differ in length.\n" +
                    "Formal: 1, Actual: " + arg.getCount() + ".");
        }
        return builtinBool(arg).equals(LithpValue.FALSE) ? LithpValue.TRUE : LithpValue.FALSE;
    }
    // math functions
    private LithpValue builtinOp(LithpValue args, String op) {
        for (LithpValue arg : args) {
            if (arg.getType() != LithpValue.Type.NUM
                && arg.getType() != LithpValue.Type.BOOL) return LithpValue.err(args.toString() + " contains a non-number."); // #t = 1, #f = 0
        }
        LithpValue x = new LithpValue(args.pop());
        if (op.equals("-") && args.getCount() == 0) {
            x.setNum(-x.getNum());
        }
        loop:
        while (args.getCount() > 0) {
            LithpValue y = args.pop();
            switch (op) {
                case "+":
                    x.setNum(x.getNum() + y.getNum());
                    break;
                case "-":
                    x.setNum(x.getNum() - y.getNum());
                    break;
                case "*":
                    x.setNum(x.getNum() * y.getNum());
                    break;
                case "/":
                    if (y.getNum() == 0) {
                        x = LithpValue.err("Division by Zero");
                        break loop;
                    }
                    x.setNum(x.getNum() / y.getNum());
                    break;
                case "%":
                    x.setNum(x.getNum() % y.getNum());
                    break;
                case "^":
                    x.setNum((long) Math.pow(x.getNum(), y.getNum()));
                    break;
            }
        }
        return x;
    }
    LithpValue builtinAdd(LithpValue args) {
        return builtinOp(args, "+");
    }
    LithpValue builtinSub(LithpValue args) {
        return builtinOp(args, "-");
    }
    LithpValue builtinMult(LithpValue args) {
        return builtinOp(args, "*");
    }
    LithpValue builtinDiv(LithpValue args) {
        return builtinOp(args, "/");
    }
    LithpValue builtinMod(LithpValue args) {
        return builtinOp(args, "%");
    }
    LithpValue builtinPow(LithpValue args) {
        return builtinOp(args, "^");
    }

    // for error reporting
    private String typeName(LithpValue.Type type) {
        switch (type) {
            case Q_EXPR:
                return "List";
            case SYM:
                return "Symbol";
            case VOID:
                return "Void";
            case MACRO:
            case FUNC:
                return "Function";
            case S_EXPR:
                return "S-Expression";
            case NUM:
                return "Number";
            case ERR:
                return "Error";
            default:
                return "Unknown";
        }
    }
    private String typeName(LithpValue value) {
        return typeName(value.getType());
    }
}
