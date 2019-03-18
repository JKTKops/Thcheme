package Lithp;

import ParserCombinator.ParseTree;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.BiFunction;

public class LithpValue implements Iterable<LithpValue> {
    final static LithpValue VOID = LithpValue.voidValue();
    final static LithpValue TRUE = LithpValue.bool(true);
    final static LithpValue FALSE = LithpValue.bool(false);
    final static LithpValue NIL = LithpValue.qexpr();

    enum Type { ERR, VOID, NUM, BOOL, SYM, S_EXPR, Q_EXPR /* stops evaluation */, FUNC, MACRO }

    /** The type of this L-val */
    private Type type;
    /** The number stored by a num L-Val */
    private long num;
    /** Boolean types store a boolean */
    private boolean bool;
    /** ERR and SYM types store a string */
    private String err;
    private String sym;
    /** S_EXPR types store a list of L-Vals */
    private List<LithpValue> lvals;
    /** builtin FUNC and MACRO types store a (LithpEnv, LithpValue) -> LithpValue function */
    private BiFunction<LithpEnv, LithpValue, LithpValue> builtinFunction;
    private boolean builtin;
    /** User-defined functions store a creation environment, calling environment, formal argument list, and code body */
    private LithpEnv env;
    private LithpValue formals;
    private LithpValue body;

    void add(LithpValue toAdd) {
        if (!(type == Type.S_EXPR || type == Type.Q_EXPR)) { return; }
        lvals.add(toAdd);
    }
    void join(LithpValue y) {
        while (y.getCount() > 0) {
            add(y.pop());
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof LithpValue)) return false;
        LithpValue value = (LithpValue) obj;
        if (type != value.type) return false;
        switch (type) {
            case VOID: return true;
            case SYM: return sym.equals(value.sym);
            case BOOL: return bool == value.bool;
            case Q_EXPR:
            case S_EXPR:
                if (lvals.size() != value.lvals.size()) return false;
                for (int i = 0; i < getCount(); i++) {
                    if (!lvals.get(i).equals(value.lvals.get(i))) return false;
                }
                return true;
            case ERR: return err.equals(value.err);
            case NUM: return num == value.num;
            case FUNC:
            case MACRO:
                if (builtin != value.builtin) return false;
                if (builtin) {
                    return sym.equals(value.sym);
                } else {
                    return env.equals(value.env) && formals.equals(value.formals) && body.equals(value.body);
                }
                default: return false;
        }
    }

    static LithpValue read(ParseTree.Node node) {
        switch (node.getValue()) {
            case "number":
                return readNum(node.getChild());
            case "symbol":
                return LithpValue.sym(node.getDeepValue(1));
            case "expr":
                return read(node.getChild());
        }
        LithpValue ret;
        switch (node.getValue()) {
            case "sexpr":
                ret = LithpValue.sexpr();
                break;
            case "qexpr":
                ret = LithpValue.qexpr();
                break;
            default: ret = VOID; break; // Happens when attempting to read an empty s-expression
        }
        for (ParseTree.Node child : node) {
            LithpValue toAdd = read(child);
            if (toAdd.getType() != Type.VOID) ret.add(toAdd);
        }
        return ret;
    }
    private static LithpValue readNum(ParseTree.Node node) {
        try {
            return LithpValue.num(Long.valueOf(node.getValue()));
        } catch (NumberFormatException e) {
            return LithpValue.err("Error: Invalid Number");
        }
    }

    @Override
    public Iterator<LithpValue> iterator() {
        if (type != Type.S_EXPR && type != Type.Q_EXPR) {
            return new ArrayList<LithpValue>().iterator();
        }
        return lvals.iterator();
    }

    @Override
    public String toString() {
        switch(type) {
            case NUM: return String.valueOf(num);
            case VOID: return "#<void>";
            case SYM: return sym;
            case BOOL: return bool ? "#t" : "#f";
            case S_EXPR: return exprString("(", ")");
            case Q_EXPR: return exprString("'(", ")");
            case FUNC:
                return (builtin ? "<builtinFunction>: " : "<Function>: ") + ((sym != null) ? sym : "<lambda>");
            case MACRO: return "<builtinFunction>: " + sym;
            case ERR: return "Error: " + err;
        }
        return "Untyped Lithp Value";
    }
    private String exprString(String open, String close) {
        StringBuilder ret = new StringBuilder(open);
        for (LithpValue v : lvals) {
            ret.append(v.toString()).append(' ');
        }
        return ret.toString().trim() + close;
    }
    Type getType() {
        return type;
    }
    void setType(Type setType) {
        type = setType;
    }
    long getNum() {
        return num;
    }
    void setNum(long setNum) {
        num = setNum;
    }
    String getSym() {
        return sym;
    }
    String getErr() {
        return err;
    }
    LithpValue pop() {
        return lvals.remove(0);
    }
    LithpValue pop(int i) {
        return lvals.remove(i);
    }
    int getCount() {
        return lvals.size();
    }
    List<LithpValue> getCells() {
        return lvals;
    }
    LithpValue get(int i) {
        return lvals.get(i);
    }
    BiFunction<LithpEnv, LithpValue, LithpValue> getBuiltinFunction() {
        return builtinFunction;
    }
    boolean isBuiltin() {
        return builtin;
    }
    LithpEnv getEnv() {
        return env;
    }
    LithpValue getFormals() {
        return formals;
    }
    LithpValue getBody() {
        return body;
    }

    private LithpValue() {}
    LithpValue(LithpValue toCopy) {
        type = toCopy.type;
        switch(type) {
            case FUNC:
            case MACRO:
                sym = toCopy.sym;
                builtinFunction = toCopy.builtinFunction;
                env = new LithpEnv(toCopy.env);
                formals = new LithpValue(toCopy.formals);
                body = new LithpValue(toCopy.body);
                break;
            case VOID: break;
            case NUM: num = toCopy.num; break;
            case SYM: sym = toCopy.sym; break;
            case BOOL: bool = toCopy.bool; break;
            case ERR: err = toCopy.err; break;
            case S_EXPR:
            case Q_EXPR:
                lvals = new ArrayList<>();
                for (LithpValue copy : toCopy) {
                    lvals.add(new LithpValue(copy));
                }
                break;
        }
    }

    static LithpValue num(long n) {
        LithpValue v = new LithpValue();
        v.type = Type.NUM;
        v.num = n;
        return v;
    }
    static LithpValue bool(boolean b) {
        LithpValue v = new LithpValue();
        v.type = Type.BOOL;
        v.bool = b;
        v.num = b ? 1 : 0;
        return v;
    }
    static LithpValue err(String e) {
        LithpValue v = new LithpValue();
        v.type = Type.ERR;
        v.err = e;
        return v;
    }
    static LithpValue voidValue() {
        LithpValue v = new LithpValue();
        v.type = Type.VOID;
        return v;
    }
    static LithpValue sym(String s) {
        LithpValue v = new LithpValue();
        v.type = Type.SYM;
        v.sym = s;
        return v;
    }
    static LithpValue sexpr() {
        LithpValue v = new LithpValue();
        v.type = Type.S_EXPR;
        v.lvals = new ArrayList<>();
        return v;
    }
    static LithpValue qexpr() {
        LithpValue v = new LithpValue();
        v.type = Type.Q_EXPR;
        v.lvals = new ArrayList<>();
        return v;
    }
    static LithpValue func(String symbol, BiFunction<LithpEnv, LithpValue, LithpValue> func) {
        LithpValue v = new LithpValue();
        v.type = Type.FUNC;
        v.sym = symbol;
        v.builtinFunction = func;
        v.builtin = true;
        return v;
    }
    static LithpValue lambda(LithpEnv creator, LithpValue formalArgs, LithpValue codeBody) {
        LithpValue v = new LithpValue();
        v.type = Type.FUNC;
        v.builtin = false;
        v.env = new LithpEnv(creator);
        v.formals = formalArgs;
        v.body =  codeBody;
        return v;
    }
    static LithpValue macro(String symbol, BiFunction<LithpEnv, LithpValue, LithpValue> func) {
        LithpValue v = new LithpValue();
        v.type = Type.MACRO;
        v.sym = symbol;
        v.builtinFunction = func;
        v.builtin = true;
        return v;
    }
    static LithpValue exit() {
        LithpValue v = new LithpValue();
        v.type = Type.ERR;
        v.err = "exit";
        return v;
    }
}
