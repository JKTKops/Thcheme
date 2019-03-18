package ParserCombinator;

/**
 * ADT for storing output values of parsers, as well as making the tree structure of a Result value explicit.
 *
 * @author Max Kopinsky
 */
class Symbol {
    /** The possible types of Symbols. */
    enum SymbolType {
        /** Indicates that following Symbols are children of the previous Symbol. */
        CHILD_MARKER,
        /** Indicates a return to the parent of the previous Symbol. */
        PARENT_MARKER,
        /** Stores the value of nonterminal symbols. */
        NONTERMINAL,
        /** Stores terminal symbols. */
        VALUE
    }

    /** The type of this Symbol. */
    private SymbolType type;
    /** The value of this Symbol. Only used for Value and Nonterminal Symbols. */
    private String value;

    /** Private constructor to prevent creation of Symbols in this way. All Symbols should be created from the static factories. */
    private Symbol() {}

    /**
     * Getter for type.
     * @return This Symbol's type.
     */
    SymbolType getType() {
        return type;
    }

    /**
     * Gets the value of this Symbol. If this symbol is not of type Value or Nonterminal, an exception is thrown.
     * @return This Symbol's value.
     */
    String getValue() {
        if (!(type == SymbolType.VALUE || type == SymbolType.NONTERMINAL)) {
            throw new IllegalStateException("Can't get value of a marker Symbol.");
        }
        return value;
    }

    /**
     * Asserts that this Symbol has a value. If the assertion fails, an exception is thrown with the given error message.
     * @param e The error message to use in case of failure.
     */
    void assertValue(String e) {
        if (!(type == SymbolType.VALUE || type == SymbolType.NONTERMINAL)) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * Get a string representation of this Symbol.
     * Markers are represented by parentheses.
     * Value symbols are represented by their value.
     * @return A string representation of this Symbol.
     */
    @Override
    public String toString() {
        switch(type) {
            case CHILD_MARKER: return "(";
            case PARENT_MARKER: return ")";
            case NONTERMINAL:
            case VALUE: return value;
        }
        return "UntypedSymbol";
    }

    /** Static factory method to make child markers. */
    static Symbol childMarker() {
        Symbol ret = new Symbol();
        ret.type = SymbolType.CHILD_MARKER;
        return ret;
    }

    /** Static factory method to make parent markers. */
    static Symbol parentMarker() {
        Symbol ret = new Symbol();
        ret.type = SymbolType.PARENT_MARKER;
        return ret;
    }

    /** Static factory method to make nonterminal value Symbols. */
    static Symbol nonterminal(String v) {
        Symbol ret = new Symbol();
        ret.type = SymbolType.NONTERMINAL;
        ret.value = v;
        return ret;
    }

    /** Static factory method to make terminal value Symbols. */
    static Symbol value(String v) {
        Symbol ret = new Symbol();
        ret.type = SymbolType.VALUE;
        ret.value = v;
        return ret;
    }
}
