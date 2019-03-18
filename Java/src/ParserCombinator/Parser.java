package ParserCombinator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import ParserCombinator.Result.Failure;

/**
 * An abstract data type that stores a function which takes a Stream and outputs a Result.
 * The stored function can be "consuming" or "nonconsuming". A consuming function returns
 * a Result whose 'rest' field is not equivalent to the input Stream. A nonconsuming function
 * returns a Result whose 'rest' field is equal to the input Stream.
 *
 * Parsers can map, bimap, chain, and fold over their Results.
 *
 * @author Max Kopinsky
 */
class Parser {
    /**
     * Class method which runs a given parser and folds its Result into a useful printed message.
     * @param p The parser to run.
     * @param i The input to run the parser on. This should be a String or a Stream.
     */
    static void run(Parser p, Object i) {
        p.run(i).fold(
                s -> System.out.println("success: " + s.value + "\nUnparsed stream: " + s.rest),
                f -> System.out.println("error: " + f.value + "\nUnparsed stream: " + f.rest)
        );
    }

    /** This Parser's parse function. */
    private Function<Stream, Result> parse;

    Parser(Function<Stream, Result> setParse) {
        parse = setParse;
    }

    /**
     * Runs the parser on the given input. Does not produce a response message, unlike the class run() method.
     *
     * @param input The input to run the parser on. This should be a String or a Stream.
     * @return The result of the parser, or an error Failure if the input was not a String or Stream.
     */
    Result run(Object input) {
        if (input instanceof Stream) {
            return parse.apply((Stream) input);
        } else if (input instanceof String) {
            return parse.apply(new Stream((String) input));
        }
        return new Failure(Symbol.value("invalid input to parser"), new Stream(""));
    }

    /**
     * Implementation of one common Parser map used to build an implicit tree structure into Success values.
     * If the Parser succeeds with the value [a, b], both value Symbols, then the Success will be mapped
     * to a Success with output value [nonterminal, (, a, b, )], where 'nonterminal' is a nonterminal Symbol, intended
     * to be the name of the rule that generated this Parser, and the ( and ) Symbols are child and parent markers.
     *
     * @param nonterminal The name of the rule that parses this production.
     * @return A mapped Result with an implicit tree structure.
     */
    Parser parent(String nonterminal) {
        return new Parser(stream -> parse.apply(stream).map(list -> {
            list.add(0, Symbol.childMarker());
            list.add(0, Symbol.nonterminal(nonterminal));
            list.add(Symbol.parentMarker());
            return list;
        }));
    }
    /**
     * Implementation of a supporting Parser map. A parser under the "literal" map loses its tree structure,
     * returning only the <em>terminal</em> symbols it matched.
     *
     * If you'd like to see an example, try running the following set of Parsers on a sentence with no punctuation:
     * Parser letter = set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
     * Parser word = plus(letter).parent("word");
     * Parser sentence = concat(word, plus(concat(string(" "), word))).parent("sentence");
     * The difference will be easiest to see if call sout(new ParseTree.buildTree(sentence(.literal()).run(your input))).
     *
     * @return A literalized Parser.
     */
    Parser literal() {
        return new Parser(stream -> parse.apply(stream).map(list -> {
            List<Symbol> collapsed = new ArrayList<>();
            collapsed.add(Symbol.value(list.stream().map(symbol ->
                    symbol.getType() == Symbol.SymbolType.VALUE
                            ? symbol.getValue()
                            : ""
            ).reduce("", (a, b) -> a + b)));
            return collapsed;
        }));
    }

    /**
     * Implementation of another common parser map. Used to indicate that a consuming Parser's
     * Result should be ignored. Intended for use in composition of Parsers.
     *
     * @return A mapped parser whose Success Results have empty values.
     */
    Parser ignore() {
        return new Parser(stream -> parse.apply(stream).map(list -> new ArrayList<>()));
    }
    /** Gets a new Parser which applies this Parser and then maps the Result. See Result.map(). */
    Parser map(Function<List<Symbol>, List<Symbol>> fn) {
        return new Parser(stream -> parse.apply(stream).map(fn));
    }
    /** Gets a new Parser which applies this Parser and then bimaps the Result. See Result.bimap(). */
    Parser bimap(Function<List<Symbol>, List<Symbol>> success, Function<List<Symbol>, List<Symbol>> failure) {
        return new Parser(stream -> parse.apply(stream).bimap(success, failure));
    }
    /**
     * Gets a new Parser which chains this Parser to another one. Takes a function from Result values to Parsers.
     * The output Parser runs this Parser, applies the input function to the Result's value, and then runs the
     * Parser returned by the input function on the Result's 'rest' Stream.
     *
     * The normal input function is vs -> p2.map(v -> { vs.addAll(v); return vs; }) where p2 is the Parser to chain to.
     * The end result of the above function is a Parser which applies this Parser and p2 in sequence, and returns their values in sequence.
     *
     * @param f A function f : List<Symbol> -> Parser to get the Parser to run after this one.
     * @return A chained Parser.
     */
    Parser chain(Function<List<Symbol>, Parser> f) {
        return new Parser(stream -> parse.apply(stream).chain((v, s) -> f.apply(v).run(s))); // v = value, s = stream of result
    }

    /** An unused function to apply Result -> Result folds to Parsers. Equivalent to folding the Result.
     * Since folding is usually a postprocess on a Parser, folding the Result is preferred. */
    Parser fold(BiFunction<List<Symbol>, Stream, Result> success, BiFunction<List<Symbol>, Stream, Result> failure) {
        return new Parser(stream -> parse.apply(stream).fold(success, failure));
    }
}
