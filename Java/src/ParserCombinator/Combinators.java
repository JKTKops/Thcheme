package ParserCombinator;
import ParserCombinator.Result.*;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class containing several higher order functions.
 * Some of these are Parser Suppliers, which produce a Parser from scratch.
 * Most are true combinators, which take some number of Parsers as input and output a Parser that combines them in some way.
 * This class is statically imported into ParserCombinator, so its methods may be used without reference to this class.
 *
 * The core combinators are based on ideas from Chet Corcos' article, https://medium.com/@chetcorcos/introduction-to-parsers-644d1b5d7f3d.
 *
 * @author Max Kopinsky
 */
abstract class Combinators {
    /**
     * The "alternation" combinator. The output Parser runs the input Parsers in sequence and
     * returns the Result of the first successful one. If they all fail, it returns a Failure
     * whose value lists the error produced by each Parser.
     *
     * @param list The list of input Parsers to alternate. Accepts variadic input.
     * @return An alternated Parser.
     */
    static Parser alternate(Parser... list) {
        return new Parser(stream -> {
            List<Symbol> error = new ArrayList<>();
            for (Parser parser : list) {
                Result result = parser.run(stream);
                if (result instanceof Success) {
                    return result;
                } else if (result instanceof Failure) {
                    String e = "";
                    for (Symbol symbol : result.value) {
                        e += (e.endsWith(": ") ? " " : "; ") + symbol.toString();
                    }
                    error.add(Symbol.value(e.substring(2)));
                }
            }
            return new Failure(error, stream);
        });
    }

    /**
     * A Parser supplier. The output Parser is nonconsuming, always succeeds, and does so with the given string
     * as the only (terminal) Symbol in its value.
     *
     * @param value The value that the output Parser should succeed with.
     * @return A Parser that always succeeds.
     */
    static Parser always(String value) {
        return new Parser(stream -> new Success(Symbol.value(value), stream));
    }

    /**
     * A Parser supplier. The output Parser is nonconsuming, always fails, and does so with the given string
     * as its error value.
     *
     * @param error The value that the output Parser should fail with.
     * @return A Parser that always fails.
     */
    static Parser never(String error) {
        return new Parser(stream -> new Failure(Symbol.value(error), stream));
    }

    /**
     * Returns a Parser that applies two given Parsers in sequence. Slightly better time efficiency than the
     * sequence combinator when only two Parsers are given.
     *
     * @param p1 The first Parser to apply.
     * @param p2 The second Parser to apply.
     * @return A concatenated Parser.
     */
    static Parser concat(Parser p1, Parser p2) {
        return p1.chain(vs -> p2.map(v -> { vs.addAll(v); return vs; }));
    }

    /**
     * Returns a Parser that applies any number of given Parsers in sequence.
     * If only two Parsers are required, concat() should be used instead.
     * If one Parser is required, that Parser should be used instead.
     * If zero input Parsers are desired, use an always("") Parser instead, but this combinator has safety for that case.
     *
     * @param list Variadic Parser input.
     * @return A Parser that applies the given Parsers in sequence.
     */
    static Parser sequence(Parser... list) {
        if (list.length == 0) {
            return always("");
        }
        Parser acc = list[0];
        for (int i = 1; i < list.length; i++) {
            acc = concat(acc, list[i]);
        }
        return acc;
        //CHALLENGE EXERCISE: How is the return value of the following one-liner for this method different from the implementation above?
        //It may help to look at the `string` Parser supplier for a comment on how .reduce() works.
        //return Arrays.stream(list).reduce(always("").ignore(), (p1, p2) -> concat(p1, p2));
    }

    /**
     * Returns a Parser that matches the input Parser 0 or 1 times. Equivalent functionality to the regex modifier '?'.
     *
     * @param parser The Parser to attempt.
     * @return A Parser that matches the input Parser 0 or 1 times.
     */
    static Parser maybe(Parser parser) {
        return new Parser(stream -> parser.run(stream).fold(
                (v, s) -> new Success(v, s),
                // If the parse fails, return a Success with empty value
                // that does NOT consume any of the input stream
                (e, s) -> new Success(new ArrayList<>(), stream)
        ));
    }

    /**
     * Returns a nonconsuming Parser that succeeds when the input Parser would succeed.
     * Behaves similarly to the regex (?=...) where '...' is whatever the input Parser matches.
     *
     * @param parser The Parser to make nonconsuming.
     * @return A nonconsuming Parser.
     */
    static Parser lookahead(Parser parser) {
        return new Parser(stream -> parser.run(stream).fold(
                (v, s) -> new Success(new ArrayList<>(), stream),
                (e, s) -> new Failure(e, stream)));
    }

    /**
     * Returns a Parser that matches the input Parser 0 or more times.
     * Behaves like the regex modifier '*'.
     *
     * @param parser The Parser to attempt.
     * @return A Parser that matches the input Parser 0 or more times.
     */
    static Parser star(Parser parser) {
        return new Parser(stream -> parser.run(stream).fold(
                (value, s) -> star(parser).map(rest -> {value.addAll(rest); return value; }).run(s),
                (error, s) -> new Success(new ArrayList<>(), stream)));
    }

    /**
     * Returns a Parser that matches the input Parser 1 or more times.
     * Behaves like the regex modifier '+'.
     *
     * @param parser The Parser to attempt.
     * @return A Parser that matches the input Parser 1 or more times.
     */
    static Parser plus(Parser parser) {
        return new Parser(stream -> parser.run(stream).fold(
                (value, s) -> star(parser).map(rest -> { value.addAll(rest); return value; }).run(s),
                (error, s) -> {
                    error.add(0, Symbol.value("'Plus' parser failed: "));
                    return new Failure(error, stream);
                }));
    }

    /**
     * Parser Supplier that returns a Parser that matches the input string.
     * @param str The string that the Parser should match.
     * @return A Parser that matches the input string.
     */
    static Parser string(final String str) {
        if (str.length() == 0) {
            return always("");
        }
        Parser[] list = new Parser[str.length()];
        for (int i = 0; i < str.length(); i++) {
            list[i] = accept(str.charAt(i));
        }
        // As an exercise in functional programming, the following line has the same functionality
        // as the above 4 (Why is the loop preferable, other than readability? What space/time comparisons are there?):
        // list = str.chars().mapToObj(c -> accept((char) c)).toArray((IntFunction<Parser[]>) Parser[]::new);

        return sequence(list).bimap(
                v -> v,
                e -> {
                    e.add(0, Symbol.value("Failed to match \"" + str + "\": "));
                    return e;
                }
        ).literal(); // Otherwise we would return [e, x, a, m, p, l, e] instead of [example].
    }

    /**
     * Returns a Parser that matches any one character in the input string.
     * If the input string is empty, the output Parser will always fail with error "'set' failed: empty set".
     *
     * @param charSet The set of characters that can be matched.
     * @return A Parser that matches any one character in the input string.
     */
    static Parser set(final String charSet) {
        if (charSet.length() == 0) {
            return never("'set' failed: empty set");
        }
        Parser[] list = new Parser[charSet.length()];
        for (int i = 0; i < charSet.length(); i++) {
            list[i] = accept(charSet.charAt(i));
        }
        return alternate(list).bimap(
                v -> v,
                e -> {
                    e.add(0, Symbol.value("Failed to match character set \"" + charSet + "\": "));
                    return e;
                }
        );
    }

    /**
     * A Parser supplier that returns a Parser that matches any character.
     * The output Parser fails only if there are no characters left in the Stream.
     * Identical in functionality to the regex special character '.'.
     *
     * @return A Parser that matches any character.
     */
    static Parser dot() {
        return new Parser(stream -> stream.length() == 0
                                    ? new Failure(Symbol.value("unexpected EOF"), stream)
                                    : new Success(Symbol.value(stream.head()), stream.move(1)));
    }

    /**
     * Returns a Parser that matches any character if the input Parser fails.
     * Because of its inflexibility, this combinator should only be used with input Parsers that only match one character.
     * It will still work with more complex input Parsers, but with behavior that is difficult to manage.
     *
     * @param parser The Parser to attempt.
     * @return A Parser that matches any character if the input Parser fails.
     */
    static Parser not(Parser parser) {
        return new Parser(stream -> parser.run(stream).fold(
                (value, s) -> { value.add(0, Symbol.value("'Not' parser failed; matched: "));
                                return new Failure(value, stream); },
                (error, s) -> stream.length() > 0
                                ? new Success(Symbol.value(stream.head()), stream.move(1))
                                : new Failure(Symbol.value("'Not' parser failed; empty stream"), stream)));
    }

    static Parser nonConsumingNot(Parser parser) {
        return new Parser(stream -> parser.run(stream).fold(
                (value, s) -> {
                    value.add(0, Symbol.value("Non-consuming Not parser failed; matched: "));
                    return new Failure(value, stream); },
                (error, s) -> new Success(new ArrayList<>(), s)));
    }

    /**
     * Parser supplier that returns a Parser which matches only the given character.
     * This and regex are the only Parser suppliers that produce consuming Parsers.
     *
     * @param c The character the output Parser should match.
     * @return A Parser that matches only the given character.
     */
    static Parser accept(char c) {
        return new Parser(stream -> {
            if (stream.length() == 0) {
                return new Failure(Symbol.value("unexpected EOF"), stream);
            }
            String value = stream.head();
            if (value.equals(((Character) c).toString())) {
                return new Success(Symbol.value(value), stream.move(1));
            }
            return new Failure(Symbol.value("\"" + value + "\" did not match \"" + c + "\""), stream);
        });
    }

    /**
     * A Parser supplier that returns a Parser that matches the given regex.
     * This and accept are the only Parser suppliers that produce consuming Parsers.
     * @param regex The regex this Parser should match.
     * @return A Parser that matches the given regex.
     */
    static Parser regex(String regex) {
        return new Parser(stream -> {
            Matcher matcher = Pattern.compile("^"+ regex).matcher(stream.toString());
            if (matcher.find()) {
                return new Success(Symbol.value(matcher.group().replaceAll("\\\\/", "/")), stream.move(matcher.group().length()));
            }
            return new Failure(Symbol.value("Failed to match regex: " + regex), stream);
        });
    }

    /**
     * A Parser that matches only the end-of-input 'character'.
     * The output Parser of a ParserCombinator should always be concat(<start-symbol>, eof()).
     *
     * @return A Parser that matches the end-of-input 'character'.
     */
    static Parser eof() {
        return new Parser(stream -> stream.length() == 0
                ? new Success(new ArrayList<>(), stream)
                : new Failure(Symbol.value("'eof' failed: "), stream));
    }

    /**
     * Returns a Parser that matches the Parser returned by the input Supplier<Parser>.
     * Though it may seem useless, this Parser is required for both directly and indirectly recursive productions,
     * since references cannot be forward-declared.
     *
     * A Supplier<Parser> is a function which takes no inputs and outputs a Parser; f : {} -> Parser.
     * Parsers which would require forward-declaration should be stored in a Map in an identifiable way,
     * and retrieved in the Parser declared before them by the Supplier () -> Map.get(identifier).
     *
     * @param parserSupplier A Supplier function that returns a Parser.
     * @return A Parser that matches the Parser returned by the input Supplier<Parser>.
     */
    static Parser delayed(Supplier<Parser> parserSupplier) {
        return new Parser(stream -> parserSupplier.get().run(stream));
    }

    /**
     * Parser supplier that gets a Parser which recognizes the grammar of BNF grammars.
     * @return A Parser that recognizes BNF grammars.
     */
    static Parser getBNFParser() {
        // Note that this grammar is not all that similar to one that this combinator
        // would generate. It is optimized to build a direct AST rather than a
        // parse tree through use of .ignore() to remove syntax sugar of the BNF
        // as well as skipping redundant single-child chains.
        // Based on the BNF grammar on the BNF wikipedia page.
        Parser digit = set("0123456789");
        Parser letter = set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
        Parser symbol = set("| !#$%&()*+,-./:;>=<?@[\\]^_`{}~");
        Parser character = alternate(letter, digit, symbol);
        Parser character1 = alternate(character, accept('\''));
        Parser character2 = alternate(character, accept('"'));
        Parser text1 = star(character1);
        Parser text2 = star(character2);
        Parser rule_char = alternate(letter, digit, accept('-'));
        Parser opt_whitespace = star(accept(' ')).ignore();
        Parser line_end = concat(opt_whitespace, alternate(string("\n"), string(System.lineSeparator()), eof())).ignore();

        // I highly recommend collapsing the lambda code block in the following line for readability.
        Parser regex = sequence(accept('/').ignore(), new Parser(stream -> {
            Stream s = stream;
            StringBuilder ret = new StringBuilder();
            for (String head = s.head(); !head.equals("/"); s = s.move(1), head = s.head()) {
                if (head.equals("\\")) {
                    s = s.move(1);
                    head = s.head();
                    if (!head.equals("/")) { // if the user was attempting to escape a character that isn't /
                        // escape it again.
                        ret.append("\\");
                    }
                }
                ret.append(head);
                if (line_end.run(s.move(1)) instanceof Result.Success) {
                    return new Result.Failure(Symbol.value("Unclosed regex in grammar!"), s);
                }
            }
            return new Result.Success(Symbol.value(ret.toString()), s);
        }), accept('/').ignore())
                .bimap(
                        v -> v,
                        e -> { e.clear(); e.add(Symbol.value("Couldn't match 'regex' pattern")); return e; }).parent("regex");
        Parser literal = alternate(sequence(accept('"').ignore(), text1, accept('"').ignore()),
                sequence(accept('\'').ignore(), text2, accept('\'').ignore())).literal().bimap(
                v -> v,
                e -> { e.clear(); e.add(Symbol.value("Couldn't match 'literal' pattern")); return e; }).parent("literal");
        Parser rule_name = sequence(accept('<').ignore(), concat(letter, star(rule_char)).literal().parent("rule-name"), accept('>').ignore())
                .bimap(
                        v -> v,
                        e -> { e.clear(); e.add(Symbol.value("Couldn't match 'rule name' pattern")); return e; });
        Parser multOptions = set("?*+");
        Parser literalOption = accept('l');
        Parser ignoreOption = accept('i');
        // todo: a multiplicity flag should be allowed with ignore
        Parser termOptions = maybe(alternate(concat(maybe(multOptions), literalOption), multOptions, ignoreOption).parent("option"));
        Parser ruleOption = alternate(literalOption, ignoreOption);
        Parser lhsDef = concat(rule_name.literal(), maybe(ruleOption)).parent("left-hand-side");

        Parser term = concat(alternate(literal, rule_name, regex), termOptions).parent("term");
        Parser list = concat(term, star(concat(opt_whitespace, term))).parent("list");
        Parser expr = concat(list, star(sequence(
                opt_whitespace, string("|").ignore(),
                opt_whitespace, list))).parent("expression");
        Parser rule = sequence(
                opt_whitespace, lhsDef,
                opt_whitespace, string("::=").ignore(),
                opt_whitespace, expr,
                line_end).parent("rule");
        return concat(plus(rule).parent("syntax"), eof()).bimap(v -> v, e -> {e.add( Symbol.value("Input did not end with a valid rule.")); return e; });
    }
}
