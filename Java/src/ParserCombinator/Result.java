package ParserCombinator;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Class representing an abstract data type which contains the result of a parser and the remaining unparsed Stream.
 *
 * In the words of category theory, Results "close over" the category of Lists of Symbols and the category of Streams,
 * and we say that the "closure" of a Result is the combination of its value and stored Stream.
 * We can also write Result = (List<Symbol> X Stream), using the cartesian product, to express this relationship.
 *
 * Whether the parser succeeded or failed is encoded in whether the result is a Success or a Failure.
 *
 * Results can be mapped, bimapped, chained, and folded.
 *
 * Based on ideas from Chet Corcos' article, https://medium.com/@chetcorcos/introduction-to-parsers-644d1b5d7f3d
 *
 * @author Max Kopinsky
 */
abstract class Result {
    /**
     * The actual result of the parser which produced this Result.
     * The tree structure of the parse is explicit in the list through use of Child and Parent marker Symbols.
     */
    List<Symbol> value;
    /** The remaining unparsed portion of parsed Stream. */
    Stream rest;

    /**
     * Explicit constructor.
     * @param v The value of this result.
     * @param r The remaining unparsed Stream.
     */
    Result(List<Symbol> v, Stream r) {
        value = v;
        rest = r;
    }
    /**
     * Implicit constructor for Results with single-Symbol values.
     * @param v The single Symbol to store in this Result.
     * @param r The remaining unparsed Stream.
     */
    Result(Symbol v, Stream r) {
        List<Symbol> newList = new ArrayList<>();
        newList.add(v);
        value = newList;
        rest = r;
    }

    /**
     * Returns a string representation of the Result. Only the Result's value is considered.
     * See List.toString().
     * @return A string representation of the Result.
     */
    @Override
    public String toString() {
        return value.toString();
    }

    /**
     * Maps a Success with value v to a new Success whose value is fn(v).
     * @param fn A function fn : List<Symbol> -> List<Symbol> to apply to the Result.
     * @return A Result representing the Result after mapping.
     */
    abstract Result map(Function<List<Symbol>, List<Symbol>> fn);
    /**
     * Maps a Success with value v to a new Success whose value is success(v).
     * Maps a Failure with value e to a new Failure whose value is failure(e).
     *
     * @param success A function success : List<Symbol> -> List<Symbol> to apply to Success Results.
     * @param failure A function failure : List<Symbol> -> List<Symbol> to apply to Failure Results.
     * @return A Result representing this Result after mapping.
     */
    abstract Result bimap(Function<List<Symbol>, List<Symbol>> success, Function<List<Symbol>, List<Symbol>> failure);
    /**
     * Maps a Success to the Result returned by a given function applied to the Success.
     * Used for "chaining" results together with the function (v, s) -> f.apply(v).run(s)
     * where f : List<Symbol> -> Parser. The normal f is constructed from a Parser p2
     * and an existing Parser p1 which returns the Result with value vs (for values)
     * and maps the Value of p2's Result, v, to vs.addAll(v). See Combinators.concat(Parser p1, Parser p2).
     *
     * The end result under such an fn is that chaining to Results concatenates their values.
     *
     * @param fn A function fn : Result = (List<Symbol> X Stream) -> Result to apply to Success Results.
     * @return A chained Result.
     */
    abstract Result chain(BiFunction<List<Symbol>, Stream, Result> fn);
    /**
     * Folds a Result into a new Result. A "fold" is a collapse of a Result.
     * For example, the combinator not(Parser p) takes the Result of p,
     * and "collapses" it into the result of not(p): a Success becomes a Failure,
     * and a Failure becomes a Success that consumes a token.
     *
     * This type of fold can be thought of as a bimap, and in fact they are nearly interchangeable.
     * The difference is that a fold collapses into a Result, whereas a bimap only "folds" the values of Results.
     *
     * @param success A function success : Result = (List<Symbol> X Stream) -> Result to fold Successes under.
     * @param failure A function failure : Result = (List<Symbol> X Stream) -> Result to fold Failures under.
     * @return The folded Result.
     */
    abstract Result fold(BiFunction<List<Symbol>, Stream, Result> success, BiFunction<List<Symbol>, Stream, Result> failure);
    /**
     * Folds a Result into a void behavior. This collapses the result completely;
     * a behavior is executed based on the Result and no Result is given back.
     * However, this method returns the Result that was folded for convenience.
     *
     * Because this type of fold is used for its side-effects, it cannot be thought of as a bimap.
     *
     * @param success A function success : Result -> {} to fold Successes under.
     * @param failure A function failure : Result -> {} to fold Failures under.
     * @return The folded Result, unchanged, for convenience.
     */
    abstract Result fold(Consumer<Result> success, Consumer<Result> failure);

    /**
     * A class representing a successful Result of a parser.
     */
    static class Success extends Result {
        Success(List<Symbol> value, Stream rest) {
            super(value, rest);
        }
        Success(Symbol v, Stream rest) {
            super(v, rest);
        }

        /** The application of map on Successes. Applies fn() to value. */
        Result map(Function<List<Symbol>, List<Symbol>> fn) {
            return new Success(fn.apply(value), rest);
        }
        /** The application of bimap on Successes. Applies success() to value. */
        Result bimap(Function<List<Symbol>, List<Symbol>> success, Function<List<Symbol>, List<Symbol>> failure) {
            return new Success(success.apply(value), rest);
        }
        /** The application of chain on successes. Applies fn() to the closure of the Success. */
        Result chain(BiFunction<List<Symbol>, Stream, Result> fn) {
            return fn.apply(value, rest);
        }
        /** The application of Result -> Result folds on Successes. Applies success() to the closure of the Success. */
        Result fold(BiFunction<List<Symbol>, Stream, Result> success, BiFunction<List<Symbol>, Stream, Result> failure) {
            return success.apply(value, rest);
        }
        /** The application of Result -> {} folds on Successes. Applies success() to value, and returns the Success for convenience. */
        Result fold(Consumer<Result> success, Consumer<Result> failure) {
            success.accept(this);
            return this;
        }
    }

    /**
     * A class representing the result of a failed parser. A Failure's "value" should be thought of as its "error".
     */
    static class Failure extends Result {
        Failure(List<Symbol> value, Stream rest) {
            super(value, rest);
        }
        Failure(Symbol value, Stream rest) {
            super(value, rest);
        }

        /** The application of map on Failures. Returns the Failure unchanged. */
        Result map(Function<List<Symbol>, List<Symbol>> fn) {
            return this;
        }
        /** The application of bimap on Failures. Applies failure() to value. */
        Result bimap(Function<List<Symbol>, List<Symbol>> success, Function<List<Symbol>, List<Symbol>> failure) {
            return new Failure(failure.apply(value), rest);
        }
        /** The application of chain on Failures. Returns the Failure unchanged. */
        Result chain(BiFunction<List<Symbol>, Stream, Result> fn) {
            return this;
        }
        /** The application of Result -> Result folds on Failures. Applies failure() to the closure of the Failure. */
        Result fold(BiFunction<List<Symbol>, Stream, Result> success, BiFunction<List<Symbol>, Stream, Result> failure) {
            return failure.apply(value, rest);
        }
        /** The application of Result -> {} folds on Failures. Applies failure() to value, and returns the Failure for convenience. */
        Result fold(Consumer<Result> success, Consumer<Result> failure) {
            failure.accept(this);
            return this;
        }
    }
}
