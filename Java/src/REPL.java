import Lithp.LithpEvaluator;
import ParserCombinator.*;
import java.util.Scanner;

/**
 * Main class of the REPL interpreter.
 * Maintains the Lithp grammar and handles the REPL.
 *
 * @author Max Kopinsky
 */
public class REPL {
    public static void main(String[] args) {
        LithpEvaluator evaluator = new LithpEvaluator();
        ParserCombinator lithp = new ParserCombinator(
                "<number> ::= /-?[0-9]+/\n" +
                        "<symbol> ::= /[a-zA-Z0-9_+\\-*\\/\\\\=<>?~!@#$%^&|]+/\n" +
                        "<ws>i ::= /\\s/*\n" +
                        "<comment>i ::= /;.*?\\n/\n" +
                        "<expr> ::= <number> <ws> | <symbol> <ws> | <comment>* <sexpr> <ws> | <qexpr> <ws>\n" +
                        "<sexpr> ::= '('i <ws> <expr>* ')'i\n" +
                        "<qexpr> ::= \"'(\"i <ws> <expr>* ')'i\n" +
                        "<lithp> ::= <expr>");
        String code;
        Scanner in = new Scanner(System.in);

        System.out.println("Lithp version 0.2.0.1\nUse (exit) to exit.");

        while(true) {
            System.out.print("Lithp> ");

            code = in.nextLine();
            int missingParens;
            while ((missingParens = missingParens(code)) > 0) {
                System.out.print(">>>... ");
                String more = in.nextLine();
                if (more.length() == 0) {
                    while(missingParens > 0) {
                        code += ")";
                        missingParens--;
                    }
                } else {
                    code += more;
                }
            }
            ParseTree parseTree = lithp.run(code);
            if (!parseTree.assertSuccess()) {
                System.out.println("Couldn't parse that input: " + parseTree.toString().substring(2));
                continue;
            }
            try {
                if (evaluator.eval(parseTree) < 0) break;
            } catch (Exception e) {
                System.out.println("An error occured:\n" + e);
            }
        }
    }

    private static int missingParens(String code) {
        int unclosed = 0;
        for (char c : code.toCharArray()) {
            if (c == '(') unclosed++;
            else if (c == ')') unclosed--;
        }
        return unclosed;
    }
}
