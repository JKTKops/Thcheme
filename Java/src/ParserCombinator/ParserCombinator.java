package ParserCombinator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import ParserCombinator.ParseTree.Node;

import static ParserCombinator.Combinators.*;

/**
 * Main class of the ParserCombinator package.
 * Represents a complex Parser constructed dynamically from an input grammar in BNF form.
 *
 * @author Max Kopinsky
 */
public class ParserCombinator {
    private Parser parseGrammar;

    /**
     * Constructor for a ParserCombinator. Takes a grammar in BNF form.
     * A constructed ParserCombinator can call run() on a string to attempt to parse it,
     * which will return a ParseTree.
     *
     * @param BNFGrammar The grammar that this Parser should parse.
     * @throws IllegalArgumentException if the grammar is invalid.
     */
    public ParserCombinator(String BNFGrammar) throws IllegalArgumentException {
        Parser syntax = getBNFParser();
        ParseTree grammar =  new ParseTree(syntax.run(BNFGrammar));
        if (!grammar.assertSuccess()) {
            throw new IllegalArgumentException("Failed to parse the input grammar. " + grammar);
        }

        int startCount = 0;
        String startSymbol = null;

        // todo: throw an error if a rule is left-recursive
        // todo: verify every referenced rule is also defined
        // todo: builtin parsers: <ws>, <line-end>, <set>[]
        //<editor-fold desc="Identify the start symbol. Throws errors if one can't be found or if there are multiple.">
        List<String> referencedRules = new ArrayList<>();
        List<String> definedRules = new ArrayList<>();
        for (Node rule : grammar.getRoot()) {
            String defName = rule.getChild().getChild().getValue(); // rule -> lhs -> name
            if (definedRules.contains(defName)) {
                throw new IllegalArgumentException("A rule was defined twice. Alternation should be declared with '|' characters.");
            }
            definedRules.add(defName);
            for (Node list : rule.getChild(1)) {
                for (Node potentialReference : list) {
                    if (potentialReference.getChild().getValue().equals("rule-name")) {
                        String ruleName = potentialReference.getChild().getChild().getValue();
                        if (!referencedRules.contains(ruleName) && !ruleName.equals(defName)) {
                            referencedRules.add(ruleName);
                        }
                    }
                }
            }
        }
        for (String rule : definedRules) {
            if (!referencedRules.contains(rule)) {
                startCount++;
                if (startCount > 1) { throw new IllegalArgumentException("More than one potential start symbol (unreferenced rule)"); }
                startSymbol = rule;
            }
        }
        if (startCount == 0) {
            throw new IllegalStateException("No potential start symbol (all nonterminals are reference in other productions)");
        }
        //</editor-fold>

        Map<String, Parser> parsers = new HashMap<>();
        // iterate through rules, store the rule name
        for (Node rule : grammar.getRoot()) {
            String ruleName = rule.getChild().getChild().getValue(); // rule -> lhs  -> name
            // Create a list of lists of temporary Parsers.
            List<List<Parser>> seqProductions = new ArrayList<>();
            // for each list in the expression, create a list of temporary Parsers and add it to the list above
            for (Node list : rule.getChild(1)) {
                List<Parser> sequence = new ArrayList<>();
                seqProductions.add(sequence); // by reference.
                // for each term in the list, add a parser that matches it to the list above
                for (Node term : list) {
                    Node termValue = term.getChild();
                    Parser toAdd = never(""); // just to initialize to something, but this will always be overwritten.
                    switch (termValue.getValue()) {
                        case "regex":
                            toAdd = regex(termValue.getChild().getValue());
                            break;
                        case "literal":
                            toAdd = string(termValue.getChild().getValue());
                            break;
                        case "rule-name":
                            // whenever a rule is encountered, check if it is in the Map.
                            String referenceName = termValue.getChild().getValue();
                            if (parsers.containsKey(referenceName)) {
                                // If it is, reference it directly with Map.get(ruleName)
                                toAdd = parsers.get(referenceName);
                            } else {
                                // if it is not, reference it through a delayed combinator: delayed(() -> Map.get(ruleName))
                                toAdd = delayed(() -> parsers.get(referenceName));
                            }
                            break;
                    }
                    // Apply option flags to the term
                    for (Node option = termValue.getSibling(); option != null; option = option.getSibling()) {
                        switch (option.getChild().getValue()) {
                            case "?": toAdd = maybe(toAdd); break;
                            case "+": toAdd = plus(toAdd); break;
                            case "*": toAdd = star(toAdd); break;
                            case "l": toAdd = toAdd.literal(); break;
                            case "i": toAdd = toAdd.ignore(); break;
                        }
                    }
                    sequence.add(toAdd);
                }
            }

            // for each List<Parser> in the List<List<Parser>>
            // if the List<Parser> has size 1, map it to a Parser that is equivalent
            // otherwise map it to the sequence of the Parsers in the List (in order)
            // Results in a List<Parser>, one per list of this rule.
            List<Parser> productions = seqProductions.stream().map(sequence -> {
                        if (sequence.size() == 1) {
                            return sequence.get(0);
                        }
                        if (sequence.size() == 2) {
                            return concat(sequence.get(0), sequence.get(1));
                        }
                        return sequence(sequence.toArray(new Parser[0]));
                    }).collect(Collectors.toList());

            // Create a temporary Parser.
            Parser thisRule;
            // Set the temporary Parser equal to (the alternation of each Parser in the above list).parent("rule name")
            if (productions.size() == 1) {
                thisRule = productions.get(0);
            } else {
                thisRule = alternate(productions.toArray(new Parser[0]));
            }

            //POSTPROCESSING ON RULE
            Node option = rule.getChild().getChild(1);
            // Apply literal option flag
            if (option != null && option.getValue().equals("l")) {
                thisRule = thisRule.literal();
            }
            // Apply parent after literal so we still get [name, (, literal, )]
            thisRule = thisRule.parent(ruleName);
            // Apply ignore option flag after parent so the Result never appears at all
            if (option != null && option.getValue().equals("i")) {
                thisRule = thisRule.ignore();
            }
            // Store the temp Parser in the Map under the key of its name.
            parsers.put(ruleName, thisRule);
            // If its name is the start symbol, then also set parseGrammar to the temp Parser.
            if (ruleName.equals(startSymbol)) {
                parseGrammar = concat(thisRule, eof());
            }
        }
    }

    public ParseTree run(String input) {
        return new ParseTree(parseGrammar.run(input));
    }
}
