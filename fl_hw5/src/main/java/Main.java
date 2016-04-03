import org.antlr.v4.gui.TreeViewer;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;

import javax.swing.*;
import java.io.IOException;
import java.util.Arrays;

public class Main {
    public static void main(String[] args) throws IOException {
        new Main().run(args[0]);
    }
    void run(String filename) throws IOException {
        CharStream input = new ANTLRFileStream(filename);
        LlangLexer lexer = new LlangLexer(input);
        //printTokens(lexer); // HW4
        showTree(lexer); // HW5
    }
    void printTokens(LlangLexer lexer) {
        Token tok;
        while ((tok = lexer.nextToken()).getType() != LlangLexer.EOF) {
            System.out.println(getTokenName(tok));
        }
    }
    void showTree(LlangLexer lexer) {
        LlangParser parser = new LlangParser(new CommonTokenStream(lexer));
        parser.addParseListener(new ParseTreeListener() {
            @Override
            public void visitTerminal(TerminalNode terminalNode) {
                System.out.println(getTokenName(terminalNode.getSymbol()));
            }
            @Override
            public void visitErrorNode(ErrorNode errorNode) {}
            @Override
            public void enterEveryRule(ParserRuleContext parserRuleContext) {}
            @Override
            public void exitEveryRule(ParserRuleContext parserRuleContext) {}
        });
        ParseTree tree = parser.p();
        //System.out.println(tree.toStringTree(parser));

        // show AST in GUI
        JFrame frame = new JFrame("Antlr AST");
        JPanel panel = new JPanel();
        TreeViewer viewr = new TreeViewer(Arrays.asList(parser.getRuleNames()),tree);
        viewr.setScale(2.0);
        panel.add(viewr);
        frame.add(panel);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(600, 600);
        frame.setVisible(true);
    }
    public String getTokenName(Token tok) {
        if (tok.getType() == LlangLexer.EOF)
            return "<EOF>";
        String extra = "";
        if (tok.getType() == LlangParser.IDENT)
            extra = "\"" + tok.getText() + "\", ";
        if (tok.getType() == LlangParser.INT)
            extra = tok.getText() + ", ";
        return LlangLexer.ruleNames[tok.getType() - 1] +
                "(" + extra +
                tok.getStartIndex() + ", " +
                tok.getStopIndex() + ")";
    }
}
