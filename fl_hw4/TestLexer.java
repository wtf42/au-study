import java.io.*;

import com.sun.java_cup.internal.runtime.*;
import java_cup.runtime.*;

public class TestLexer {

    public static void main(String[] args) throws IOException {
        LLexer lexer = new LLexer(new FileReader(args[0]));
        try {
            while (true) {
                java_cup.runtime.Symbol sym = lexer.next_token();
                if (sym.value == null) break;

                LLexer.Tok tok = (LLexer.Tok)sym.value;
                System.out.println(
                    tok.name + "(" +
                    (tok.value != null ? tok.value + ", " : "") +
                    sym.left + ", " +
                    sym.right + ")");
            }
        } catch(LLexer.LlangException e) {
            System.out.println("Fail: '" + e.text + "' line: " + e.line + ", column: " + e.col);
        }
    }
}
