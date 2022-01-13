import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import bnfc.cpp.Absyn.*;

import bnfc.cpp.Yylex;
import bnfc.cpp.parser;

class Main {
    public static void main(String[] args) {
        Yylex l;
        parser p;
        Reader input = new InputStreamReader(System.in);
        l = new Yylex(input);
        p = new parser(l, l.getSymbolFactory());
        PDefs ast;
        try {
            ast = (PDefs) p.pProgram();
            
            // Successful parse! Typecheck here

        } catch (IOException e) {
            System.err.println("i/o error! oh no!");
            System.exit(1);
        } catch (Throwable e) {
            System.err.println("parse error!");
            e.printStackTrace();
            System.exit(1);
        }
    }
}