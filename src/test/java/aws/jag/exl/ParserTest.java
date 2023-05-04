/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.exl;

import aws.jag.DiagramEditor.code.toJava.JavaTarget;
import aws.jag.DiagramEditor.nodegraph.*;
import static aws.jag.DiagramEditor.nodegraph.Domain.*;
import aws.jag.exl.*;
import java.io.*;
import java.util.*;
import java.util.logging.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class ParserTest {

    public ParserTest() {
    }

    @Test
    public void t1() throws IOException {
        var a = Expression.of(Token.number(2));
        var b = Expression.of(Token.number(2));
        var c = a.equals(b);
        System.out.println(c);
        System.out.println("equals: " + Expression.of(Token.number(2)).equals(Expression.of(Token.number(2))));
        assertEquals(Expression.of(Token.number(2)), Expression.of(Token.number(2)));
        assertEquals(Expression.of(Token.number(2)), new Parser(new Tokenizer("2")).term());
        assertEquals(Expression.of(Tokenizer.LPAREN, Expression.of(Token.identifier("f")),
                Expression.of(Token.number(42))),
                new Parser(new Tokenizer("f(42)")).term());
    }

    @Test
    public void t2() throws IOException {
        T("n+(j+2)");
        T("n+1");
        T("n+j+2");
        T("(n+4)+(j+2)");
        T("n+j*3");
    }
    @Test
    public void t3() throws IOException {
        T("n+(j-2)");
        T("n+1");
        T("n-j-2");
        T("(n+4)+(j-2)");
        T("n+j*3");
        T("f(2+3,π*2)");
        T("a={x}");
        T("a={x:5}");
        T("a={x:5+2}");
        T("a={x:5,y:/*xyz\nqq*/7} // whatever\n");
        U("a={x:5,y:/*xyz\nqq*/7} // whatever\n", "(= a ({ (: x 5) (: y 7)))");
    }
    @Test
    public void t4() throws IOException {
        assertEquals(x("n+j+5"), x("n+(j+5)"));
        assertEquals(x("(n+4)+(j+2)"), x("n+4+(j+2)"));
        assertEquals(x("(n+4)+(j+2)"), x("n+4+j+2"));
        assertEquals(x("// foo \n(n+4)+(j+2)"), x("n+4+j+2"));
        assertEquals(x("// foo \n(n+4)+(j+2)"), x("n+4+j //foo\n+2"));
        assertEquals(x("// foo \n(n+4)+(j+2)"), x("n+4+j+2 //foo"));
        assertEquals(x("/* foo */(n+4)+(j+2)"), x("n+4/*eh*/+j+2 //foo"));
    }
    @Test
    public void t5() throws IOException {
        var aj = new JavaTarget(Domain.unknown);
        aj.append(x("a>b"));
        aj.close();
        System.out.println();
    }
    private final Set<String> imports = new HashSet<>();
    public void addImport(String pkg) {
        imports.add(pkg);
    }
    public void dumpImports(Appendable out) {
        imports.forEach(p -> {
            try {
                out.append("import ").append(p).append(";\n");
            } catch(IOException ex) {
                Logger.getLogger(ParserTest.class.getName()).log(Level.SEVERE, null, ex);
            }
        });
    }
    private Expression x(String e) throws IOException {
        return new Parser(new Tokenizer(e)).expression();
    }
    private void T(String e) throws IOException {
        var p = x(e);
        assertNotNull(p);
//        System.out.println("__________\n"+e+" -> ");
//        System.out.println("\t"+x(e));
    }
    private void U(String e, String sexpr) throws IOException {
        assertEquals(x(e).toString(), sexpr);
    }

}
