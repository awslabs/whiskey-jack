/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.exl;

import java.io.*;
import java.util.*;

public class Parser {
    private final Tokenizer src;
    private Token tok; // the token we're currently looking at
    public Parser(Tokenizer t) throws IOException {
        if(priority == null) initOpProperties();
        src = t;
        tok = t.get(); // position at the beginning
//        System.out.println("Parse " + tok);
    }
    Expression term() throws IOException {
        Expression ret;
        if(tok == Tokenizer.LPAREN) {
            tok = src.get();
            ret = expression();
            if(tok == Tokenizer.RPAREN) tok = src.get();
            else src.syntaxError("missing ')', at '" + tok + "'");
        } else if(tok == Tokenizer.LBRACE)
            ret = parseList(null, Tokenizer.LBRACE, Tokenizer.RBRACE);
        else if(tok == Tokenizer.LSQUARE)
            ret = parseList(null, Tokenizer.LSQUARE, Tokenizer.RSQUARE);
        else if(tok.isKeyword())
            if(tok == Tokenizer.NOT || tok == Tokenizer.MINUS || tok == Tokenizer.PLUS) {
                var t = tok;
                tok = src.get();
                ret = term();
                if(tok != Tokenizer.PLUS)
                    ret = Expression.of(t, ret);
            } else {
                src.syntaxError(tok + " shouldn't be at the beginning of a term");
                ret = null;
            }
        else {
            ret = Expression.of(tok);
            tok = src.get();
        }
        while(true)
            if(tok == Tokenizer.LPAREN)
                ret = parseList(ret, tok, Tokenizer.RPAREN);
            else if(tok == Tokenizer.LSQUARE)
                ret = parseList(ret, tok, Tokenizer.RSQUARE);
            else if(tok == Tokenizer.LBRACE)
                ret = parseList(ret, Tokenizer.HASPROPERTIES, Tokenizer.RBRACE);
            else if(tok == Tokenizer.DOT) {
                Token id = src.get();
                if(!id.isIdentifier())
                    src.syntaxError("Identifer expected, got " + id);
                ret = Expression.of(Tokenizer.DOT, ret, Expression.of(id));
                tok = src.get();
            } else break;
//        System.out.println("Term " + ret);
        return ret;
    }
    public Expression expression() throws IOException {
//        Expression ret = term();
//        System.out.println("Exp " + ret);;
        return expressionTail(term(), 0);
    }
    private Expression expressionTail(Expression LHS, int endPriority) throws IOException {
        while(tok != Tokenizer.EOF) {
            var op = tok;
            var pri = priority[op.getType()];
            if(pri <= endPriority) break;
            tok = src.get();
//            System.out.println("tail "+op+' '+pri+" LHS "+LHS+"  ..."+tok);
            var RHS = expressionTail(term(), pri);
//            System.out.println("\t RHS "+RHS);
            if(flatten[op.getType()] && (LHS.kind() == op || RHS.kind() == op)) {
                var args = new ArrayList<Expression>(3);
                extractFlattened(args, LHS, op);
                extractFlattened(args, RHS, op);
                LHS = Expression.of(op, args);
            } else LHS = Expression.of(op, LHS, RHS);
        }
        return LHS;
    }
    private void extractFlattened(ArrayList<Expression> args, Expression e, Token op) {
        if(e.kind() == op)
            e.forEach(f -> extractFlattened(args, f, op));
        else args.add(e);
    }
    // just parsed <first><op>; now parse list up to <closer>
    private Expression parseList(Expression first, Token op, Token closer) throws IOException {
        tok = src.get();
        var args = new ArrayList<Expression>(3);
        if(first != null) args.add(first);
        if(tok != closer)
            while(true) {
                args.add(expression());
                if(tok != Tokenizer.COMMA)
                    break;
                tok = src.get();
                if(tok == closer) break;  // allow trailing comma
            }
        if(tok == closer) tok = src.get();
        else src.syntaxError(closer + " expected, but got " + tok);
        return Expression.of(op, args);

    }
    private static void initOpProperties() {
        {
            var p = new int[Token.typeTableSize()];
            int v = 1;
            Arrays.fill(p, -10);
//        p[Tokenizer.COMMA.getType()] = v;
//        p[Tokenizer.SEMI.getType()] = v;
//        v++;
            p[Tokenizer.ASSIGN.getType()] = v;
            p[Tokenizer.DECLAREASSIGN.getType()] = v;
            p[Tokenizer.DECLAREASSIGNFINAL.getType()] = v;
            v++;
            p[Tokenizer.QUESTION.getType()] = v;
            p[Tokenizer.COLON.getType()] = v;
            v++;
            p[Tokenizer.ANDAND.getType()] = v;
            p[Tokenizer.AND.getType()] = v;
            p[Tokenizer.OR.getType()] = v;
            p[Tokenizer.OROR.getType()] = v;
            v++;
            p[Tokenizer.LT.getType()] = v;
            p[Tokenizer.LE.getType()] = v;
            p[Tokenizer.GT.getType()] = v;
            p[Tokenizer.GE.getType()] = v;
            p[Tokenizer.EQ.getType()] = v;
            p[Tokenizer.NE.getType()] = v;
            p[Tokenizer.ELEMENTOF.getType()] = v;
            v++;
            p[Tokenizer.PLUS.getType()] = v;
            p[Tokenizer.MINUS.getType()] = v;
            v++;
            p[Tokenizer.DIVIDE.getType()] = v;
            p[Tokenizer.MULTIPLY.getType()] = v;
            priority = p;
        }
        {
            var p = new boolean[Token.typeTableSize()];
            p[Tokenizer.ASSIGN.getType()] = true;
            p[Tokenizer.DECLAREASSIGN.getType()] = true;
            p[Tokenizer.DECLAREASSIGNFINAL.getType()] = true;
            rightAssoc = p;
        }
        {
            var p = new boolean[Token.typeTableSize()];
            p[Tokenizer.COMMA.getType()] = true;
            p[Tokenizer.SEMI.getType()] = true;
            p[Tokenizer.ANDAND.getType()] = true;
            p[Tokenizer.AND.getType()] = true;
            p[Tokenizer.OR.getType()] = true;
            p[Tokenizer.OROR.getType()] = true;
            p[Tokenizer.PLUS.getType()] = true;
            p[Tokenizer.MULTIPLY.getType()] = true;
            flatten = p;
        }
    }
    private static int[] priority;
    private static boolean[] rightAssoc;
    private static boolean[] flatten;
}
