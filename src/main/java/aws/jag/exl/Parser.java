/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
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
        if(tok == Vocabulary.NULL || tok == Vocabulary.NULL || tok == Vocabulary.NULL) {
            ret = Expression.of(tok);
            tok = src.get();
        }else if(tok == Vocabulary.LPAREN) {
            tok = src.get();
            ret = expression();
            if(tok == Vocabulary.RPAREN) tok = src.get();
            else src.syntaxError("missing ')', at '" + tok + "'");
        } else if(tok == Vocabulary.LBRACE)
            ret = parseList(null, Vocabulary.BLOCK, Vocabulary.RBRACE);
        else if(tok == Vocabulary.LSQUARE)
            ret = parseList(null, Vocabulary.ARRAYLITERAL, Vocabulary.RSQUARE);
        else if(tok == Vocabulary.NEW) {
            tok = src.get();
            ret = Expression.of(Vocabulary.NEW, term());
        } else if(tok.isKeyword())
            if(tok == Vocabulary.NOT || tok == Vocabulary.MINUS || tok == Vocabulary.PLUS) {
                var t = tok;
                tok = src.get();
                ret = term();
                if(tok != Vocabulary.PLUS)
                    ret = Expression.of(t, ret);
            } else {
                if(tok!= Vocabulary.SEMI && tok!=Vocabulary.COMMA)
                    src.syntaxError(tok + " shouldn't be at the beginning of a term");
                ret = null;
            }
        else {
            ret = Expression.of(tok);
            tok = src.get();
        }
        while(true)
            if(tok == Vocabulary.LPAREN)
                ret = parseList(ret, Vocabulary.INVOKE, Vocabulary.RPAREN);
            else if(tok == Vocabulary.LSQUARE)
                ret = parseList(ret, Vocabulary.SUBSCRIPT, Vocabulary.RSQUARE);
            else if(tok == Vocabulary.LBRACE)
                ret = parseList(ret, Vocabulary.BLOCK, Vocabulary.RBRACE);
            else if(tok == Vocabulary.DOT) {
                Token id = src.get();
                if(!id.isIdentifier())
                    src.syntaxError("Identifer expected, got " + id);
                ret = Expression.of(Vocabulary.DOT, ret, Expression.of(id));
                tok = src.get();
            } else break;
//        System.out.println("Term " + ret);
        return ret;
    }
    public Expression expression() throws IOException {
        if(tok == Vocabulary.RETURN) {
            tok = src.get();
            return tok != Vocabulary.SEMI && tok != Vocabulary.RBRACE
                    ? Expression.of(Vocabulary.RETURN, expression())
                    : Expression.of(Vocabulary.RETURN);
        }
        return expressionTail(term(), 0);
    }
    private Expression expressionTail(Expression LHS, int endPriority) throws IOException {
        while(tok != Vocabulary.EOF) {
            var op = tok;
            var pri = priority[op.getType()];
            if(pri <= endPriority) break;
            tok = src.get();
//            System.out.println("tail "+op+' '+pri+" LHS "+LHS+"  ..."+tok);
            var RHS = expressionTail(term(), pri);
//            System.out.println("\t RHS "+RHS);
            if(flatten[op.getType()] && (LHS.getOperator() == op || RHS.getOperator() == op)) {
                var args = new ArrayList<Expression>(3);
                extractFlattened(args, LHS, op);
                extractFlattened(args, RHS, op);
                LHS = Expression.of(op, args);
            } else LHS = Expression.of(op, LHS, RHS);
        }
        return LHS;
    }
    private void extractFlattened(ArrayList<Expression> args, Expression e, Token op) {
        if(e.getOperator() == op)
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
                var e = expression();
                if(e!=null) args.add(e);
                if(tok != Vocabulary.SEMI && tok != Vocabulary.COMMA)
                    break;
                tok = src.get();
                if(tok == closer) break;  // allow trailing comma
            }
        if(tok == closer) tok = src.get();
        else src.syntaxError(closer + " expected, but got " + tok);
        if(op == Vocabulary.BLOCK && !args.isEmpty() && args.get(0).getOperator() == Vocabulary.COLON)
            op = Vocabulary.MAPLIT;
        return Expression.of(op, args);

    }
    private static void initOpProperties() {
        {
            Vocabulary.triggerInit();
            var p = new int[Token.typeTableSize()];
            int v = 1;
            Arrays.fill(p, -10);
//            p[Tokenizer.COMMA.getType()] = v;
//            p[Tokenizer.SEMI.getType()] = v;
            v++;
            p[Vocabulary.RARROW.getType()] = v;
            v++;
            p[Vocabulary.ASSIGN.getType()] = v;
            p[Vocabulary.DECLAREASSIGN.getType()] = v;
            p[Vocabulary.DECLAREASSIGNFINAL.getType()] = v;
            v++;
            p[Vocabulary.QUESTION.getType()] = v;
            p[Vocabulary.COLON.getType()] = v;
            v++;
            p[Vocabulary.ANDAND.getType()] = v;
            p[Vocabulary.AND.getType()] = v;
            p[Vocabulary.OR.getType()] = v;
            p[Vocabulary.OROR.getType()] = v;
            v++;
            p[Vocabulary.INSTANCEOF.getType()] = v;
            p[Vocabulary.LT.getType()] = v;
            p[Vocabulary.LE.getType()] = v;
            p[Vocabulary.GT.getType()] = v;
            p[Vocabulary.GE.getType()] = v;
            p[Vocabulary.EQ.getType()] = v;
            p[Vocabulary.NE.getType()] = v;
            p[Vocabulary.ELEMENTOF.getType()] = v;
            v++;
            p[Vocabulary.PLUS.getType()] = v;
            p[Vocabulary.MINUS.getType()] = v;
            v++;
            p[Vocabulary.DIVIDE.getType()] = v;
            p[Vocabulary.MULTIPLY.getType()] = v;
            priority = p;
        }
        {
            var p = new boolean[Token.typeTableSize()];
            p[Vocabulary.ASSIGN.getType()] = true;
            p[Vocabulary.DECLAREASSIGN.getType()] = true;
            p[Vocabulary.DECLAREASSIGNFINAL.getType()] = true;
            rightAssoc = p;
        }
        {
            var p = new boolean[Token.typeTableSize()];
            p[Vocabulary.COMMA.getType()] = true;
            p[Vocabulary.SEMI.getType()] = true;
            p[Vocabulary.ANDAND.getType()] = true;
            p[Vocabulary.AND.getType()] = true;
            p[Vocabulary.OR.getType()] = true;
            p[Vocabulary.OROR.getType()] = true;
            p[Vocabulary.PLUS.getType()] = true;
            p[Vocabulary.MULTIPLY.getType()] = true;
            flatten = p;
        }
    }
    private static int[] priority;
    private static boolean[] rightAssoc;
    private static boolean[] flatten;
}
