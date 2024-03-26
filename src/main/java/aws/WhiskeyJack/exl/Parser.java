/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import java.io.*;
import java.util.*;

public class Parser {
    private final Tokenizer src;
    private Token tok; // the token we're currently looking at
    public Parser(Tokenizer t) throws IOException {
        src = t;
        tok = t.get(); // position at the beginning
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
            ret = Expression.of(Vocabulary.NEW);
            ret.addAll(term());
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
                var id = src.get();
                if(!id.isIdentifier())
                    src.syntaxError("Identifer expected, got " + id);
                ret = Expression.of(Vocabulary.DOT, ret, Expression.of(id));
                tok = src.get();
            } else break;
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
            var pri = Vocabulary.getPriority(op);
            if(pri <= endPriority) break;
            tok = src.get();
            var RHS = expressionTail(term(), pri);
            if(Vocabulary.canFlatten(op) && (LHS.getOperator() == op || RHS.getOperator() == op)) {
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
}
