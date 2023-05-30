/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.exl;

import java.util.*;
import java.util.function.*;

/**
 * Parsed Expression Node
 */
public class Expression { //TODO pick a less cryptic name
    private final Token operator;
    private final Expression[] args;
    private Expression(Token k, Expression... a) {
        operator = k;
        args = a;
    }
    public Expression duplicate() {
        var len = args.length;
        if(len == 0) return of(operator);
        var na = new Expression[len];
        for(var i = 0; i < len; i++)
            na[i] = args[i].duplicate();
        return of(operator, na);
    }
    public static Expression of(Token k) {
        return new Expression(k, empty);
    }
    public static Expression of(Token k, Expression... a) {
        if(a == null || a.length == 0) return of(k);
        return new Expression(k, a);
    }
    public static Expression of(Token k, Collection<Expression> a) {
        return of(k, a.stream().filter(v -> v != null).toArray(n ->
                new Expression[n]));
    }
    @SuppressWarnings({"UseSpecificCatch"})
    public static Expression of(Object o) {
        return switch(o) {
            case null ->
                of(Vocabulary.NULL);
            case Number n ->
                of(Token.number(n));
            case Expression e ->
                e;
            case String s -> {
                try {
                    yield new Parser(new Tokenizer(s)).expression();
                } catch(Throwable t) {
                    yield of(Token.string(s));
                }
            }
            default ->
                of(Token.string(o.toString()));
        };
    }
    public final Token getOperator() {
        return operator;
    }
    public final boolean isLeaf() {
        return !operator.isKeyword() || operator == Vocabulary.NULL;
    }
    public final Expression arg(int n) {
        return args[n];
    }
    public final int length() {
        return args.length;
    }
    public final void forEach(Consumer<Expression> f) {
        for(var e: args)
            f.accept(e);
    }
    public final Expression[] asArray() {
        return args;
    }
    public final Collection<Expression> map(Function<Expression, Expression> m) {
        var ret = new ArrayList(args.length);
        for(var a: args) {
            var v = m.apply(a);
            if(v != null) ret.add(v);
        }
        return ret;
    }
    public StringBuilder appendTo(StringBuilder sb) {
        if(sb == null) sb = new StringBuilder();
        if(isLeaf()) operator.appendTo(sb);
        else {
            operator.appendTo(sb.append('('));
            for(var a: args)
                a.appendTo(sb.append(' '));
            sb.append(')');
        }
        return sb;
    }
    @Override
    public final String toString() {
        return appendTo(null).toString();
    }
    private int myHash = 0;
    @Override
    public int hashCode() {
        var h = myHash;
        if(h == 0) {
            h = operator.hashCode();
            for(var a: args)
                h = 63 * h + a.hashCode();
            if(h == 0) h = 42;
            myHash = h;
        }
        return h;
    }
    @Override
    public boolean equals(Object v) {
        if(v instanceof Expression e
           && operator.equals(e.operator)
           && args.length == e.args.length) {
            for(int i = args.length; --i >= 0;)
                if(!args[i].equals(e.args[i]))
                    return false;
            return true;
        } else return false;
    }
    private static final Expression[] empty = new Expression[0];
    static int D = 0;
    public Expression rewrite(Rewriter rewriter) {
        D++;
        msg("Rewrite " + this);
        var rargs = args;
        var i = 0;
        var limit = rargs.length;
        while(i < limit) {
            var e0 = rargs[i];
            var e1 = e0.rewrite(rewriter);
            if(e1 != e0) {
                e1 = e1.rewrite(rewriter);
                if(rargs == args) // Found first change
                    rargs = Arrays.copyOf(rargs, limit);
                rargs[i] = e1;
                msg(e0+"->"+e1);
            }
            i++;
        }
        var ret = rewriter.rewrite(rargs == args ? this : Expression.of(operator, rargs));
        if(ret != this) msg("=> " + ret);
        D--;
        return ret;
    }
    private void msg(String s) {
        System.out.printf("%-4d", D);
        for(var i = D; --i >= 0;)
            System.out.append("    ");
        System.out.println(s);
    }
    public Expression rename(Map m) {
        return rewrite(exNode -> {
            var op = exNode.getOperator();
            if(op.isIdentifier()) {
                msg("Found Identifier " + exNode);
                var foundValue = m.get(op.getBody());
                if(foundValue != null) {
                    msg("  with value " + foundValue);
                    return Expression.of(foundValue);
                }
            }
            return exNode;
        });
    }

    public interface Rewriter {
        /**
         * When this method is called, all of the arguments will already have
         * been rewritten
         */
        Expression rewrite(Expression e);
    }
}
