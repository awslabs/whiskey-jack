/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.nodegraph.*;
import aws.WhiskeyJack.util.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

/**
 * Parsed Expression Node
 */
public class Expression { //TODO pick a less cryptic name
    private final Token operator;
    private final Expression[] args;
    private Type type;
    private Expression(Token k, Expression... a) {
        operator = k;
        args = squeezeNulls(a);
        type = k.isNumber() ? Type.number
                : k.isString() ? Type.string
                : Vocabulary.likelyType(k);
    }
    public static Expression of(Token k) {
        return new Expression(k, empty);
    }
    public static Expression of(Token k, Expression... a) {
        if(a == null || a.length == 0) return of(k);
        if(k==Vocabulary.DECLAREASSIGN) return of(Vocabulary.DECLARE, a);
        if(k==Vocabulary.DECLAREASSIGNFINAL)
            return of(Vocabulary.DECLARE, append(a,Vocabulary.finalMarker));
        return new Expression(k, a);
    }
    public static Expression of(Token k, Collection<Expression> a) {
        return of(k, a.stream());
    }
    public static Expression of(Token k, Stream<Expression> a) {
        return of(k, a.filter(v -> v != null).toArray(n ->
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
    public static Expression[] append(Expression[] list, Expression e) {
        if(e==null) return list;
        if(list==null || list.length==0) return new Expression[] { e };
        var len = list.length;
        var na = Arrays.copyOf(list, len+1);
        na[len] = e;
        return na;
    }
    public static Expression[] squeezeNulls(Expression... e) {
        var limit = e.length;
        for(var i = 0; i<limit; i++)
            if(e[i]==null) {
                var nulls = 1;
                System.out.println("Squeezing nulls from "+Utils.deepToString(e));
                while(++i<limit)
                    if(e[i]==null) nulls++;
                var squeezed = new Expression[limit-nulls];
                var j = 0;
                for(var k = 0; k<limit; k++) {
                    var v = e[k];
                    if(v!=null) squeezed[j++] = v;
                }
                assert j == squeezed.length;
                System.out.println("  Squeezed "+e.length+"=>"+squeezed.length);
                return squeezed;
            }
        return e;
    }
    public Type getType() { return type; }
    public Expression setType(Type t) {
        assert t!=null;
        type = t;
        return this;
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
    public Expression duplicate() {
        var len = args.length;
        if(len == 0) return of(operator);
        var na = new Expression[len];
        for(var i = 0; i < len; i++)
            na[i] = args[i].duplicate();
        return of(operator, na);
    }
    Expression clean() {
        return getOperator() != Vocabulary.BLOCK || length() != 1
                ? this
                : arg(0).clean();
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
                if(e1!=null) e1 = e1.rewrite(rewriter);
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
