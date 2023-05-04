/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.exl;

import java.util.*;
import java.util.function.*;

/**
 * Parsed Expression Node
 */
public class Expression { //TODO pick a less cryptic name
    private final Token kind;
    private final Expression[] args;
    private Expression(Token k, Expression... a) {
        kind = k;
        args = a;
    }
    public Expression duplicate() {
        var len = args.length;
        if(len==0) return of(kind);
        var na = new Expression[len];
        for(var i = 0; i<len; i++)
            na[i] = args[i].duplicate();
        return of(kind, na);
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
    public final Token kind() {
        return kind;
    }
    public final boolean isLeaf() {
        return !kind.isKeyword();
    }
    public final Expression arg(int n) {
        return args[n];
    }
    public final void forEach(Consumer<Expression> f) {
        for(var e: args)
            f.accept(e);
    }
    public final Expression[] asArray() { return args; }
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
        if(isLeaf()) kind.appendTo(sb);
        else {
            kind.appendTo(sb.append('('));
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
    @Override
    public int hashCode() {
        int hash = kind.hashCode();
        for(var a: args)
            hash = 63 * hash + a.hashCode();
        return hash;
    }
    @Override
    public boolean equals(Object v) {
        if(v instanceof Expression e
           && kind.equals(e.kind)
           && args.length == e.args.length) {
            for(int i = args.length; --i >= 0;)
                if(!args[i].equals(e.args[i]))
                    return false;
            return true;
        } else return false;
    }
    private static final Expression[] empty = new Expression[0];
}
