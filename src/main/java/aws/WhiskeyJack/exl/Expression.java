/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.exl;

import aws.WhiskeyJack.nodegraph.*;
import java.util.*;
import java.util.function.*;

/**
 * Parsed Expression Node
 */
public class Expression extends ArrayList<Expression> { //TODO pick a less cryptic name
    private final Token operator;
    private Type type;
    private Expression(Token k) {
        operator = k;
        type = k.isNumber() ? Type.number
                : k.isString() ? Type.string
                : Vocabulary.likelyType(k);
    }
    public static Expression of(Token k) {
        return new Expression(k);
    }
    public static Expression of(Token k, Expression... a) {
        if(a == null || a.length == 0) return of(k);
        if(k==Vocabulary.DECLAREASSIGN) return of(Vocabulary.DECLARE, a);
        if(k==Vocabulary.DECLAREASSIGNFINAL)
            return of(Vocabulary.DECLARE).add(a).add1(Vocabulary.finalMarker).finish();
        return of(k).add(a).finish();
    }
    public static Expression of(Token k, Collection<Expression> a) {
        return of(k).add(a).finish();
    }
//    public static Expression of(Token k, Stream<Expression> a) {
//        return of(k, a.filter(v -> v != null).toArray(n ->
//                new Expression[n]));
//    }
    public static boolean isEmpty(Expression e) {
        return e==null || e.getOperator()==Vocabulary.BLOCK && e.isEmpty();
    }
    @Override public boolean add(Expression e) {
        if(e==null) return true;
        if(getOperator()==Vocabulary.BLOCK && e.getOperator()==Vocabulary.BLOCK)
            for(var se:e) add(se);
        else
            if(!isEmpty(e)) super.add(e);
        return true;
    }
    @Override public boolean addAll(Collection<? extends Expression> c) {
        if(c!=null)
            for(var e:c) add(e);
        return true;
    }
    public Expression add(Expression... expressions) {
        if(expressions!=null)
            for(var e:expressions) add(e);
        return this;
    }
    public Expression add1(Expression e) {
        add(e);  // add with a single Expression has to return boolean
        return this;
    }
    public Expression add(Collection<Expression> expressions) {
        addAll(expressions);
        return this;
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
    public final Expression[] asArray() {
        return toArray(n->new Expression[n]);
    }
    public Expression duplicate() {
        var ret = of(getOperator());
        forEach(e->ret.add(e.duplicate()));
        return ret;
    }
    Expression clean() {
        if (getOperator() != Vocabulary.BLOCK) return this;
        var len = size();
        if(len==1) return get(0).clean();
        if(len==0) return null;
        if(!needsDeepClean()) return this;
        var a = new ArrayList<Expression>();
        for(var e:this)
            if(e!=null)
                if(e.getOperator()==Vocabulary.BLOCK) {
                    e.forEach(se->{
                        var cse = se.clean();
                        if(cse!=null) a.add(cse);
                    });
                } else a.add(e);
        if(a.isEmpty()) return null;
        if(a.size()==1)
            return a.get(0).clean();
        return of(Vocabulary.BLOCK, a);
    }
    static Expression clean(Expression e) {
        return e==null ? null : e.clean();
    }
    private boolean needsDeepClean() {
        for(var e:this)
            if(e==null || e.getOperator()==Vocabulary.BLOCK)
                return true;
        return false;
    }
    public boolean argsHaveSideEffect() {
        for(var a:this) if(a.hasSideEffect()) return true;
        return false;
    }
    public boolean hasSideEffect() {
        var ret = Vocabulary.hasSideEffect(operator) || argsHaveSideEffect();
        if(ret) System.out.println("    Has side effect: "+DomainCode.txt(this));
        return ret;
    }
    public boolean complex() {
        return !isLeaf();  // might get more complex
    }
    public Expression reduceToSideEffects() {
        return clean(collectSideEffects(null));
    }
    public Expression collectSideEffects(Expression container) {
        if(Vocabulary.hasSideEffect(operator)) {
            if(container==null) container = of(Vocabulary.BLOCK);
            container.add(this);
        }
        else for(var a:this) container = a.collectSideEffects(container);
        return container;
    }
    public final Collection<Expression> map(Function<Expression, Expression> m) {
        var ret = new ArrayList();
        for(var a: this)
            ret.add(m.apply(a));
        return ret;
    }
    public Expression finish() {
        if(getOperator()==Vocabulary.NEW && size()==1) {
            var typeExpr = getFirst();
            if(typeExpr.getOperator()==Vocabulary.INVOKE && !typeExpr.isEmpty())
                typeExpr = typeExpr.get(0);
            var nm = typeExpr.asName();
            setType(Type.of(nm));
            System.out.println("Finish: "+nm+" >> "+this);
        }
        else if(DomainCode.DeclarationInfo.of(this) instanceof DomainCode.DeclarationInfo decl) {
            var v = decl.initialValue;
            if(v!=null && v.getOperator()==Vocabulary.NEW) {
                setType(v.getType());
                System.out.println("DECL type "+getType()+" >> "+this);
            }
        }
        return this;
    }
    public String asName() {
        var op = getOperator();
        if(op==Vocabulary.DOT) {
            var sb = new StringBuilder();
            for(var e:this) {
                if(!sb.isEmpty()) sb.append('.');
                sb.append(e.asName());
            }
            return sb.toString();
        } else if(op.isIdentifier()) return op.getBody();
        else return "«"+this+"»";
    }
    public StringBuilder appendTo(StringBuilder sb) {
        if(sb == null) sb = new StringBuilder();
        if(isLeaf()) operator.appendTo(sb);
        else {
            sb.append('(');
//            sb.append(getType().toString()).append(":");
            operator.appendTo(sb);
            for(var a: this)
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
            for(var a: this)
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
           && size() == e.size()) {
            for(int i = size(); --i >= 0;)
                if(!get(i).equals(e.get(i)))
                    return false;
            return true;
        } else return false;
    }
    static int D = 0;
    public Expression rewrite(Rewriter rewriter) {
        D++;
        msg("Rewrite " + this);
        var chg = this;
        var i = 0;
        var limit = size();
        while(i < limit) {
            var e0 = get(i);
            var e1 = e0.rewrite(rewriter);
            if(e1 != e0) { // found change
                chg = of(getOperator());
                for(var j = 0; j<i; j++) chg.add(get(j)); // copy unchanged
                chg.add(e1);
                while(++i<limit) chg.add(this.get(i).rewrite(rewriter));
                break;
            }
            i++;
        }
        var ret = clean(rewriter.rewrite(chg));
        if(ret != this) msg("=> " + ret);
        D--;
        return ret;
    }
    public void visit(Consumer<Expression> visitor) {
        for(var e:this) e.visit(visitor);
        visitor.accept(this);
    }
    private void msg(String s) {
        System.out.printf("%-4d", D);
        for(var i = D; --i >= 0;)
            System.out.append("    ");
        System.out.println(s);
    }
    public Expression rename(Map<String,Expression> m) {
        return rewrite(exNode -> {
            var op = exNode.getOperator();
            if(op.isIdentifier()) {
                var nm = op.getBody();
                msg("Found Identifier " + nm);
                var foundValue = m.get(nm);
                if(foundValue != null) {
                    msg("  becomes " + foundValue);
                    return foundValue.duplicate().rename(m);  //TODO: this could recurse infinitely
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
