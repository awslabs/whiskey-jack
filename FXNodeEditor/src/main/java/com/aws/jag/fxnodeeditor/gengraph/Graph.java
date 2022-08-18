/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;
import java.util.function.*;

public abstract class Graph<N extends Node, P extends Port, A extends Arc, G extends Graph> extends GraphPart/*sic*/ {
    private final Map<String, GraphPart> map = new HashMap<>();
    private final Class<N> nodeClass;
    private final Class<P> portClass;
    private final Class<A> arcClass;
    protected Graph(Class<N> n, Class<P> p, Class<A> a) {
        nodeClass = n;
        portClass = p;
        arcClass = a;
    }
    public GraphPart get(String uid) {
        return map.get(uid);
    }
    public GraphPart computeIfAbsent(String uid, Function<String, GraphPart> f) {
        return map.computeIfAbsent(uid, f);
    }
    private <T extends GraphPart> T create(Class<T> c, T original) {
        try {
            return (T)c.getConstructor(Graph.class, c).newInstance(this, original);
        } catch(Throwable ex) {
            ex.printStackTrace(System.out);
            throw ex instanceof RuntimeException E ? E : new IllegalStateException("Couldn't create "+c,ex);
        }
    }
    @SuppressWarnings({"PMD.AvoidCatchingGenericException","checkstyle:IllegalCatch","CatchGenericClass"}) // TODO: grrr
    private <T extends GraphPart> T create(Class<T> c, MetaPart mp) {
        try {
            return (T)c.getConstructor(Graph.class, mp.getClass()).newInstance(this, mp);
        } catch(Throwable ex) {
            ex.printStackTrace(System.out);
            throw ex instanceof RuntimeException E ? E : new IllegalStateException("Couldn't create "+c,ex);
        }
    }
    @Override
    public Graph<N,P,A,G> getContext() {
        return this;
    }
    public N newNode(N original) {
        return create(nodeClass, original);
    }
    public N newNode(MetaNode mn) {
        return create(nodeClass, mn);
    }
    public P newPort(P original) {
        return create(portClass, original);
    }
    public P newPort(MetaPort mn) {
        return create(portClass, mn);
    }
    public A newArc(P a, P b) {
        try {
            return arcClass.getConstructor(portClass,portClass).newInstance(a,b);
        } catch(Throwable ex) {
            ex.printStackTrace(System.out);
            throw ex instanceof RuntimeException E ? E : new IllegalStateException("Couldn't create Arc",ex);
        }
    }
    public void add(GraphPart aThis) {
        map.put(aThis.getUid(), aThis);
    }
    public void remove(GraphPart aThis) {
        map.remove(aThis.getUid());
    }
    public void copyTo(Graph dest) {
        map.forEach((k,v)->{
            if(v instanceof Node n) {
                dest.newNode(n);
//                c.copyFrom(n);
            }
        });
    }
}
