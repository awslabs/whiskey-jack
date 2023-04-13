/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.function.*;

public class Graph<N extends Node, P extends Port, A extends Arc, G extends Graph> extends GraphPart<Graph<N, P, A, G>>/*sic*//*sic*//*sic*//*sic*/ {
    private final Map<String, N> map = new HashMap<>();
    private final Class<N> nodeClass;
    private final Class<P> portClass;
    private final Class<A> arcClass;
    private final List<String> pendingConnections = new ArrayList<>();
    public Graph(Class<N> n, Class<P> p, Class<A> a) {
        nodeClass = n;
        portClass = p;
        arcClass = a;
    }
    public N get(String uid) {
        return map.get(uid);
    }
    public GraphPart computeIfAbsent(String uid, Function<String, N> f) {
        return map.computeIfAbsent(uid, f);
    }
    public P getPort(String s) {
        var dot = s.indexOf('.');
        if(dot <= 0)
            throw new IllegalArgumentException("Improper port reference: " + s);
        var n = get(s.substring(0, dot));
        if(n == null)
            throw new IllegalArgumentException("Undefined node reference in port: " + s);
        return (P) n.getPort(s.substring(dot + 1));
    }
    @Override
    public Graph<N, P, A, G> getContext() {
        return this;
    }
    @Override
    public String getDescription() {
        return toString();
    }
    @Override
    public String opcode() {
        return "graph";
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append("Graph:").append(getClass().getName());
    }
    public void addConnection(String con) {
        pendingConnections.add(con);
    }
//    @Override
    public void populateFrom(Graph<? super Node, ? super Port, ? super Arc, ? super Graph> other) {
        other.forEachNode(n -> newNode(n)); // copy the nodes
        other.forEachNode(node -> {
            // once all nodes exist, copy the interconnection graph
            node.forEachPort(port -> {
                ((Port) port).forEachArc(a -> {
                    // Each arc in the graph
                    // locate the endpoints of the arc in the new graph
                    var n1 = get(a.oneEnd().within.getUid());
                    var n2 = get(a.otherEnd().within.getUid());
                    assert n1 != null;
                    assert n2 != null;
                    // then the ports on the to-be-connected endpoints
                    var p1 = n1.getPort(a.oneEnd().getName());
                    var p2 = n2.getPort(a.otherEnd().getName());
                    assert p1 != null;
                    assert p2 != null;
                    Arc.connect(p1, p2);
                });
            });
        });
    }
    public void forEachNode(Consumer<? super Node> f) {
        map.values().forEach(f);
    }
    public void forEachPort(Consumer<Port> f) {
        forEachNode((var n) -> n.forEachPort(f));
    }
    public void forEachArc(Consumer<? super Arc> f) {
        forEachPort((Consumer<Port>) (Port p) -> {
            p.forEachArc((Arc a) -> {
                if(a.oneEnd() == p) // To only visit each arc once
                    f.accept(a);
            });
        });
    }
    @Override
    public String toString() {
        return getName();
    }
    public void checkTypes() {
        // something has happened to indicate that the type-correctness of
        // the graph may have changed
    }
    public void inferIntermediates() {
        // Try to fix the graph
    }
    public void layoutNodes() {
        // if this graph is being made visible (via a view) tidy it up
    }
    public N newNode(MetaNode mn) {
        try {
            return nodeClass.getConstructor(this.getClass(), MetaNode.class).newInstance(this, mn);
        } catch(RuntimeException ex) {
            ex.printStackTrace(System.out);
            throw ex;
        } catch(IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException ex) {
            ex.printStackTrace(System.out);
            throw new IllegalStateException("Couldn't create " + mn, ex);
        }
    }
    public N newNode(Map m) {
        var ret = newNode(MetaNode.lookup(get(m, "metadata", "unknown")));
        ret.populateFrom(m);
        return ret;
    }
    public N newNode(Node prev) {
        var n = newNode(prev.metadata);
        n.setUid(prev.getUid());
        n.populateFrom(prev);
        return n;
    }
    public P newPort(Node within, MetaPort meta) {
        try {
            return portClass.getConstructor(within.getClass(), meta.getClass()).newInstance(within, meta);
        } catch(RuntimeException ex) {
            ex.printStackTrace(System.out);
            throw ex;
        } catch(IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException ex) {
            ex.printStackTrace(System.out);
            throw new IllegalStateException("Couldn't create port " + meta, ex);
        }
    }
    public A newArc(String a, String b) {
        return newArc(getPort(a), getPort(b));
    }
    public A newArc(P a, P b) {
        try {
            return arcClass.getConstructor(portClass, portClass).newInstance(a, b);
        } catch(RuntimeException ex) {
            ex.printStackTrace(System.out);
            throw ex;
        } catch(IllegalAccessException | InstantiationException | NoSuchMethodException | InvocationTargetException ex) {
            ex.printStackTrace(System.out);
            throw new IllegalStateException("Couldn't create arc " + a + "<>" + b, ex);
        }
    }
    public void add(N aThis) {
        map.put(aThis.getUid(), aThis);
    }
    public void remove(N aThis) {
        map.remove(aThis.getUid());
    }
    public void copyTo(Graph dest) {
        map.values().forEach(v -> dest.newNode(v));
    }
    @Override
    public void populateFrom(Map values) {
    }
    @Override
    protected void collectMore(Map<String, Object> map) {
        super.collectMore(map);
    }
    public void dump(PrintStream p) {
        p.println("\nGraph " + getName());
        map.values().forEach(n -> System.out.println("  " + n));
        if(!pendingConnections.isEmpty()) {
            p.println("\tPending Connections:");
            pendingConnections.forEach(s -> p.println("\t\t" + s));
        }
    }
    public void dump() {
        dump(System.out);
    }
    public List<Arc> typeMismatches = new ArrayList<>();
    public boolean allOK = false;
}
