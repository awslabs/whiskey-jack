/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;
import java.util.function.*;
import java.util.regex.*;

public class MNode extends Collectable implements Comparable<MNode>  { // meta-node
    public MNode(MNode p, String n) {
        parent = p;
        name = n;
        System.out.println("create "+fullname()+" in "+p);
    }
    public final MNode parent;
    public final String name;
    Map<String, MNode> children = null;
    final Map<String,Port> in = new LinkedHashMap<>();
    final Map<String,Port> out = new LinkedHashMap<>();
    MNode in(String name, Object dflt) {
        in.put(name, new Port(nslot(true), true, name, dflt, false));
        return this;
    }
    MNode out(String name, Object dflt) {
        out.put(name, new Port(nslot(false), false, name, dflt, false));
        return this;
    }
    public boolean isEmpty() { return in.isEmpty() && out.isEmpty(); }
    private int nslot(boolean isIn) {
        return (isIn ? in : out).size();
    }
    MNode get(String n) {
        return children == null ? null : children.get(n);
    }
    void remove(String n) {
        if(children != null) {
            children.remove(n);
            if(children.isEmpty())
                children = null;
        }
    }
    public void forAllChildren(Consumer<Port> c) {
        in.values().forEach(c);
        out.values().forEach(c);
    }
    MNode create(String n) {
        if(children == null)
            children = new LinkedHashMap<>();
        System.out.println("Create "+name+"."+n);
        return children.computeIfAbsent(n, nm -> new MNode(MNode.this, nm));
    }
    @Override
    public String toString() {
        var sb = new StringBuilder();
        appendNameTo(sb);
        sb.append(':');
        lports(true, sb);
        sb.append("=>");
        lports(false, sb);
        return sb.toString();
    }
    void appendNameTo(StringBuilder sb) {
        if(parent != null && parent != root) {
            parent.appendNameTo(sb);
            sb.append('.');
        }
        sb.append(name);
    }
    String fullname() {
        var sb = new StringBuilder();
        appendNameTo(sb);
        return sb.toString();
    }
    void dump(int depth) {
        for(var i = depth; --i >= 0;)
            System.out.print("    ");
//        System.out.println(this);
        if(children != null)
            children.forEach((n, m) -> m.dump(depth + 1));
    }
    private void lports(boolean isIn, StringBuilder sb) {
        var first = true;
        for(var p: (isIn ? in : out).values()) {
                if(first)
                    first = false;
                else
                    sb.append(',');
                sb.append(p);
            }
    }
    @Override
    public int compareTo(MNode o) {
//        int r = group.compareToIgnoreCase(o.group);
//        if(r==0) r = name.compareToIgnoreCase(o.name);
//        return r;
        return name.compareToIgnoreCase(o.name);
    }
    public MNode isFlat() {
        flatmap.put(name, this);
        return this;
    }
    @Override
    public Object collect() {
        var m = new LinkedHashMap<String, Object>();
//        m.put("name", name);
        if(!in.isEmpty())
            m.put("in", asObject(in));
        if(!out.isEmpty())
            m.put("out", asObject(out));
        if(children!=null && !children.isEmpty())
            m.put("children", asObject(children));
//        System.out.println("AsObject "+name+" "+m.getClass());
        return m;
    }
    public class Port extends Collectable {
        Port(int s, boolean i, String n, Object d, boolean m) {
            slot = s;
            in = i;
            multi = m;
            name = n;
            dflt = d == null ? Boolean.FALSE : d;
        }
        final int slot;
        final boolean in;
        final boolean multi;
        final String name;
        final Object dflt;
        @Override
        public Object collect() {
            var m = new LinkedHashMap<String, Object>();
//            m.put("name", name);
            m.put("default", dflt);
            if(multi) m.put("multi", multi);
//            m.put("in", in);
            return m.size()==1 ? dflt : m;
        }
        @Override
        public String toString() {
            return tname(dflt) + " " + name;
        }
        String fullname() {
            return MNode.this.name + (in ? "↓" : "↑") + name;
        }
    }
    private static String tname(Object o) {
        return o.getClass().getCanonicalName();
    }
    public static final MNode root = new MNode(null, "root");
    public static final Map<String, MNode> flatmap = new HashMap<>();
    public static MNode add(String g, String n) {
        return root.create(g).create(n).isFlat();
    }
    public static MNode rootCreate(String... names) {
        if(names.length == 1)
            return flatmap.get(names[0]);
        var v = root;
        for(var name: names)
            v = v.create(name);
//        System.out.println(Arrays.toString(names) + ": " + v);
        return v;
    }
    public static String[] expand(String s) {
        return joiners.split(s);
    }
    private static final Pattern joiners = Pattern.compile(" *[.,:/] *");
    static {
        // stopgap
        add("input", "constant").out("v", 0);
        add("input", "slider").out("v", 0);
        add("input", "sin").in("freq", 0.5).in("amp", 1).out("v", 0);
        add("output", "chart").in("v", 0);
        add("output", "log").in("v", 0).in("log", "node.log");
        add("filter", "clamp").in("v", 0).in("min", 0).in("max", 100).out("v", 0);
        add("filter", "expmean").in("v", 0).in("window", 10).out("v", 0);
        add("filter", "boxmean").in("v", 0).in("window", 10).out("v", 0);
        add("filter", "ratelimit").in("v", 0).in("limit", 2).out("v", 0);
        add("math", "sum").in("a", 0).in("b", 0).out("v", 0);
        add("math", "diff").in("a", 0).in("b", 0).out("v", 0);
        add("math", "eval").in("a", 0).in("b", 0).in("c", 0).in("expr", "a+b").out("v", 0);
        root.dump(0);
    }
    public static void forAll(Consumer<MNode> f) {
        System.out.println("\nFA "+root.fullname());
        root.forEach(f);
    }
    public void forEach(Consumer<MNode> f) {
        System.out.println("[ FE "+fullname());
//        new Throwable("forEach").printStackTrace();
        if(children == null)
            f.accept(this);
        else
            children.values().forEach(f);
        System.out.println("  FE "+fullname()+" ]");
    }
}
