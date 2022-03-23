/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;
import java.util.function.*;

public class MNode implements Comparable<MNode> { // meta-node
    public MNode(String g, String n) {
        group = g;
        name = n;
    }
    public final String group;
    public final String name;
    final ArrayList<Port> ports = new ArrayList<>();
    MNode in(String name, Object dflt) {
        ports.add(new Port(nslot(true), true, name, dflt));
        return this;
    }
    MNode out(String name, Object dflt) {
        ports.add(new Port(nslot(false), false, name, dflt));
        return this;
    }
    private int nslot(boolean in) {
        int slot = 0;
        for(Port p: ports)
            if(p.in==in)
                slot++;
        return slot;
    }
    @Override
    public String toString() {
        var sb = new StringBuilder();
        sb.append(group);
        sb.append('.');
        sb.append(name);
        sb.append(':');
        lports(true, sb);
        sb.append("=>");
        lports(false, sb);
        return sb.toString();
    }
    private void lports(boolean in, StringBuilder sb) {
        var first = new boolean[]{true}; // I can't believe I'm doing this
        ports.forEach(p -> {
            if(p.in==in) {
                if(first[0]) first[0] = false;
                else sb.append(',');
                sb.append(p);
            }
        });
    }
    @Override
    public int compareTo(MNode o) {
        int r = group.compareToIgnoreCase(o.group);
        if(r==0) r = name.compareToIgnoreCase(o.name);
        return r;
    }
    class Port {
        Port(int s, boolean i, String n, Object d) {
            slot = s;
            in = i;
            name = n;
            dflt = d==null ? Boolean.FALSE : d;
        }
        final int slot;
        final boolean in;
        final String name;
        final Object dflt;
        @Override
        public String toString() {
            return tname(dflt)+" "+name;
        }
        String fullname() {
            return MNode.this.name+(in?"↓":"↑")+name;
        }
    }
    private static String tname(Object o) {
        return o.getClass().getCanonicalName();
    }
    public static final ArrayList<MNode> nodeKinds = new ArrayList<>();
    public static MNode add(String g, String n) {
        MNode mn = new MNode(g,n);
        nodeKinds.add(mn);
        return mn;
    }
    public static MNode find(String name) {
        for(var k:nodeKinds) {
            if(k.name.equalsIgnoreCase(name))
                return k;
        }
        return nodeKinds.get(0); // TODO this whole method is crap
    }
    static {
        // stopgap
        add("input","constant").out("v", 0);
        add("input","slider").out("v", 0);
        add("input","sin").in("freq",0.5).in("amp", 1).out("v", 0);
        add("output","chart").in("v", 0);
        add("output","log").in("v", 0).in("log", "node.log");
        add("filter","clamp").in("v", 0).in("min", 0).in("max", 100).out("v", 0);
        add("filter","expmean").in("v", 0).in("window", 10).out("v", 0);
        add("filter","boxmean").in("v", 0).in("window", 10).out("v", 0);
        add("filter","ratelimit").in("v", 0).in("limit", 2).out("v", 0);
        add("math","sum").in("a", 0).in("b", 0).out("v", 0);
        add("math","diff").in("a", 0).in("b", 0).out("v", 0);
    }
    public static void forEach(Consumer<MNode> f) {
        nodeKinds.stream().sorted().forEach(f);
    }
}
