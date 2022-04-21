/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.util.*;
import static com.nighthacks.fxnodeeditor.util.Utils.*;
import java.util.*;
import java.util.function.*;

public class MNode extends Collectable implements Comparable<MNode>  { // meta-node
    public MNode(MNode p, String n) {
        parent = p;
        name = n;
    }
    public final MNode parent;
    public final String name;
    public String tooltip;
    public String description;
    Map<String, MNode> children = null;
    final Map<String,Port> in = new LinkedHashMap<>();
    final Map<String,Port> out = new LinkedHashMap<>();
    MNode in(String name, Object dflt) {
        in.computeIfAbsent(name, n->new Port(nslot(true), true, n, dflt, false));
        return this;
    }
    MNode out(String name, Object dflt) {
        out.computeIfAbsent(name, n->new Port(nslot(false), false, n, dflt, false));
        return this;
    }
    public boolean isEmpty() { return in.isEmpty() && out.isEmpty(); }
    public boolean isRoot() { return false; }
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
    MNode createIfAbsent(String n) {
        if(children == null)
            children = new LinkedHashMap<>();
        return children.computeIfAbsent(n, nm -> new MNode(MNode.this, nm));
    }
    void merge(Object o) {
        if(o instanceof Map m) merge(m);
        else Dlg.error("While reading "+fullname(),"found unexpected value",deepToString(o));
    }
    void merge(Map m) {
        System.out.println("Merge into "+fullname());
        tooltip = Coerce.get(m, "tooltip", null);
        description = Coerce.get(m, "description", null);
        Coerce.getMap(m, "children").forEach((k,v)->createIfAbsent(Coerce.toString(k)).merge(v));
        Coerce.getMap(m, "in").forEach((k,v)->in(Coerce.toString(k),v));
        Coerce.getMap(m, "out").forEach((k,v)->out(Coerce.toString(k),v));
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
        if(parent != null && !parent.isRoot()) {
            parent.appendNameTo(sb);
            sb.append('.');
        }
        sb.append(name);
    }
    final String fullname() {
        var sb = new StringBuilder();
        appendNameTo(sb);
        return sb.toString();
    }
    void dump(int depth) {
        for(var i = depth; --i >= 0;)
            System.out.print("    ");
        System.out.println(this);
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
        return name.compareToIgnoreCase(o.name);
    }
    @Override
    public Object collect() {
        var m = new LinkedHashMap<String, Object>();
        if(!in.isEmpty())
            m.put("in", asObject(in));
        if(!out.isEmpty())
            m.put("out", asObject(out));
        if(children!=null && !children.isEmpty())
            m.put("children", asObject(children));
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
            m.put("default", dflt);
            if(multi) m.put("multi", multi);
            return m.size()==1 ? dflt : m;
        }
        @Override
        public String toString() {
            return NodeLibrary.tname(dflt) + " " + name;
        }
        String fullname() {
            return MNode.this.name + (in ? "↓" : "↑") + name;
        }
    }

    public void forEach(Consumer<MNode> f) {
        if(children == null)
            f.accept(this);
        else
            children.values().forEach(v->v.forEach(f));
    }
}
