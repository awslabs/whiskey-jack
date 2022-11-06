/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.meta;

import com.aws.jag.fxnodeeditor.graph.*;
import static com.aws.jag.fxnodeeditor.meta.Port.*;
import com.aws.jag.fxnodeeditor.util.*;
import static com.aws.jag.fxnodeeditor.util.Utils.*;
import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.function.*;

/**
 * The meta information needed to understand a node.  It's like a "class" in
 * objected-oriented programming.
 */
public class MetaNode extends Collectable implements Comparable<MetaNode>  { 
    public MetaNode(MetaNode p, String n) {
        parent = p;
        name = n;
    }
    Path fromFile;
    public final MetaNode parent;
    public final String name;
    public String tooltip;
    public String description;
    public String domain;
    Map<String, MetaNode> children = null;
    final Map<String,Port> ports = new LinkedHashMap<>();
    public boolean hasOutputs() {
        return hasIO(false);
    }
    public boolean hasInputs() {
        return hasIO(true);
    }
    private boolean hasIO(boolean in) {
        for(var p:ports.values())
            if(in==p.in) return true;
        return false;
    }
    void createPort(String name, Object dflt, final boolean isIn) {
        if("v".equals(name)) name = isIn ? "in" : "out";
        var p = ports.computeIfAbsent(name, n->new Port(nslot(isIn), isIn, n, this));
        if(dflt instanceof Map m) {
            p.dflt = Coerce.get(m, "default", null);
            p.type = Coerce.get(m, "type", null);
            if(p.type==null) p.type = guessType(p.dflt);
            p.description = Coerce.get(m, "description", null);
        } else {
            p.dflt = dflt;
            p.type = guessType(dflt);
        }
    }
    public boolean isEmpty() { return ports.isEmpty(); }
    public boolean isRoot() { return false; }
    private int nslot(boolean isIn) {
        var ns = 0;
        for(var p:ports.values())
            if(isIn==p.in) ns++;
        return ns;
    }
    MetaNode get(String n) {
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
        ports.values().forEach(c);
    }
    MetaNode createIfAbsent(String n) {
        if(children == null)
            children = new LinkedHashMap<>();
        return children.computeIfAbsent(n, nm -> new MetaNode(MetaNode.this, nm));
    }
    void merge(Object o) {
        if(o instanceof Map m) merge(m);
        else Dlg.error("While reading "+fullname(),"found unexpected value",deepToString(o));
    }
    void merge(Map m) {
//        System.out.println("Merge into "+fullname());
        tooltip = Coerce.get(m, "tooltip", null);
        description = Coerce.get(m, "description", null);
        domain = Coerce.get(m, "domain", "Anywhere");
        Coerce.getMap(m, "children").forEach((k,v)->createIfAbsent(Coerce.toString(k)).merge(v));
        Coerce.getMap(m, "in").forEach((k,v)->createPort(Coerce.toString(k),v,true));
        Coerce.getMap(m, "out").forEach((k,v)->createPort(Coerce.toString(k),v,false));
        Coerce.getMap(m, "ports").forEach((k,v)->createPort(Coerce.toString(k),v,true));
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
    public final String fullname() {
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
        for(var p: ports.values())
            if(isIn==p.in) {
                if(first)
                    first = false;
                else
                    sb.append(',');
                sb.append(p);
            }
    }
    @Override
    public int compareTo(MetaNode o) {
        return name.compareToIgnoreCase(o.name);
    }
    @Override
    public Object collect() {
        var m = new LinkedHashMap<String, Object>();
//        if(!ports.isEmpty())
//            m.put("ports", Collectable.asObject(ports));
//    if(!in.isEmpty())
//            m.put("in", asObject(in));
//        if(!out.isEmpty())
//            m.put("out", asObject(out));
        collectChildren(m, true);
        collectChildren(m, false);
        if(children!=null && !children.isEmpty())
            m.put("children", Collectable.asObject(children));
        if(!"Anywhere".equals(domain))
            m.put("domain", domain);
        return m;
    }
    private void collectChildren(Map<String,Object> m, boolean in) {
    Map<String,Port> ret = new LinkedHashMap<>();
        ports.forEach((k,p)->{
            if(p.in == in)
                ret.put(k, p);
        });
        if(!ret.isEmpty()) {
            m.put(in ? "in" : "out", asObject(ret));
        }
    }

    public void forAllLeaves(Consumer<? super MetaNode> f) {
        if(children == null)
            f.accept(this);
        else
            children.values().forEach(v->v.forAllLeaves(f));
    }
    public void forEach(Consumer<? super MetaNode> f) {
        if(children != null)
            children.values().forEach(v->f.accept(v));
    }
    public void setDirty() {
        if(!dirty) {
            dirty = true;
            if(parent!=null) parent.setDirty();
        }
    }
    private void clearDirty() {
        if(dirty) {
            dirty = false;
            forEach(c->clearDirty());
        }
    }
    private boolean dirty;
    public void writeDirty(List<String> results) {
        if(dirty) {
            System.out.println("Write "+fullname()+": "+fromFile);
            if(fromFile !=null) {
                System.out.println("writing "+fullname()+" to "+fromFile);
                try(var out = CommitableWriter.abandonOnClose(fromFile)) {
                    NodeEditorController.fileio.writeValue(out, asObject(this));
                    out.commit();
                    results.add("Write "+fromFile);
                    System.out.println("Wrote "+fromFile);
                } catch(IOException ex) {
                    Dlg.error("Failed to write "+fullname(),"to file "+fromFile,ex);
                }
            } else {
                dirty = false;
                System.out.println("Loop "+children.size());
                forEach(m->m.writeDirty(results));
            }
            clearDirty();
        }
        else System.out.println("Clean "+fullname()+": "+fromFile);
    }
}
