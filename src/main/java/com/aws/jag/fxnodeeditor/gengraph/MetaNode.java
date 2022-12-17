/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.gengraph;

import static com.aws.jag.fxnodeeditor.gengraph.Domain.*;
import java.util.*;
import java.util.function.*;

public class MetaNode extends Node<MetaNode> {
    private Domain domain = any;
    private final Map<String,MetaNode> subnodes = new HashMap<>();
    private final MetaNode parent;
    private String description;
    /* The root of the metadata hierarchy (kinda like class Class).
       Meta-circular subtlety: when this static variable is initialized, the
       meta root isn't known (cuz, this is it) so there's an odd null-text in the
       Node constructor to handle this */
    public static final MetaNode metaMeta = new MetaNode().setName("meta-meta");
    public MetaNode(MetaNode p) {
        super(metaGraph, metaMeta);
        if(p==null && (p = metaMeta)==null)
            p = this;  // only used when creating metaMeta
        parent = p;
        parent.addChild(p);
    }
    public MetaNode() {
        this(metaMeta);
    }
    public boolean isRoot() { return metaMeta==this; }
    public void addChild(MetaNode mn) { subnodes.put(mn.getName(), mn); }
    public void removeChild(MetaNode mn) { subnodes.remove(mn.getName()); }
    public void forEachChild(Consumer<MetaNode> f) { subnodes.values().forEach(f); }
    public boolean hasChildren() { return !subnodes.isEmpty(); }
    public MetaNode getParent() { return parent; }
    @Override
    public void populateFrom(Map values) {
        super.populateFrom(values);
        // TODO implement
    }
    @Override
    public String getDescription() {
        return description;
    }
    public MetaNode createIfAbsent(String name) {
        return subnodes.computeIfAbsent(name, n->new MetaNode(MetaNode.this).setName(n));
    }
    @Override
    public MetaNode setDescription(String d) {
        description = d;
        return this;
    }
    @Override
    protected void collectMore(Map<String,Object> map) {
        super.collectMore(map);
        putOpt(map,"domain", getDomain());
        putOpt(map,"ports",ports.values());
        putOpt(map,"description",description);
    }
    public MetaNode add(MetaPort p) {
        if(p.name==null || ports.containsKey(p.name)) {
            // generate locally-unique name to ease testing
            int i = 0;
            var nm = "";
            do {
                nm = "P" + ++i;
            } while(ports.containsKey(nm));
            p.name = nm;
        }
        ports.put(p.name, p);
        return this;
    }
    public static MetaNode lookup(String uid) {
        return null;
    }
    public Domain getDomain() {
        return domain;
    }
    public MetaNode setDomain(Domain d) {
        domain = d;
        return this;
    }
    @Override
    public MetaPort defaultPort(boolean in) { 
        MetaPort ret = null;
        var priority = 0;
        var magicName = in ? "in" : "out";
        for(var p:ports.values())
            if(p instanceof MetaPort mp && mp.in==in)
                if(mp.getName().equals(magicName))
                    return mp;
                else {
                    var tp = 1;
                    if(!mp.isConnected()) tp = 2;
                    if(tp>priority) {
                        priority = tp;
                        ret = mp;
                    }
                }
        return ret;
    }
    public void markDirty() { MetaNode.markDirty(this); }
    private static Set<MetaNode> dirty;
    public synchronized static void markDirty(MetaNode mn) { 
        var d = dirty;
        if(d==null) dirty = d = new HashSet<>();
        d.add(mn); }
    private synchronized static Set<MetaNode> captureDirty() {
        var d = dirty;
        dirty = null;
        return d;
    }
    public static void cleanAllDirty(Consumer<MetaNode> f) {
        var d = captureDirty();
        if(d!=null)
            d.forEach(f);
    }
    public static final Graph<MetaNode,MetaPort,Arc,Graph> metaGraph = 
            new Graph<>(MetaNode.class, MetaPort.class, Arc.class) {
        @Override
        public String getDescription() {
            return toString();
        }
        @Override
        public String toString() {
            return "Graph of all metadata";
        }
        { setName("All metanodes"); }
    };
    int count(boolean in) {
        var slot = 0;
        for(var p:ports.values()) if(((MetaPort)p).in == in)
            slot++;
        return slot;
    }
}
