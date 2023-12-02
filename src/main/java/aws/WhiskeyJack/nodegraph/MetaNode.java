/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodegraph;

import java.util.*;
import java.util.function.*;

public class MetaNode extends Node<MetaNode> {
    private final Map<String, MetaNode> subnodes = new HashMap<>();
    private final MetaNode parent;
    private String description;
    private String pathName;
    private MetaPort dfltIn, dfltOut;
    private float weight = 1; // how "expensive" is this computation
    private MetaNode(MetaNode p) {
        super(metaGraph, metaMeta);
        if(p == null && (p = metaMeta) == null)
            p = this;  // only used when creating metaMeta
        parent = p;
        domain = Domain.any;
    }
    private MetaNode() {  // only called when constructing metaMeta
        super(metaGraph, null);  // the null MetaNode argument is magically handled in Node
        parent = this;
        domain = Domain.any;
    }
    public boolean isRoot() {
        return metaMeta == this;
    }
    public void addChild(MetaNode mn) {
        System.out.println(" addChild " + mn.getName());
        subnodes.put(mn.getName(), mn);
    }
    public void removeChild(MetaNode mn) {
        subnodes.remove(mn.getName());
    }
    public void forEachChild(Consumer<MetaNode> f) {
        subnodes.values().forEach(f);
    }
    public void forEachLeaf(Consumer<MetaNode> f) {
        if(subnodes.isEmpty()) f.accept(this);
        else subnodes.values().forEach(m -> {
                m.forEachLeaf(f);
            });
    }
    public boolean hasChildren() {
        return !subnodes.isEmpty();
    }
    public MetaNode getParent() {
        return parent;
    }
    public String getPath() {
        var s = pathName;
        if(s == null)
            s = pathName = parent == null || parent == this || parent == metaMeta
                 ? getName()
                 : parent.getPath() + '/' + getName();
        return s;
    }
    @Override
    public void populateFrom(Map values) {
        super.populateFrom(values);
        setWeight((float)get(values, "weight", weight));
        setDomain(Domain.of(getStringProp("domain", getDomain().toString())));
        setDescription(get(values, "description", getDescription()));
        populateSubnodes(values, "subnodes");
        populateSubnodes(values, "children"); // compatibility for old name
    }
    private void populateSubnodes(Map values, String key) {
        getMap(values, key).forEach((k, v) -> {
            createIfAbsent(k).populateFrom((Map) v);
        });
    }
    @Override
    public String getDescription() {
        return description;
    }
    @Override
    public String opcode() {
        return "metanode";
    }
    public MetaNode createIfAbsent(String name) {
        return subnodes.computeIfAbsent(name, n ->
                new MetaNode(MetaNode.this).setName(n));
    }
    @Override
    public MetaNode setDescription(String d) {
        description = d;
        return this;
    }
    @Override
    protected void collectMore(Map<String, Object> map) {
        super.collectMore(map);
        putOpt(map, "domain", getDomain());
        putOpt(map, "description", description);
        putOpt(map, "subnodes", subnodes);
        if(weight != 1) putOpt(map, "weight", Float.toString(weight));
        putMissingProps(map);
    }
    public static MetaNode lookup(String uid) {
        return null;
    }
    @Override
    public Domain getDomain() {
        return domain;
    }
    @Override
    public Object getProp(String s, Object dflt) {
        var verbose = "domain".equals(s) && getName().equals("temperature");
        var mn = this;
        while(mn!=null) {
            var ret = mn.getProp0(s, dflt);
            if(verbose) System.out.println("***** "+mn.getName()+": "+ret);
            if(ret!=dflt) return ret;
            var p = mn.parent;
            if(p==mn) return dflt;
            mn = p;
        }
        return dflt;
    }
    @Override
    public MetaNode setDomain(Domain d) {
//        System.out.println(getName()+": "+d);
        domain = d == null || d == Domain.unknown ? Domain.any : d;
        return this;
    }
    @Override
    public MetaPort defaultPort(boolean in) {
        var ret = in ? dfltIn : dfltOut;
        if(ret == null) {
            var priority = 0;
            var magicName = in ? "in" : "out";
            for(var p: ports.values())
                if(p instanceof MetaPort mp && mp.isInputSide() == in)
                    if(mp.getName().equals(magicName))
                        return mp;
                    else {
                        var tp = 1;
                        if(!mp.isConnected())
                            tp = 2;
                        if(tp > priority) {
                            priority = tp;
                            ret = mp;
                        }
                    }
            if(in) dfltIn = ret;
            else dfltOut = ret;
        }
        return ret;
    }
    public void markDirty() {
        MetaNode.markDirty(this);
    }
    private static Set<MetaNode> dirty;
    public synchronized static void markDirty(MetaNode mn) {
        var d = dirty;
        if(d == null)
            dirty = d = new HashSet<>();
        d.add(mn);
    }
    private synchronized static Set<MetaNode> captureDirty() {
        var d = dirty;
        dirty = null;
        return d;
    }
    public static void cleanAllDirty(Consumer<MetaNode> f) {
        var d = captureDirty();
        if(d != null)
            d.forEach(f);
    }
    public static final Graph<MetaNode, MetaPort, Arc, Graph> metaGraph
            = new Graph<>(MetaNode.class, MetaPort.class, Arc.class) {
        @Override
        public String getDescription() {
            return toString();
        }
        @Override
        public String toString() {
            return "Graph of all metadata";
        }
        {
            setName("All metanodes");
        }
    };
    /* The root of the metadata hierarchy (kinda like class Class).
       Meta-circular subtlety: when this static variable is initialized, the
       meta root isn't known (cuz, this is it) so there's an odd null-text in the
       Node constructor to handle this */
    public static final MetaNode metaMeta = new MetaNode().setName("meta-meta");
    int count(boolean output) {
        var slot = 0;
        for(var p: ports.values())
            if(((MetaPort) p).isOutputSide() == output)
                slot++;
        return slot;
    }
    public float getWeight() {
        return weight;
    }
    public MetaNode setWeight(float w) {
        if(w <= 0) w = 1;
        weight = w;
        return this;
    }
}
