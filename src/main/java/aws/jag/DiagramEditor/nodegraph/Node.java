/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import aws.jag.DiagramEditor.util.Utils;
import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import javax.annotation.*;

public class Node<T extends Node> extends GraphPart<T> {
    private String uid;
    private final Graph context;
    public final MetaNode metadata;
    public Node<T> copiedFrom;
    public final Map<String, Port> ports = new LinkedHashMap<>();
    @SuppressWarnings("LeakingThisInConstructor")
    public Node(@Nonnull Graph parent, MetaNode mn) {
        context = parent;
        if(mn == null) {
            mn = (MetaNode) this; // for the initial metaMeta circularity
            setName("NoName");
        } else
            setName(mn.getName());
        parent.add(this);
        metadata = mn;
//        System.out.println("Meta " + mn.getName() + " " + mn.ports.size());
        mn.ports.entrySet().forEach(e -> {
            var p = context.newPort(this, e.getValue().metadata);
            ports.put(e.getKey(), p);
            p.populateFrom(e.getValue());
        });
    }
    @Override
    public Graph getContext() {
        return context;
    }
    @Override
    public String getDescription() {
        return metadata.getDescription();
    }
    public void setUid(String u) {
        if(uid != null)
            getContext().remove(this);
        uid = u;
        if(u != null)
            getContext().add(this);
    }
    public String getUid() {
        var u = uid;
        if(u == null)
            uid = u = uniquePrefix + sequenceNumber.incrementAndGet();
//        System.out.println("UID: " + u);
        return u;
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append(uid);
    }
    private static final String uniquePrefix = Utils.generateRandomString(12);
    private static final AtomicInteger sequenceNumber = new AtomicInteger(0);
    public boolean hasUid() { // rarely needed
        return uid == null;
    }
    @Override
    public String opcode() {
        return "node";
    }
    @Override
    public String toString() {
        return appendTo(new StringBuilder()).toString();
    }
    public StringBuilder appendTo(StringBuilder sb) {
        appendNameTo(sb);
        sb.append('[');
        boolean first = true;
        for(var p: ports.entrySet()) {
            if(first)
                first = false;
            else
                sb.append(',');
            sb.append(p.getKey()).append('=');
            var v = p.getValue();
            if(v.isConnected()) {
                v.forEachArc(new Consumer<Arc>() {
                    char pfx = '{';
                    @Override
                    public void accept(Arc a) {
                        sb.append(pfx);
                        pfx = ',';
                        a.otherEnd(v).appendFullNameTo(sb);
                    }
                });
                sb.append('}');
            } else
                sb.append(v.getValue());
        }
        sb.append(']');
        return sb;
    }
    @Override
    public StringBuilder appendNameTo(StringBuilder sb) {
        super.appendNameTo(sb)
                .append('_')
                .append(getUid());
        return sb;
    }
    public void forEachPort(Consumer<? super Port> f) {
        ports.values().forEach(f);
    }
    public boolean hasPorts() {
        return !ports.isEmpty();
    }
    @Override
    protected void collectMore(Map<String, Object> map) {
        super.collectMore(map);
        putOpt(map, "ports", ports);
        putOpt(map, "uid", getUid());
        putOpt(map, "meta", metadata.getUid());
    }
    public Port defaultPort(boolean in) {
        var dp = metadata.defaultPort(in);
        return dp == null ? null : getPort(dp.getName());
    }
//    @Override
    public void populateFrom(Node other) {
//        super.populateFrom(other);
        assert metadata == other.metadata;
        assert ports.size() == other.ports.size();
        assert !hasUid();
        setUid(other.getUid());
        copiedFrom = other;
        forEachPort(p -> p.populateFrom(other.getPort(p.getName())));
    }
    public Port getPort(String s) {
        return ports.get(s);
    }
    @Override
    public void populateFrom(Map map) {
        super.populateFrom(map);
        setUid(get(map, "uid", null));
        populatePorts(map, "ports", false, false);
        populatePorts(map, "in", true, true); // these two lines are compatibility with an old format
        populatePorts(map, "out", false, true);
    }
    private void populatePorts(Map map, String key, boolean in, boolean rename) {
        getMap(map, key).forEach((k, v) -> {
            if(rename && k.equals("v"))
                k = in ? "in" : "out";
            var p = ports.get(k);
            if(p == null) {
                p = getContext().newPort(this, MetaPort.meta);
                ports.put(k, p);
                p.setName(k);
            }
            Map vmap;
            if(v instanceof Map pm)
                vmap = pm;
            else {
                vmap = new HashMap();
                vmap.put("value", v);
            }
            if(in)
                vmap.put("in", true);
            p.populateFrom(vmap);
        });
    }
}
