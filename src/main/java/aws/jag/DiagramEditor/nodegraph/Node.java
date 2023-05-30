/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import aws.jag.DiagramEditor.util.*;
import java.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;
import javax.annotation.*;

public class Node<T extends Node> extends GraphPart<T> {
    private String uid;
    protected Domain domain = Domain.unknown;
    private final Graph context;
    public final MetaNode metadata;
    public Node<T> copiedFrom;
    public final Map<String, Port> ports = new LinkedHashMap<>();
    @SuppressWarnings({"LeakingThisInConstructor", "OverridableMethodCallInConstructor"})
    public Node(@Nonnull Graph parent, MetaNode mn) {
        context = parent;
        if(mn == null) {
            mn = (MetaNode) this; // for the initial metaMeta circularity
            setName("NoName");
        } else
            setName(mn.getName());
        metadata = mn;
        parent.add(this);
        mn.ports.entrySet().forEach(e -> {
            var p = context.newPort(this, e.getValue().metadata);
            ports.put(e.getKey(), p);
            p.populateFrom(e.getValue());
        });
        setDomain(getDomain()); // to trigger change notification
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
        if(u != null) {
            uids.add(u);
            getContext().add(this);
        }
    }
    public String getUid() {
        var u = uid;
        if(u == null)
            uid = u = genUID(getName());
        return u;
    }
    private final static HashMap<String, AtomicInteger> uidMap = new HashMap<>();
    private final static Set<String> uids = new HashSet<>();
    public synchronized static String genUID(String pfx) {
        while(true) {
            var candidate = pfx + uidMap.computeIfAbsent(pfx, nm ->
                    new AtomicInteger(-1)).incrementAndGet();
            if(!uids.contains(candidate)) {
                uids.add(candidate);
                return candidate;
            }
        }
    }
    public Domain getDomain() {
        return domain == Domain.unknown ? metadata.getDomain() : domain;
    }
    public T setDomain(Domain d) {
        domain = d == null || d == Domain.any || d == metadata.getDomain() ? Domain.unknown : d;
        return (T) this;
    }

    @Override
    public void appendRefTo(StringBuilder sb) {
        sb.append(uid);
    }
    public boolean hasUid() { // rarely needed
        return uid == null;
    }
    @Override
    public String opcode() {
        return getName();
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
    public void forEachPort(Consumer<Port> f) {
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
        putOpt(map, "metapath", metadata.getPath());
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
    public Object getProp(String s, Object dflt) {
        return metadata.getProp(s, dflt);
    }
    public String getStringProp(String s, String dflt) {
        return Coerce.toString(getProp(s, dflt));
    }
    public boolean getBooleanProp(String s, boolean dflt) {
        return Coerce.toBoolean(getProp(s, dflt));
    }
    public int getIntProp(String s, int dflt) {
        return Coerce.toInt(getProp(s, dflt));
    }
    @Override
    public void populateFrom(Map map) {
        super.populateFrom(map);
        setUid(get(map, "uid", null));
        populatePorts(map, "ports", false, false);
        populatePorts(map, "in", true, true); // these two lines are compatibility with an old format
        populatePorts(map, "out", false, true);
    }
    private void populatePorts(Map map, String key, boolean input, boolean rename) {
        getMap(map, key).forEach((k, v) -> {
            if(rename && k.equals("v"))
                k = input ? "in" : "out";
            var p = ports.get(k);
            if(p == null) {
                if(!(this instanceof MetaNode))
                  return; // not found in the metadata
                p = getContext().newPort(this, MetaPort.meta);
                ports.put(k, p);
                p.setName(k);
            }
            Map vmap;
            if(v instanceof Map pm)
                vmap = pm;
            else {
//                if("out".equals(k) && v instanceof CharSequence cs)
//                    System.out.println("  outv "+v);
                vmap = new HashMap();
                var possibleT = Type.of(String.valueOf(v));
                /* If it looks like a type, it is a type.  Otherwise it's a value. */
                if(possibleT != null && possibleT != Type.any)
                    vmap.put("type", possibleT.getName());
                else vmap.put("value", v);
            }
            if(!input)
                vmap.put("output", true);
            p.populateFrom(vmap);
        });
    }
}
