/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import aws.jag.DiagramEditor.util.Utils;
import aws.jag.DiagramEditor.util.Collectable;
import static aws.jag.DiagramEditor.nodegraph.GraphPart.*;
import java.util.*;
import java.util.function.*;
import javax.annotation.*;

public class Port extends Collectable {
    public final MetaPort metadata;
    public final Node within;
    private Object constantValue; // used when disconnected
    private List<Arc> arcs;
    public boolean isConnected() {
        return arcs != null;
    }
    public Port(@Nonnull Node wi, MetaPort m) {
        within = wi;
        metadata = m != null ? m : (MetaPort) this;  // Metaport's metadata is a circular reference.
    }
    public void populateFrom(Port other) {
        constantValue = other.constantValue;
        other.forEachArc(a -> System.out.println("  mk arc " + a));
    }
    public void populateFrom(Map values) {
        constantValue = getOpt(values,"value",constantValue);
        getCollection(values, "arcs").forEach(s->getContext().addConnection(s.toString()));
    }
    public void remove(Arc a) {
        if(arcs != null) {
            arcs.remove(a);
            if(arcs.isEmpty())
                arcs = null;
        }
    }
    public void add(Arc a) {
        if(arcs == null)
            arcs = new ArrayList<>();
        arcs.add(a);
    }
    public Iterable<Arc> allArcs() { return arcs==null ? Collections.emptyList() : arcs; }
    public void forEachArc(Consumer<? super Arc> f) {
        if(arcs != null)
            arcs.forEach(f);
    }
    public int nArcs() {
        return arcs == null ? 0 : arcs.size();
    }
    public boolean connectsTo(Port x) {
        if(arcs != null)
            for(var a: arcs)
                if(a.connectsTo(x))
                    return true;
        return false;
    }
    public void connectTo(Port n) {
        Arc.connect(this, n);
    }
    @Override
    public Object collect() {
        var ret = new HashMap<String,Object>();
        collectMore(ret);
        return ret;
    }
    protected void collectMore(Map<String,Object> map) {
        putOpt(map, "arcs", arcs);
//        putOpt(map, "meta", metadata.getUid());
        putOpt(map, "value", constantValue);
    }
    public Graph getContext() {
        return within.getContext();
    }
    public String getFullName() {
        return appendFullNameTo(new StringBuilder()).toString();
    }
    public StringBuilder appendFullNameTo(StringBuilder sb) {
        return within.appendNameTo(sb)
               .append('.')
               .append(getName());
    }
    public String getName() {
        return metadata.getName();
    }
    public void setName(String name) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        within.appendRefTo(sb);
        sb.append('.').append(getName());
    }
    public String getDescription() {
        return metadata.getDescription();
    }
    public Type getType() {
        return metadata.getType();
    }
    public boolean isRightSide() {
        return metadata.isRightSide();
    }
    public boolean isLeftSide() {
        return !metadata.isRightSide();
    }
    @Override
    public String toString() {
        return "Port<" + within.getName() + "." + getName() + ">";
    }
    public final void setParsedValue(String v) {
        setValue(Utils.parseObject(v));
    }
    public void setValue(Object v) {
        constantValue = v;
        var original = within.copiedFrom;
        if(original!=null)
            original.getPort(getName()).setValue(v);
    }
    public Object getValue() { return constantValue; }
}
