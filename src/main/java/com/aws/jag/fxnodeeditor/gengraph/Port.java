/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import com.aws.jag.fxnodeeditor.util.*;
import java.util.*;
import java.util.function.*;
import javax.annotation.*;

public class Port /*extends GraphPart<Port>*/implements Iterable<Arc> {
    public final MetaPort metadata;
    public final Node within;
    public Object constantValue; // used when disconnected
    private List<Arc> arcs;
    public boolean isConnected() {
        return arcs != null;
    }
    public Port(@Nonnull Node wi, MetaPort m) {
        within = wi;
        metadata = m != null ? m : (MetaPort) this;  // Metaport's metadata is a circular reference.
    }
    @Override
    public Iterator<Arc> iterator() {
        return arcs.iterator();
    }
    public void populateFrom(Port other) {
        constantValue = other.constantValue;
        other.forEach(a -> System.out.println("  mk arc " + a));
    }
    public void populateFrom(Map values) {
        throw new UnsupportedOperationException("PFM Not supported yet.");
    }
    protected void collectMore(Map map) {
        GraphPart.putOpt(map, "value", constantValue);
        GraphPart.putOpt(map, "value", constantValue);
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
    public void forEach(Consumer<? super Arc> f) {
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
    public Type getType() {
        return metadata.getType();
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
}
