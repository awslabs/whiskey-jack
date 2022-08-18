/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.gengraph;

import com.aws.jag.fxnodeeditor.util.*;
import java.util.*;
import java.util.function.*;
import javax.annotation.*;

public class Port extends GraphPart {
    public MetaPort metadata;
    public Node within;
    public Object constantValue; // used when disconnected
    private List<Arc> arcs;
    public boolean isConnected() { return arcs!=null; }
    public Port(@Nonnull Graph parent, @Nonnull Port original) {
        metadata = original.metadata;
        constantValue = original.constantValue;
    }
    public Port(@Nonnull Graph parent, @Nonnull MetaPort mn) {
        metadata = mn;
        constantValue = mn.defaultValue;
    }
    public void remove(Arc a) {
        if(arcs!=null) {
            arcs.remove(a);
            if(arcs.isEmpty()) arcs = null;
        }
    }
    public void add(Arc a) {
        if(arcs==null) arcs = new ArrayList<>();
        arcs.add(a);
    }
    public void forEach(Consumer<Arc> f) {
        if(arcs!=null) arcs.forEach(f);
    }
    public boolean connectsTo(Port x) { 
        if(arcs==null) return true;
        for(var a:arcs)
            if(a.connectsTo(x)) return true;
        return false;
    }
    @Override
    public Graph getContext() {
        return within.getContext();
    }
    @Override
    public String getName() {
        return within.getName()+"."+metadata.getName();
    }
    @Override
    public String toString() {
        return "Port<"+within.getName()+"."+metadata.getName()+">";
    }
    public void setValue(String v) {
        constantValue = Utils.parseObject(v);
    }
}
