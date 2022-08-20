/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;
import javax.annotation.*;

public class Node extends GraphPart implements Named {
    private final Graph context;
    public MetaNode metadata;
    public final List<Port> ports = new LinkedList<>();
    @SuppressWarnings("LeakingThisInConstructor")
    public Node(@Nonnull Graph parent, @Nonnull Node original) {
        context = parent;
        metadata = original.metadata;
        for(var p:original.ports)
            add(parent.newPort(p));
    }
    @SuppressWarnings("LeakingThisInConstructor")
    public Node(@Nonnull Graph parent, @Nonnull MetaNode mn) {
        context = parent;
        metadata = mn;
        System.out.println("Meta "+mn.name+" "+mn.ports.size());
        for(var mp:mn.ports.values()) {
            System.out.println("  "+mp.name);
            add(parent.newPort(mp));
        }
    }
    public final void add(Port p) {
        p.within = this;
        ports.add(p);
    }
    @Override
    public Graph getContext() {
        return context;
    }
    @Override
    public String getName() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(metadata.getName()).append('[');
        boolean first = true;
        for(var p:ports) {
            if(first) first = false;
            else sb.append(',');
            sb.append(p.metadata.name);
            if(p.isConnected()) sb.append('@').append(p.nArcs());
            else sb.append('=').append(p.constantValue);
        }
        sb.append(']');
        return sb.toString();
    }
}
