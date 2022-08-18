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
            parent.newPort(p).within = this;
    }
    @SuppressWarnings("LeakingThisInConstructor")
    public Node(@Nonnull Graph parent, @Nonnull MetaNode mn) {
        context = parent;
        metadata = mn;
        for(var p:mn.ports.values())
            parent.newPort(p).within = this;
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
        return metadata.getName();
    }
}
