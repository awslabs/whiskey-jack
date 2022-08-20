/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;

public class MetaNode implements Named, MetaPart {
    public Domain domain;
    public final List<MetaNode> alternates = new ArrayList<MetaNode>();
    public String name;
    public String description;
    public final Map<String,MetaPort> ports = new LinkedHashMap<>();
    @Override
    public String getName() {
        return name;
    }
    public MetaNode add(MetaPort p) {
        ports.put(p.name, p);
        return this;
    }
}
