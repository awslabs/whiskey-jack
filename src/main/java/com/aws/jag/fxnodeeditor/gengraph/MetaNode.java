/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.gengraph;

import static com.aws.jag.fxnodeeditor.gengraph.Domain.*;
import java.util.*;

public class MetaNode extends Node<MetaNode> {
    private Domain domain = any;
    public final Set<MetaNode> alternates = new HashSet<>();
    private String description;
    public MetaNode() {
        super(metaGraph, metaMeta);
    }
    @Override
    public void populateFrom(Map values) {
        // TODO implement
    }
    @Override
    public String getDescription() {
        return description;
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
    public static final Graph<MetaNode,MetaPort,Arc,Graph> metaGraph = new Graph<>(MetaNode.class, MetaPort.class, Arc.class) {
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
    /* meta-circular subtlety: when this static variable is initialized, the
       meta root isn't known (cuz, this is it) so there's an odd null-text in the
       Node constructor to handle this */
    public static final MetaNode metaMeta = new MetaNode().setName("meta-meta");
}
