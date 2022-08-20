/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

public class MetaPort implements Named, MetaPart {
    public String name;
    public String description;
    public MetaNode parent;
    public Type type;
    public Object defaultValue;
    public boolean in;
    public MetaPort() {
    }
    public MetaPort(String n, String d, MetaNode p, Type t, Object df, boolean i) {
        name = n;
        description = d;
        parent = p;
        type = t;
        defaultValue = df;
        in = i;
    }
    @Override
    public String getName() {
        return name;
    }
}
