/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;
import javax.annotation.*;

public class MetaPort extends Port {
    public Type type;
    public Object defaultValue;
    public String name;
    public String description;
    public boolean in;
    public int y;
    @SuppressWarnings("LeakingThisInConstructor")
    public MetaPort(@Nonnull Node wi, MetaPort m) {
        super(wi, null);
    }
    public MetaPort(String n, String d, MetaNode wi, Type t, Object df, boolean i) {
        super(wi, null);
        name = n;
        description = d;
        type = t;
        defaultValue = df;
        in = i;
        y = wi.count(i);
    }
    public void markDirty() {
        ((MetaNode)within).markDirty();
    }
    @Override
    public String getName() {
        return name;
    }
    @Override
    public Type getType() {
        return type;
    }
    public void setName(String name) {
        this.name = name;
    }
    @Override
    public Graph getContext() {
        return within.getContext();
    }
    @Override
    public void populateFrom(Map values) {
        // TODO implement
    }
    @Override
    public String toString() {
        return getName();
    }
}
