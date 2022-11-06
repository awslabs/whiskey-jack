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
    }
    @Override
    public String getName() {
        return name;
    }
    public Type getType() {
        return type;
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
