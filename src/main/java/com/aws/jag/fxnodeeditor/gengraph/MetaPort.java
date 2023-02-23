/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import static com.aws.jag.fxnodeeditor.gengraph.GraphPart.*;
import static com.aws.jag.fxnodeeditor.gengraph.Type.*;
import java.util.*;
import javax.annotation.*;

public class MetaPort extends Port {
    private Type type = Type.any_t;
//    public Object defaultValue;
    private String name;
    private String description;
    private boolean rightSide;
    private int y;
    private MetaPort(@Nonnull Node wi) {
        super(wi, null);
    }
    public MetaPort(@Nonnull MetaNode wi, MetaPort mp) {
        this(wi);
    }
    public static MetaPort of(String n, String d, MetaNode wi, Type t, Object df, boolean right) {
        var p = new MetaPort(wi);
        wi.ports.put(n,p);
        p.name = n;
        p.description = d;
        p.type = t;
        p.setValue(df);
        p.setRightSide(right);
        p.setY(wi.count(right));
        return p;
    }
    public void markDirty() {
        ((MetaNode)within).markDirty();
    }
    @Override
    public String getName() {
        return name;
    }
    @Override
    public String getDescription() {
        return description;
    }
    public void setDescription(String d) {
        description = d;
    }
    @Override
    public Type getType() {
        return type;
    }
    public void setType(Type t) {
        if(t!=null) type = t;
    }
    @Override
    public void setName(String name) {
        this.name = name;
    }
    @Override
    public Graph getContext() {
        return within.getContext();
    }
    @Override
    public void populateFrom(Map values) {
        super.populateFrom(values);
//        System.out.println("PopulateFrom");dump(values);
        setType(Type.of(getOpt(values, "type", getType().toString())));
        setName(getOpt(values, "name", getName()));
//        setValue(getOpt(values, "defaultValue", getValue()));
        if(getValue()!=null && getType()==any_t)
            setType(Type.guess(getValue()));
        setDescription(getOpt(values, "description", getDescription()));
        setRightSide((boolean) getOpt(values, "in", isRightSide()));// TODO: can be removed, eventually
        setRightSide((boolean) getOpt(values, "right", isRightSide()));
        setY((int) getOpt(values, "y", getY()));
    }
    @Override
    public void add(Arc a) {
        throw new IllegalAccessError("Arcs cannot be added to a MetaPort");
    }
    @Override
    protected void collectMore(Map<String,Object> map) {
        super.collectMore(map);
        putOpt(map, "type", type);
        putOpt(map, "name", name);
        putOpt(map, "description", description);
        putOpt(map, "right", isRightSide());
        putOpt(map, "y", getY());
    }
    @Override
    public String toString() {
        return getName();
    }
    static final MetaPort meta = new MetaPort(MetaNode.metaMeta);
    @Override
    public boolean isRightSide() {
        return rightSide;
    }
    private void setRightSide(boolean in) {
        this.rightSide = in;
    }
    public int getY() {
        return y;
    }
    private void setY(int y) {
        this.y = y;
    }
}
