/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import java.util.*;
import javax.annotation.*;

public class MetaPort extends Port {
    private Type type = Type.any;
    private String name;
    private String description;
    private boolean outputSide;
    private boolean unboundOK;
    private int y;
    private MetaPort(@Nonnull Node wi) {
        super(wi, null);
    }
    public MetaPort(@Nonnull MetaNode wi, MetaPort mp) {
        this(wi);
    }
    public static MetaPort of(String n, String d, MetaNode wi, Type t, Object df, boolean output) {
        var p = new MetaPort(wi);
        wi.ports.put(n, p);
        p.name = n;
        p.description = d;
        p.type = t;
        p.setValue(df);
        p.setOutputSide(output);
        p.setY(wi.count(output));
        return p;
    }
    public void markDirty() {
        ((MetaNode) within).markDirty();
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
        if(t != null) type = t;
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
        setType(Type.of(getOpt(values, "type", getType().getName())));
        var proposedName = getOpt(values, "name", getName());
        setName(proposedName);
        setDescription(getOpt(values, "description", getDescription()));
        setOutputSide((boolean) getOpt(values, "output", isOutputSide()));
        if(getName().equals("id")) {
            unboundOK = true;
            setValue(null);
        }
        unboundOK = (boolean) getOpt(values, "unboundok", unboundOK);
        setY((int) getOpt(values, "y", getY()));
    }
    @Override
    public void add(Arc a) {
        throw new IllegalAccessError("Arcs cannot be added to a MetaPort");
    }
    @Override
    protected void collectMore(Map<String, Object> map) {
        super.collectMore(map);
        putOpt(map, "type", type.toString());
        putOpt(map, "name", name);
        putOpt(map, "description", description);
        putOpt(map, "output", isOutputSide());
        putOpt(map, "y", getY());
    }
    @Override
    public String toString() {
        return getName();
    }
    static final MetaPort meta = new MetaPort(MetaNode.metaMeta);
    @Override
    public boolean isUnboundOK() {
        return unboundOK;
    }
    @Override
    public boolean isOutputSide() {
        return outputSide;
    }
    private void setOutputSide(boolean in) {
        this.outputSide = in;
    }
    public int getY() {
        return y;
    }
    private void setY(int y) {
        this.y = y;
    }
}
