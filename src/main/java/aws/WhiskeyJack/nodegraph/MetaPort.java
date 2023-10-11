/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodegraph;

import java.util.*;
import javax.annotation.*;

public class MetaPort extends Port {
    private Type type = Type.any;
    private String name;
    private String description;
    private boolean outputSide;
    private boolean unboundOK;
    private boolean code = false;
    private int nodeSlot;
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
        p.setNodeSlot(wi.count(output));
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
        setName(getOpt(values, "name", getName()));
        setCode(get(values, "code", isCode()));
        setDescription(getOpt(values, "description", getDescription()));
        setOutputSide((boolean) getOpt(values, "output", isOutputSide()));
        if(getName().equals("id")) {
            unboundOK = true;
            setType(Type.string);
            setValue(null);
        }
        unboundOK = (boolean) getOpt(values, "unboundok", unboundOK);
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
        putOpt(map, "y", getNodeSlot());
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
    @Override
    public boolean isCode() {
        return code;
    }
    public void setCode(boolean ic) {
        code = ic;
    }
    public int getNodeSlot() {
        return nodeSlot;
    }
    private void setNodeSlot(int y) {
        this.nodeSlot = y;
    }
    public static final MetaPort markerMetaPort = new MetaPort(MetaNode.metaMeta);
}
