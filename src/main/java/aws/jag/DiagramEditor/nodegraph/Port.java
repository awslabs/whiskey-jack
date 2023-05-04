/*
 * SPDX-FileCopyrightText: Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import aws.jag.DiagramEditor.nodegraph.Graph.PendingConnection;
import aws.jag.DiagramEditor.util.Utils;
import aws.jag.DiagramEditor.util.Collectable;
import static aws.jag.DiagramEditor.nodegraph.GraphPart.*;
import java.util.*;
import java.util.function.*;

public class Port extends Collectable {
    public final MetaPort metadata;
    public Domain domain = Domain.unknown;
    private ErrorCode errorCode;
    public final Node within;
    private Object constantValue; // used when disconnected
    private List<Arc> arcs;
    public boolean isConnected() {
        return arcs != null;
    }
    public Port(Node wi, MetaPort m) {
        within = wi;
        metadata = m != null ? m : (MetaPort) this;  // Metaport's metadata is a circular reference.
    }
    public void populateFrom(Port other) {
        constantValue = other.constantValue;
        domain = other.domain;
        other.forEachArc(a -> System.out.println("  mk arc " + a));
    }
    public void populateFrom(Map values) {
        constantValue = getOpt(values,"value",constantValue);
        String d = getOpt(values,"domain",(String) null);
        if(d!=null)
            setDomain(Domain.of(d));
        getCollection(values, "arcs").forEach(s->{
            if(s instanceof Map arc) {
                getContext().addConnection(new PendingConnection(this,
                        (String) arc.get("toUid"),
                        (String) arc.get("toPort")));
            }
        });
    }
    public void remove(Arc a) {
        if(arcs != null) {
            arcs.remove(a);
            if(arcs.isEmpty())
                arcs = null;
        }
    }
    public void add(Arc a) {
        if(arcs == null)
            arcs = new ArrayList<>();
        arcs.add(a);
    }
    public Iterable<Arc> allArcs() { return arcs==null ? Collections.emptyList() : arcs; }
    public void forEachArc(Consumer<? super Arc> f) {
        if(arcs != null)
            arcs.forEach(f);
    }
    public int nArcs() {
        return arcs == null ? 0 : arcs.size();
    }
    public boolean connectsTo(Port x) {
        if(arcs != null)
            for(var a: arcs)
                if(a.connectsTo(x))
                    return true;
        return false;
    }
    public boolean compatibleWith(Port b) {
        return getDomain().compatibleWith(b.getDomain())
                && getType().compatibleWith(b.getType());
    }
    public void connectTo(Port n) {
        Arc.connect(this, n);
    }
    @Override
    public Object collect() {
        var ret = new HashMap<String,Object>();
        collectMore(ret);
        return ret;
    }
    protected void collectMore(Map<String,Object> map) {
        if(arcs!=null) {
            List<Map> arcList = new ArrayList<>(1);
            for(var thisArc:arcs)
                if(thisArc.oneEnd()==this) {
                    var nmap = new HashMap();
                    var otherEnd = thisArc.otherEnd();
                    putOpt(nmap,"toUid",otherEnd.within.getUid());
                    putOpt(nmap,"toPort",otherEnd.getName());
                    arcList.add(nmap);
                }
            putOpt(map, "arcs", arcList);
        }
        putOpt(map, "value", constantValue);
        if(domain != Domain.unknown) putOpt(map, "domain", domain);
    }
    public Graph getContext() {
        return within.getContext();
    }
    public String getFullName() {
        return appendFullNameTo(new StringBuilder()).toString();
    }
    public StringBuilder appendFullNameTo(StringBuilder sb) {
        return within.appendNameTo(sb)
               .append('.')
               .append(getName());
    }
    public String getName() {
        return metadata.getName();
    }
    public void setName(String name) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        within.appendRefTo(sb);
        sb.append('.').append(getName());
    }
    public String getDescription() {
        return metadata.getDescription();
    }
    public Type getType() {
        return metadata.getType();
    }
    public Domain getDomain() {
        return domain == Domain.unknown ? within.getDomain() : domain;
    }
    public Port setDomain(Domain d) {
        System.out.println("Port setDomain "+getFullName()+"->"+d);
        if(d==within.getDomain()) d = Domain.unknown;
        domain = d;
        return this;
    }
    public boolean isOutputSide() {
        return metadata.isOutputSide();
    }
    public boolean isInputSide() {
        return !metadata.isOutputSide();
    }
    public boolean isUnboundOK() {
        return metadata.isUnboundOK();
    }
    @Override
    public String toString() {
        return "Port<" + within.getName() + "." + getName() + ">";
    }
    public String dtString() {
        return getName()+'['+getDomain()+','+getType().getName()+']';
    }
    public final void setParsedValue(String v) {
        setValue(Utils.parseObject(v));
    }
    public void setValue(Object v) {
        constantValue = v;
        var original = within.copiedFrom;
        if(original!=null)
            original.getPort(getName()).setValue(v);
    }
    public Object getValue() { return constantValue; }
    private String message;
    public final String getMessage() { return message; }
    public Port setMessage(ErrorCode ec, String m) {
        errorCode = ec==null ? ErrorCode.allIsWell : ec;
        message = m;
        return this;
}
    public final Port setMessage(String m) {
        return setMessage(ErrorCode.allIsWell, m);
    }
    public ErrorCode getErrorCode() { return errorCode; }
}
