/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.jag.DiagramEditor.nodegraph;

import static aws.jag.DiagramEditor.nodegraph.ErrorCode.*;
import aws.jag.DiagramEditor.util.*;
import java.util.*;
import java.util.function.*;

public abstract class GraphPart<T extends GraphPart> extends Collectable {
    private String message;  // situation-specific message (eg. an error)
    private ErrorCode errorCode = ErrorCode.allIsWell;  // TODO: not fleshed-out
    private String name = "unsetName";
    public abstract String getDescription(); // describe this part
    public String getMessage() { return message; }
    public String getName() { return name==null ? "nullName" : name; }
    public boolean isNamed() { return name!=null; }
    public T setDescription(String d) { // describe this part
        throw new IllegalAccessError("can't edit metadata.");
    }
    public T setName(String n) {
        if(n!=null) name = n;
        else
            System.out.println("Null setName "+name);
        return (T) this;
    }
    public T setMessage(ErrorCode ec, String m) {
        errorCode = ec==null ? allIsWell : ec;
        message = m;
        return (T) this;
    }
    public final T setMessage(String m) {
        return setMessage(allIsWell, m);
    }
    public abstract Graph getContext();
    @Override
    public abstract String toString(); // firce subclasses to implement toString
    private aListener listeners;
    public void fireListener(GEvent what, Object arg) {
        for(var p = listeners; p != null; p = p.next)
            if(p.tag == what)
                p.action.accept(this, arg);
    }
    public void removeListener(BiConsumer<GraphPart, Object> action) {
        aListener prev = null;
        for(var p = listeners; p != null; p = p.next)
            if(p.action != action)
                prev = p;
            else if(prev == null)
                listeners = p.next;
            else
                prev.next = p.next;
    }
    public void addListener(GEvent tag, BiConsumer<GraphPart, Object> action) {
        if(!hasListener(tag, action))
            listeners = new aListener(tag, action, listeners);
    }
    public boolean hasListener(Object tag, BiConsumer<GraphPart, Object> action) {
        for(var p = listeners; p != null; p = p.next)
            if(p.action == action)
                return true;
        return false;
    }
    private static class aListener {
        aListener(GEvent t, BiConsumer<GraphPart, Object> a, aListener n) {
            tag = t;
            action = a;
            next = n;
        }
        aListener next;
        final GEvent tag;
        final BiConsumer<GraphPart, Object> action;
    }
    public abstract String opcode();
    @Override
    protected void collectMore(Map<String,Object> map) {
        putOpt(map, "op", opcode());
        putOpt(map, "name", getName());
        putOpt(map, "message", message);
    }
    public void populateFrom(Map<String,Object> values) {
        name = get(values,"name",name);
        message = get(values,"message",null);
        getMap(values,"sidecars").forEach((k,v)->{
            System.out.println("Populating sidecar "+k+Utils.deepToString(v));
        });
    }
    public StringBuilder appendNameTo(StringBuilder sb) {
        sb.append(getName());
        return sb;
    }
}
