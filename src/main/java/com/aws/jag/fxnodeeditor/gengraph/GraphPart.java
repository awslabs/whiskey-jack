/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import com.aws.jag.fxnodeeditor.util.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public abstract class GraphPart<T extends GraphPart> extends Collectable {
    private String message;  // situation-specific message (eg. an error)
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
    public T setMessage(String m) {
        message = m;
        return (T) this;
    }
    public abstract Graph getContext();
//    protected abstract void copyFromImpl(T source);
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
    /* I should probably be using Jackson's built-in autoserializer, but I
     * like the control I get by hand-rolling */
    @Override
    public Object collect() {
        var ret = new HashMap<String,Object>();
        collectMore(ret);
        return ret;
    }
    public abstract String opcode();
    protected void collectMore(Map<String,Object> map) {
        putOpt(map, "op", opcode());
        putOpt(map, "name", getName());
        putOpt(map, "message", message);
    }
    public void populateFrom(Map<String,Object> values) {
        name = get(values,"name",name);
        message = get(values,"message",null);
    }
    private Map<Class,Object> sidecars;
    public <T> T sidecar(Class<T> cl) {
        if(sidecars==null) sidecars = new HashMap<>();
        return (T)sidecars.computeIfAbsent(cl, kcl->{
            try {
                return (T)kcl.getConstructor().newInstance();
            } catch(ReflectiveOperationException | RuntimeException  ex) {
                throw new Error("Trying to create "+cl.getSimpleName(), ex);
            }
        });
    }
    public void removeSidecar(Class<T> cl) {
        if(sidecars!=null) {
            sidecars.remove(cl);
            if(sidecars.isEmpty()) removeAllSidecars();
        }
    }
    public void removeAllSidecars() {
        sidecars = null;
    }
//    public abstract void populateFrom(T other);
    public static Object asObject(Object c) {
        return switch(c) {
            case null ->
                null;
            case Collectable o ->
                o.collect();
            case Collection l ->
                l.stream().map(o -> asObject(o)).collect(Collectors.toList());
            case Map m -> {
                var rm = new LinkedHashMap<>();
                m.forEach((k, v) -> rm.put(asObject(k), asObject(v)));
                yield rm;
            }
            default -> {
                if(c.getClass().isArray()) {
                    var len = Array.getLength(c);
                    var list = new ArrayList<Object>();
                    for(var i = 0; i < len; i++)
                        list.add(asObject(Array.get(c, i)));
                    yield list;
                }
                yield c;
            }
        };
    }
    public StringBuilder appendNameTo(StringBuilder sb) {
        sb.append(getName());
        return sb;
    }
}
