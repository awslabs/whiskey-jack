/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import com.aws.jag.fxnodeeditor.util.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;

public abstract class GraphPart<T extends GraphPart> implements Named {
    private String uid;
    public String comment;
    public abstract Graph getContext();
//    protected abstract void copyFromImpl(T source);
    @Override
    public abstract String toString(); // firce subclasses to implement toString
//    protected void copyFrom(T source) {
//        setUid(source.getUid());
//        copyFromImpl(source);
//    }
    public void setUid(String u) {
        if(uid!=null)
            getContext().remove(this);
        uid = u;
        if(u!=null) getContext().add(this);
    }
    public String getUid() {
        var u = uid;
        if(u==null)
            uid = u = uniquePrefix + sequenceNumber.incrementAndGet();
        return u;
    }
    private static final String uniquePrefix = Utils.generateRandomString(12);
    private static final AtomicInteger sequenceNumber = new AtomicInteger(0);
    private aListener listeners;
    public void fireListener(GEvent what, Object arg) {
        for(var p = listeners; p != null; p = p.next)
            if(p.tag == what)
                p.action.accept(this, arg);
    }
    public void removeListener(BiConsumer<GraphPart, Object> a) {
        aListener prev = null;
        for(var p = listeners; p != null; p = p.next)
            if(p.action != a)
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
}
