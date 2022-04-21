/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

public class CheapSet<T> {
    private CheapSet() {
    }
    public static CheapSet empty = new CheapSet();
    public int size() {
        return 0;
    }
    public boolean isEmpty() {
        return true;
    }
    public boolean contains(T o) {
        return false;
    }
    public Object[] toArray() {
        return new Object[0];
    }
    public T[] toArray(T[] a) {
        return a;
    }
    public CheapSet<T> add(T e) {
        return e==null ? this : of(e, null);
    }
    public CheapSet<T> addAll(T... e) {
        CheapSet<T> ret = this;
        for(T v: e)
            ret = ret.add(v);
        return ret;
    }
    public CheapSet<T> addAll(Collection<T> e) {
        CheapSet<T> ret = this;
        for(T v: e)
            ret = ret.add(v);
        return ret;
    }
    public CheapSet<T> remove(T o) {
        return this;
    }
    public void forEach(Consumer<T> f) {
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append('{');
        forEach(v -> {
            if(sb.length()>1)
                sb.append(',');
            sb.append(v);
        });
        return sb.append('}').toString();
    }
    public static <T> CheapSet<T> of(T e) {
        return e==null ? empty : of(e, null);
    }
    public static <T> CheapSet<T> ofAll(T[] e) {
        return empty.addAll(e);
    }
    public static <T> CheapSet<T> ofAll(Collection<T> e) {
        return empty.addAll(e);
    }
    public static <T> CheapSet<T> of(final T a, final T b) {
        if(a==b)
            return a==null ? empty : of(a, null);
        if(a==null)
            return of(b, null);
        return new CheapSet<T>() {
            // a!=null; a!=b
            // a one-or-two element set
            @Override
            public int size() {
                return b==null ? 1 : 2;
            }
            @Override
            public boolean isEmpty() {
                return false;  // !!
            }
            @Override
            public boolean contains(T o) {
                return o!=null&&(o.equals(a)||o.equals(b));
            }
            @Override
            public Object[] toArray() {
                return new Object[0];
            }
            @Override
            public T[] toArray(T[] a) {
                return a;
            }
            @Override
            public CheapSet<T> remove(T o) {
                return a==o ? of(b, null)
                        : b==o ? of(a, null)
                                : this;
            }
            @Override
            public void forEach(Consumer<T> f) {
                f.accept(a);
                if(b!=null)
                    f.accept(b);
            }
            @Override
            public CheapSet<T> add(T c) {
                return c==null||contains(c) ? this
                        : b==null ? of(a, c)
                                : new CheapSet<T>() {
                                    // a, b & c are distinct non-null
                                    // promote to being a full-fledged set
                                    final Set<T> body = new CopyOnWriteArraySet<>();
                                    @Override
                                    public int size() {
                                        return body.size();
                                    }
                                    @Override
                                    public boolean isEmpty() {
                                        return body.isEmpty();
                                    }  // !!
                                    @Override
                                    public boolean contains(T o) {
                                        return body.contains(o);
                                    }
                                    @Override
                                    public Object[] toArray() {
                                        return body.toArray();
                                    }
                                    @Override
                                    public T[] toArray(T[] a) {
                                        return body.toArray(a);
                                    }
                                    @Override
                                    public CheapSet<T> add(T e) {
                                        body.add(e);
                                        return this;
                                    }
                                    @Override
                                    public CheapSet<T> remove(T o) {
                                        body.remove(o);
                                        return body.isEmpty() ? empty : this;
                                    }
                                    @Override
                                    public void forEach(Consumer<T> f) {
                                        body.forEach(f);
                                    }
                                }.add(a).add(b).add(c);
            }
        };
    }
    public static <T> CheapSet<T> of(T a, T b, T c, T... rest) {
        var ret = of(a, b).add(c);
        for(T d: rest)
            ret = ret.add(d);
        return ret;
    }
}
