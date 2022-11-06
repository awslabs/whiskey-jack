/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

import java.util.*;

public class Arc extends GraphPart<Arc> {
    private final Port a;
    private final Port b;
    @SuppressWarnings("LeakingThisInConstructor")
    public Arc(Port A, Port B) {
        assert A != null && B != null;
        a = A;
        b = B;
        a.add(this);
        b.add(this);
    }
    @Override
    public Graph getContext() {
        return a.getContext();
    }
    @Override
    public String getDescription() {
        return toString();
    }
    @Override
    public String getName() {
        return a.getName() + "-" + b.getName();
    }
    public Port oneEnd() { return a; }
    public Port otherEnd() { return b; }
    public Port otherEnd(Port x) {
        return x == a ? b
             : x == b ? a
                      : null;
    }
    public boolean connectsTo(Port x) {
        return x == a || x == b;
    }
    public static void connect(Port a, Port b) {
        if(!a.connectsTo(b))
            a.getContext().newArc(a, b);
    }
    public void disconnect() {
        a.remove(this);
        b.remove(this);
    }
//    @Override
    public void populateFrom(Arc other) {
        throw new UnsupportedOperationException("PFOA Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }
    @Override
    protected void collectMore(Map<String,Object> map) {
        super.collectMore(map);
    }
    @Override
    public void populateFrom(Map<String, Object> values) {
        throw new UnsupportedOperationException("PFMA Not supported yet.");
    }
    @Override
    public String toString() {
        return "Arc<" + getName() + ">";
    }
}
