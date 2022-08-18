/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.gengraph;

public class Arc extends GraphPart {
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
    public String getName() {
        return a.getName() + "-" + b.getName();
    }
    public Port otherEnd(Port x) {
        if(x == a)
            return b;
        if(x == b)
            return a;
        throw new IllegalAccessError("Arc not connected to requested port");
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
    @Override
    public String toString() {
        return "Arc<" + getName() + ">";
    }
}
