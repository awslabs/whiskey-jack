/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodegraph;

import java.util.*;

public class Arc extends GraphPart<Arc> {
    private final Port a;
    private final Port b;
    @SuppressWarnings({"LeakingThisInConstructor","OverridableMethodCallInConstructor"})
    public Arc(Port A, Port B) {
        assert A != null && B != null;
        if(B.isOutputSide() && A.isInputSide()) {
            // ensure that a (oneEnd) is on the right side of it's node,
            // and b (otherEnd) is on the left.  Conventionally, think of
            // the left side as containing inputs, and the right side
            // containing outputs
            var t = A;
            A = B;
            B = t;
            System.out.println("Swap "+A+" "+B);
        }
        a = A;
        b = B;
        a.add(this);
        b.add(this);
        var an = a.within;
        var bn = b.within;
        if(bn.getDomain()==Domain.any)
            bn.setDomain(a.getDomain());
        else if(an.getDomain()==Domain.any)
            an.setDomain(b.getDomain());
        getContext().checkTypes();
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
    @Override
    public String opcode() {
        return "arc";
    }
    public Port otherEnd() { return b; }
    public Port otherEnd(Port x) {
        return x == a ? b
             : x == b ? a
                      : null;
    }
    @Override
    public Object collect() {
        return getRef();
    }
    @Override
    public void appendRefTo(StringBuilder sb) {
        a.appendRefTo(sb);
        sb.append("->");
        b.appendRefTo(sb);
    }
    public Port inOutPort(boolean in) {
        return a.metadata.isOutputSide()==in ? b : a;
    }
    public boolean connectsTo(Port x) {
        return x == a || x == b;
    }
    public static void connect(Port a, Port b) {
        if(a!=null && b!=null && !a.connectsTo(b))
            a.getContext().newArc(a, b);
    }
    public void delete() {
        a.remove(this);
        b.remove(this);
    }
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
        var sb = new StringBuilder();
        appendRefTo(sb);
        return sb.toString();
    }
}
