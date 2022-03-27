/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;

public class Layout {
    private final Map<String, LNode> byUid = new HashMap<>();
    final List<LNode> ordered;
    public Layout(Collection<FGNode> nodes) {
        nodes.forEach(fg -> {
            byUid.put(fg.uid, new LNode(fg));
        });
        // create upstream links
        byUid.values().forEach(f -> {
            f.n.inputs.forEach(in -> {
                if(in.comesFrom != null)
                    f.addUpstream(byUid.get(in.comesFrom.container.uid));
            });
        });
        ordered = byUid.values().stream().sorted((a,b)->{
            return a.directDownstream-b.directDownstream;
        }).toList();
    }
    public void run() {
        dump(ordered);
    }
    public void dump(Collection<LNode> c) {
        c.forEach(n->System.out.println(n));
    }
    public static class LNode { // Layout Node
        private final FGNode n;
        private final double x0, y0, w, h;
        private double x1, y1;
        int directDownstream = 0;
        final List<LNode> upstream = new ArrayList<>();
        int layer;
        boolean inParentChain = false;
        LNode(FGNode fg) {
            n = fg;
            x1 = x0 = n.view.getLayoutX();
            y1 = y0 = n.view.getLayoutY();
            w = n.view.getWidth();
            h = n.view.getHeight();
        }
        public void addUpstream(LNode u) {
            upstream.add(u);
            u.directDownstream++;
        }
        @Override public String toString() {
            return n.meta.name+":"+upstream.size()+","+directDownstream;
        }
    }
}
