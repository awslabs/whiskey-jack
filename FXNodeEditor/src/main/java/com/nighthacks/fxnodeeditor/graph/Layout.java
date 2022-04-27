/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;
import javafx.animation.*;
import javafx.util.*;

public class Layout {
    static final double xpad = 50;
    static final double ypad = 25;
    private final Map<String, LNode> byUid = new HashMap<>();
    final List<LNode> ordered;
    int ncols = 1;
    Column[] columns;
    final double centerx, centery;
    public Layout(Collection<FGNode> nodes) {
        if(nodes != null && !nodes.isEmpty()) {
            nodes.forEach(fg -> {
                byUid.put(fg.uid, new LNode(fg));
            });
            // create upstream links
            byUid.values().forEach(f -> {
                f.flowgraph.inputs.forEach(in -> {
                    if(in.comesFrom != null)
                        f.addUpstream(byUid.get(in.comesFrom.container.uid));
                });
            });
            ordered = byUid.values().stream()
                    .sorted((a, b) -> a.directDownstream - b.directDownstream)
                    .toList();
            var first = ordered.get(0);
            var xmin = first.x0;
            var xmax = xmin + first.w;
            var ymin = first.y0;
            var ymax = ymin + first.h;
            for(var n: ordered) {
                n.assignColumns("ROOT", 0);
                if(n.x0 < xmin)
                    xmin = n.x0;
                if(n.x0 + n.w > xmax)
                    xmax = n.x0 + n.w;
                if(n.y0 < ymin)
                    ymin = n.y0;
                if(n.y0 + n.h > ymax)
                    ymax = n.y0 + n.h;
            }
            centerx = (xmin + xmax) / 2;
            centery = (ymin + ymax) / 2;
            columns = new Column[ncols];
            for(var i = 0; i < ncols; i++)
                columns[i] = new Column(i);
            ordered.forEach(n -> columns[n.column].add(n));
            for(var i = 1; i<ncols; i++) {
                columns[i-1].labelInputOrder();
                columns[i].sort();
            }
        } else {
            ordered = List.of();
            centerx = centery = 0;
        }
    }
    public Layout trivialLayout() {
        if(ordered.isEmpty())
            return this;
        double totalw = 0;
        double maxh = 0;
        for(var c: columns) {
            totalw += c.maxw;
            var h = c.height();
            if(h > maxh)
                maxh = h;
        }
        totalw += xpad * (columns.length - 1);
        var x = centerx - totalw / 2;
        for(var i = columns.length; --i >= 0;) {
            var c = columns[i];
            var y = centery - c.height() / 2;
            for(var n: c.nodes) {
                n.x1 = x;
                n.y1 = y;
                y += n.h + ypad;
            }
            x += c.maxw + xpad;
        }
        return this;
    }
    public void apply() {
        var pt = new ParallelTransition();
        for(var n: ordered)
            pt.getChildren().add(new Transition() {
                {
                    setCycleDuration(Duration.millis(1000));
                }
                @Override
                protected void interpolate(double frac) {
                    n.flowgraph.view.setLayoutX(n.x0 * (1 - frac) + n.x1 * frac);
                    n.flowgraph.view.setLayoutY(n.y0 * (1 - frac) + n.y1 * frac);
                }
            });
        pt.play();
    }
    private class Column {
        final ArrayList<LNode> nodes = new ArrayList<>();
        final int columnNumber;
        double maxw = 0;
        double height = 0;
        int x;
        void add(LNode n) {
            n.row = nodes.size();
            nodes.add(n);
            if(n.w > maxw)
                maxw = n.w;
            height += n.h;
        }
        Column(int cn) {
            columnNumber = cn;
        }
        public double height() {
            return height + ypad * (nodes.size() - 1);
        }
        public void labelInputOrder() {
            var pos = 0;
            for(var n:nodes)
                for(var p:n.flowgraph.inputs)
                    p.vizorder = ++pos;
        }
        public void sort() {
            nodes.sort((a,b)->{
                return rank(a) - rank(b);
            });
        }
        private int rank(LNode ln) {
            var n = ln.flowgraph;
            var sum = 0;
            var count = 0;
            for(var o:n.outputs)
                for(var p:o.goesTo) {
                    sum += p.vizorder;
                    count += 1;
                }
            return count==0 ? 0 : sum/count;
        }
    }
    private class LNode { // Layout Node
        private final FGNode flowgraph;
        private final double x0, y0, w, h;
        private double x1, y1;
        int directDownstream = 0;
        final List<LNode> upstream = new ArrayList<>();
        int column = -1;
        int row = 0;
        boolean inParentChain = false;
        LNode(FGNode fg) {
            flowgraph = fg;
            x1 = x0 = flowgraph.view.getLayoutX();
            y1 = y0 = flowgraph.view.getLayoutY();
            w = flowgraph.view.getWidth();
            h = flowgraph.view.getHeight();
        }
        public void addUpstream(LNode u) {
            if(upstream.contains(u)) {
                Dlg.error("Duplicate upstream: " + flowgraph.meta.name + "->" + u.flowgraph.meta.name, null);
                return;
            }
            upstream.add(u);
            u.directDownstream++;
        }
        @Override
        public String toString() {
            return flowgraph.meta.name + ":"
                    + x0 + "->" + x1 + ", " + y0 + "->" + y1;
        }
        void assignColumns(String caller, int level) {
            if(inParentChain)
                Dlg.error("Loop involving " + flowgraph.meta.name, null);
            else {
                if(level > column) {
                    column = level;
                    if(level >= ncols)
                        ncols = level + 1;
                }
                inParentChain = true;
                upstream.forEach(u -> u.assignColumns(caller + "->" + flowgraph.meta.name, level + 1));
                inParentChain = false;
            }
        }
    }
}
