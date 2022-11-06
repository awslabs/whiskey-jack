/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import org.junit.jupiter.api.*;

public class ViewTest {
    
    public ViewTest() {
        System.out.print("ViewTest");
    }
    @Test
    public void testSomeMethod() {
        System.out.println("ViewTest testSomeMethod");
        annotate();
    }
    
    public static void annotate(/*AnchorPane nodeEditor*/) {
        var m = new MetaNode()
                .setName("Add")
                .setDescription("add two things together")
                .setDomain(Domain.device);
        m.add(new MetaPort("in", null, m, Type.int_t, 0, true))
                .add(new MetaPort("b", null, m, Type.int_t, 0, true))
                .add(new MetaPort("out", null, m, Type.int_t, 0, false));
        var n = new Node(testGraph, m);
        n.getPort("in").setValue(42);
        n.getPort("b").setValue(1);
        var o = new Node(testGraph, m);
        o.getPort("in").setValue(-42);
        o.getPort("b").setValue(-1);
        o.getPort("out").connectTo(n.getPort("b"));
        testGraph.dump();
        MetaNode.metaGraph.dump();
        viewGraph.populateFrom(testGraph);
        viewGraph.dump();
//        var v = new View();
//        var nv = v.newNode(m);
//        var fx = nv.getView();
//        fx.setLayoutX(50);
//        fx.setLayoutY(50);
//        nodeEditor.getChildren().add(fx);
    }
    public static final Graph<Node, Port, Arc, Graph> testGraph
            = new Graph<>(Node.class, Port.class, Arc.class);
//    public static final Graph<NodeView, PortView, ArcView, Graph> viewGraph
//            = new Graph<>(NodeView.class, PortView.class, ArcView.class).setName("View");
    public static final Graph<Node, Port, Arc, Graph> viewGraph
            = new Graph<>(Node.class, Port.class, Arc.class).setName("View");
    static {
        testGraph.setName("testGraph");
        System.out.println("ViewTest static");
    }
}
