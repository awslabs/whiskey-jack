/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import static com.aws.jag.fxnodeeditor.gengraph.GraphPart.*;
import static com.aws.jag.fxnodeeditor.gengraph.MetaNode.*;
import com.aws.jag.fxnodeeditor.util.*;
import javafx.application.*;
import static javafx.application.Application.*;
import javafx.fxml.*;
import javafx.scene.Scene;
import javafx.stage.*;
import org.junit.jupiter.api.*;
//import com.aws.jag.fxnodeeditor.view.;

public class ViewTest extends Application {
    MetaNode m;
    Node n, o;
    public ViewTest() {
        try {
            System.out.print("ViewTest");
            m = metaMeta.createIfAbsent("Strange")
                    .setDescription("add two things together")
                    .setDomain(Domain.device);
            MetaPort.of("in", null, m, Type.int_t, 0, true);
            MetaPort.of("b", null, m, Type.int_t, 0, true);
            MetaPort.of("out", null, m, Type.int_t, 0, false);
            n = new Node(testGraph, m);
            n.getPort("in").setValue(42);
            n.getPort("b").setValue(1);
//            dump("Test after setValue", asObject(n));
            o = new Node(testGraph, m);
            o.getPort("in").setValue(-42);
            o.getPort("b").setValue(-1);
            o.getPort("out").connectTo(n.getPort("b"));
            dupGraph.populateFrom(testGraph);
        } catch(Throwable t) {
            t.printStackTrace(System.out);
            throw t;
        }
    }
    @Test
    public void testSomeMethod() {
        System.out.println("ViewTest testSomeMethod");
        annotate();
    }

    public void annotate(/*AnchorPane nodeEditor*/) {
//        testGraph.dump();
//        MetaNode.metaGraph.dump();
//        dupGraph.dump();
    }
    @Test
    public void fxstart() {
        launch(new String[]{});
    }

    @Override
    public void start(Stage primaryStage) {
        try {
//            Collectable.dump(asObject(m), "View");
//            Consumer<NodeEditorController> nec = controller -> controller.setGraph(v);
            GraphView.createNotifier = controller -> {
                var nv = controller.make(m);
                var fx = nv.getView();
                fx.setLayoutX(50);
                fx.setLayoutY(50);
                var nv2 = controller.make(m);
                var fx2 = nv2.getView();
                fx2.setLayoutX(250);
                fx2.setLayoutY(50);
                nv.getPort("out").connectTo(nv2.getPort("in"));
//                Collectable.dump(asObject(nv), "View2");
            };
            primaryStage.setTitle("Node Editor Test");
            primaryStage.setScene(new Scene(new FXMLLoader(GraphView.class.getResource("NodeEditor.fxml")).load(), 1000, 640));
            primaryStage.show();
//            final Graph<NodeView, PortView, ArcView, Graph> viewGraph
//                    = new Graph<>(NodeView.class, PortView.class, ArcView.class).setName("View");
//            viewGraph.populateFrom(testGraph);
        } catch(Error|Exception ex) {
            ex.printStackTrace(System.out);
            if(ex instanceof Error e)
                throw e;
        }
    }
    public final Graph<Node, Port, Arc, Graph> testGraph
            = new Graph<>(Node.class, Port.class, Arc.class);
    public final Graph<Node, Port, Arc, Graph> dupGraph
            = new Graph<>(Node.class, Port.class, Arc.class).setName("View");
    {
        testGraph.setName("testGraph");
        System.out.println("ViewTest static");
    }
}
