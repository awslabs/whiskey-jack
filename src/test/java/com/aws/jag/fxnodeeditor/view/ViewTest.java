/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import java.io.*;
import java.util.function.*;
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
        System.out.print("ViewTest");
        m = new MetaNode()
                .setName("Add")
                .setDescription("add two things together")
                .setDomain(Domain.device);
        m.add(new MetaPort("in", null, m, Type.int_t, 0, true))
                .add(new MetaPort("b", null, m, Type.int_t, 0, true))
                .add(new MetaPort("out", null, m, Type.int_t, 0, false));
        n = new Node(testGraph, m);
        n.getPort("in").setValue(42);
        n.getPort("b").setValue(1);
        o = new Node(testGraph, m);
        o.getPort("in").setValue(-42);
        o.getPort("b").setValue(-1);
        o.getPort("out").connectTo(n.getPort("b"));
        dupGraph.populateFrom(testGraph);
    }
    @Test
    public void testSomeMethod() {
        System.out.println("ViewTest testSomeMethod");
        annotate();
    }

    public void annotate(/*AnchorPane nodeEditor*/) {
        testGraph.dump();
        MetaNode.metaGraph.dump();
        dupGraph.dump();
    }
    @Test
    public void fxstart() {
        launch(new String[]{});
    }

    @Override
    public void start(Stage primaryStage) {
        try {
            var v = new GraphView();
            var nv = v.newNode(m);
            var fx = nv.getView();
            fx.setLayoutX(50);
            fx.setLayoutY(50);
            System.out.println("");
            Consumer<NodeEditorController> nec = controller->controller.setGraph(v);
            NodeEditorController.createNotifier = nec;
            primaryStage.setTitle("Node Editor Test");
            primaryStage.setScene(new Scene(new FXMLLoader(NodeEditorController.class.getResource("NodeEditor.fxml")).load(), 1000, 640));
            primaryStage.show();
//            final Graph<NodeView, PortView, ArcView, Graph> viewGraph
//                    = new Graph<>(NodeView.class, PortView.class, ArcView.class).setName("View");
//            viewGraph.populateFrom(testGraph);
        } catch(IOException ex) {
            ex.printStackTrace(System.out);
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
