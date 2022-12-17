/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import javafx.css.*;

public class GraphView extends Graph<NodeView,PortView,ArcView,GraphView> {
    public final static PseudoClass HOVER_PSEUDO_CLASS = PseudoClass.getPseudoClass("hover");
    NodeEditorController controller;
    public GraphView() {
        super(NodeView.class, PortView.class, ArcView.class);
    }
    @Override
    public String getDescription() {
        return "A view on a graph";
    }
    @Override
    public String getName() {
        return "View";
    }
    public void adjustArcs() {
        //TODO implement
    }
}
