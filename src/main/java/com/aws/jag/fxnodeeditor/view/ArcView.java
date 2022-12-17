/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */

package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.*;
import com.aws.jag.fxnodeeditor.gengraph.Arc;
import static com.aws.jag.fxnodeeditor.view.GraphView.HOVER_PSEUDO_CLASS;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.shape.*;

class ArcView extends Arc {
    CubicCurve viz;

    @SuppressWarnings("unused")
    public ArcView(Port A, Port B) {
        super(A, B);
    }
    @Override
    public GraphView getContext() {
        return (GraphView)super.getContext();
    }
    
    public void createViz() {
            if(viz == null) {
                viz = new CubicCurve();
                var controller = ((GraphView) getContext()).controller;
                viz.hoverProperty().addListener(b -> {
                    controller.hovered = viz.isHover() ? this : null;
                });
                viz.setOnDragOver(evt -> {
                    if(DragAssist.createNode != null) {
                        evt.acceptTransferModes(TransferMode.ANY);
                        evt.consume();
                    }
                });
                viz.setOnDragDropped(evt -> {
                    if(DragAssist.createNode != null) {
                        DragAssist.targetX = evt.getScreenX();
                        DragAssist.targetY = evt.getScreenY();
                        controller.hovered = this;
                        controller.make(DragAssist.createNode);
                    }
                    evt.setDropCompleted(true);
                    evt.consume();
                });
                viz.setOnDragEntered(evt -> viz.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, true));
                viz.setOnDragExited(evt -> viz.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, false));
                Tooltip.install(viz, new Tooltip(oneEnd().getName() + "->" + otherEnd().getName()));
                controller.nodeEditor.getChildren().add(viz);
            }
            // TODO Platform.runLater(() -> setViewText());
        }
}
