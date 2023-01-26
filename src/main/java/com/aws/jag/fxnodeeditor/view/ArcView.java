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
import javafx.scene.transform.*;

public class ArcView extends Arc {
    CubicCurve viz;

    @SuppressWarnings("unused")
    public ArcView(PortView a, PortView b) {
        super(a, b);
        System.out.println("arc from " + a.getName() + " to " + b.getName());
    }
    @Override
    public GraphView getContext() {
        return (GraphView) super.getContext();
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
    public void deleteViz() {
        if(viz!=null) {
            getContext().controller.nodeEditor.getChildren().remove(viz);
            viz = null;
        }
    }
    public void reposition(Transform area) {
        if(viz != null)
            if(oneEnd() instanceof PortView a && otherEnd() instanceof PortView b) {
                var out = a.getPosition(true, area);
                var in = b.getPosition(false, area);
                var midX = (in.getX() + out.getX()) / 2;
                viz.setStartX(out.getX());
                viz.setStartY(out.getY());
                viz.setControlX1(midX);
                viz.setControlY1(out.getY());
                viz.setControlX2(midX);
                viz.setControlY2(in.getY());
                viz.setEndX(in.getX());
                viz.setEndY(in.getY());
            }
    }
}
