/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import com.aws.jag.fxnodeeditor.gengraph.Arc;
import static com.aws.jag.fxnodeeditor.view.GraphView.HOVER_PSEUDO_CLASS;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.shape.*;
import javafx.scene.transform.*;

public class ArcView extends Arc {
    private CubicCurve view;

    @SuppressWarnings("unused")
    public ArcView(PortView a, PortView b) {
        super(a, b);
        System.out.println("arc from " + a.getName() + " to " + b.getName());
    }
    @Override
    public GraphView getContext() {
        return (GraphView) super.getContext();
    }

    public CubicCurve getView() {
        var r = view;
        if(r == null) {
            System.out.println("createViz");
            view = r = new CubicCurve();
            var controller = getContext();
            r.hoverProperty().addListener(b -> {
                controller.hovered = view.isHover() ? this : null;
            });
            r.setOnDragOver(evt -> {
                if(DragAssist.createNode != null) {
                    evt.acceptTransferModes(TransferMode.ANY);
                    evt.consume();
                }
            });
            r.setOnDragDropped(evt -> {
                if(DragAssist.createNode != null) {
                    DragAssist.targetX = evt.getScreenX();
                    DragAssist.targetY = evt.getScreenY();
                    controller.hovered = this;
                    controller.make(DragAssist.createNode);
                }
                evt.setDropCompleted(true);
                evt.consume();
            });
            r.setOnDragEntered(evt -> view.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, true));
            r.setOnDragExited(evt -> view.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, false));
            Tooltip.install(r, new Tooltip(oneEnd().getName() + "->" + otherEnd().getName()));
            controller.getView().getChildren().add(r);
        }
        return r;
        // TODO Platform.runLater(() -> setViewText());
    }
    public void delete() {
        if(view != null) {
            getContext().getView().getChildren().remove(view);
            view = null;
        }
    }
    public void reposition(Transform area) {
        var curve = getView();
        if(oneEnd() instanceof PortView a && otherEnd() instanceof PortView b) {
            var out = a.getPosition(true, area);
            var in = b.getPosition(false, area);
//                var midX = (in.getX() + out.getX()) / 2;
            curve.setStartX(out.getX());
            curve.setStartY(out.getY());
//                curve.setControlX1(midX);
            curve.setControlX1(out.getX() + (a.metadata.in ? -100 : 100));
            curve.setControlY1(out.getY());
//                curve.setControlX2(midX);
            curve.setControlX2(in.getX() + (b.metadata.in ? -100 : 100));
            curve.setControlY2(in.getY());
            curve.setEndX(in.getX());
            curve.setEndY(in.getY());
        }
    }
}
