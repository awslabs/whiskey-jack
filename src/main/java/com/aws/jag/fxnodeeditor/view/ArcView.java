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

public class ArcView extends Arc implements Selectable {
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
                if(view != null)
                    controller.setHovered(view.isHover() ? this : null);
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
                    controller.setHovered(this);
                    controller.make(DragAssist.createNode);
                }
                evt.setDropCompleted(true);
                evt.consume();
            });
            r.setOnDragEntered(evt -> {
                if(view != null)
                    view.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, true);
            });
            r.setOnDragExited(evt -> {
                if(view != null)
                    view.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, false);
            });
            Tooltip.install(r, new Tooltip(oneEnd().getName() + "->" + otherEnd().getName()));
            controller.getView().getChildren().add(r);
        }
        return r;
        // TODO Platform.runLater(() -> setViewText());
    }
    @Override
    public void delete() {
        super.delete();
        if(view != null) {
            getContext().getView().getChildren().remove(view);
            view = null;
        }
    }
    public void reposition(Transform area) {
        var curve = getView();
        if(oneEnd() instanceof PortView a0 && otherEnd() instanceof PortView b0) {
            PortView a, b;
            if(b0.isRightSide()) {
                a = a0;
                b = b0;
            } else {
                a = b0;
                b = a0;
            }
            var out = a.getPosition(area);
            var ox = out.getX();
            var oy = out.getY();
            var in = b.getPosition(area);
            var ix = in.getX();
            var iy = in.getY();
//                var midX = (in.getX() + out.getX()) / 2;
            var delta = ox<=ix ? Integer.min(100, ((int)(ix-ox))/3) : 100;
            curve.setStartX(ox);
            curve.setStartY(oy);
//                curve.setControlX1(midX);
            curve.setControlX1(ox + (a.metadata.isRightSide() ? -delta : delta));
            curve.setControlY1(oy);
//                curve.setControlX2(midX);
            curve.setControlX2(ix + (b.metadata.isRightSide() ? -delta : delta));
            curve.setControlY2(iy);
            curve.setEndX(ix);
            curve.setEndY(iy);
        }
    }
}
