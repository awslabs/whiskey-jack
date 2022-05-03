/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import com.nighthacks.fxnodeeditor.meta.*;
import java.util.*;
import javafx.application.*;
import javafx.css.*;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.shape.*;
import javafx.scene.transform.*;

public class InArc extends ArcEndpoint {
    InArc(Port m, FGNode c, OutArc f) {
        super(m, c);
        comesFrom = f;
    }
    public int vizorder; // for layout sorting
    public OutArc comesFrom;
    CubicCurve viz;
    Object value = "unknown";
    public void setValue(Object v) {
        if(v instanceof Optional ov) {
            if(ov.isEmpty())
                return;
            v = ov.get();
        }
        if(v == null)
            return;
        if(!Objects.equals(value, v)) {
            value = v;
            setIncoming(null);
            setViewText();
        }
    }
    public void setIncoming(OutArc n) {
        if(n == comesFrom)
            return;
        if(n == null) {
            if(comesFrom != null)
                comesFrom.connectsTo.remove(this);
            comesFrom = null;
            if(viz != null) {
                container.controller.nodeEditor.getChildren().remove(viz);
                viz = null;
            }
        } else {
            comesFrom = n;
            comesFrom.connectsTo.add(this);
            if(viz == null) {
                viz = new CubicCurve();
                viz.hoverProperty().addListener(b -> {
                    container.controller.hovered = viz.isHover() ? InArc.this : null;
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
                        container.controller.hovered = InArc.this;
                        container.controller.make(DragAssist.createNode);
                    }
                    evt.setDropCompleted(true);
                    evt.consume();
                });
                viz.setOnDragEntered(evt -> viz.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, true));
                viz.setOnDragExited(evt -> viz.pseudoClassStateChanged(HOVER_PSEUDO_CLASS, false));
                Tooltip.install(viz, new Tooltip(n.meta.name + "->" + meta.name));
                container.controller.nodeEditor.getChildren().add(viz);
            }
        }
        Platform.runLater(() -> setViewText());
    }
    @Override
    public void setView(FGNode.PortView v) {
        super.setView(v);
        setViewText();
    }
    @Override
    public void setViewText() {
        var v = getView();
        if(v != null)
            v.setText(comesFrom == null
                    ? meta.name + ": " + value
                    : meta.name);
    }
    private static final PseudoClass HOVER_PSEUDO_CLASS = PseudoClass.getPseudoClass("hover");
    public void reposition(Transform area) {
        if(viz != null)
            if(comesFrom == null)
                Dlg.error("Unexpected NULL in " + meta.name, null);
            else {
                var out = comesFrom.getPosition(true, area);
                var in = getPosition(false, area);
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
