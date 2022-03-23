/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.nighthacks.fxnodeeditor.graph;

import java.util.*;
import javafx.application.*;
import javafx.geometry.*;
import javafx.scene.*;
import javafx.scene.shape.*;

public class InArc extends ArcEndpoint {
    InArc(MNode.Port m, FGNode c, OutArc f) {
        super(m, c);
        comesFrom = f;
    }
    OutArc comesFrom;
    CubicCurve viz;
    Object value = "unknown";
    private boolean relayout;
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
        }
    }
    public void setIncoming(OutArc n) {
        if(n == comesFrom)
            return;
        if(n == null) {
            if(comesFrom != null)
                comesFrom.goesTo.remove(this);
            comesFrom = null;
            if(viz != null) {
                container.controller.nodeEditor.getChildren().remove(viz);
                viz = null;
            }
        } else {
            comesFrom = n;
            comesFrom.goesTo.add(this);
            relayout = true;
            System.out.println(n.meta.name + "->" + meta.name);
            if(viz == null) {
                viz = new CubicCurve();
                viz.hoverProperty().addListener(b -> {
                    container.controller.hovered = viz.isHover() ? InArc.this : null;
                });
                container.controller.nodeEditor.getChildren().add(viz);
//            viz.setFill(Color.TRANSPARENT);
//            viz.setStroke(Color.ORANGE);
//            viz.setStrokeWidth(2);
            }
        }
        Platform.runLater(() -> setView());
    }
    @Override
    public void setView(FGNode.PortView v) {
        super.setView(v);
        setView();
    }
    private void setView() {
        getView().setText(comesFrom == null
                ? meta.name + ": " + value
                : meta.name);
    }
    public void reposition() {
        if(viz != null)
            if(comesFrom == null)
                System.out.println("Unexpected NULL in " + meta.name);
            else {
                var out = getPosition(comesFrom.getView(), true);
                var in = getPosition(getView(), false);
                viz.setStartX(out.getX());
                viz.setStartY(out.getY());
                viz.setControlX1(out.getX() + 100);
                viz.setControlY1(out.getY());
                viz.setControlX2(in.getX() - 100);
                viz.setControlY2(in.getY());
                viz.setEndX(in.getX());
                viz.setEndY(in.getY());
                relayout = false;
            }
    }
    public Point2D getPosition(Node n, boolean right) {
        var lbl = n.getBoundsInLocal();
        var t = n.getLocalToSceneTransform();
        var ret = t.transform(lbl.getMinX(), lbl.getHeight() / 2);
        return right ? ret.add(lbl.getWidth(), 0) : ret;
    }
}
